tryperformreq <- function(req) {
  out <- try(httr2::req_perform(req))
  if(class(out)[[1]] == "try-error") {
    Sys.sleep(5)
    out <- try(httr2::req_perform(req))
  }
  if(class(out)[[1]] == "try-error") {
    Sys.sleep(30)
    out <- try(httr2::req_perform(req))
  }
  if(class(out)[[1]] == "try-error") {
    Sys.sleep(30)
    out <- httr2::req_perform(httr2::req_options(req, ssl_verifypeer = 0))
  }
  out
}


#' Retrieve zonar API credentials
#'
#' @description
#' RZonar requires the Zonar company, username, and password to be set as
#' environment variables. This function checks to make sure these are available
#' and gives instructions for setting them if they are not.
#'
#' @param customer Zonar customer id, retrieved from ZONAR_CUSTOMER environment
#' variable by default but can be overridden here.
#' @param user Zonar username, retrieved from ZONAR_USER environment
#' variable by default but can be overridden here.
#' @param pass Zonar password, retrieved from ZONAR_CUSTOMER environment
#' variable by default but can be overridden here.
#' @export
zonar_credentials <- function(customer, user, pass) {

  ## Retrieve ZONAR_CUSTOMER, ZONAR_USER and ZONAR_PASS from environment variables
  if(missing(customer)) customer <- Sys.getenv("ZONAR_CUSTOMER")
  if(missing(user)) user <- Sys.getenv("ZONAR_USER")
  if(missing(pass)) pass <- Sys.getenv("ZONAR_PASS")
  credentials <- c(customer = customer, user = user, pass = pass)

  ok <- all(nchar(credentials) > 0)
  if(!ok) stop(
  "RZonar uses environment variables ZONAR_CUSTOMER, ZONAR_USER
  and ZONAR_PASS for login info. You must set these before starting R, usually
  in .Renviron")
  ## check curl config while we're at it
  if(
    Sys.getenv("CURL_SSL_BACKEND") != "openssl" &&
    Sys.info()["sysname"] == "Windows") stop(
      "You must set CURL_SSL_BACKEND as in
  https://cran.r-project.org/web/packages/curl/vignettes/windows.html")

  return(credentials)
}

#' Create base Zonar API request
#'
#' @description Create basic request, can be reused.
#'
#' @param ... credential overrides, passed to `zonar_credentials`. Usually leave
#' this empty and set login info as environment variables.
#' @export
zonar_req <- function(...) {
  creds <- zonar_credentials(...)
  req <- httr2::request("https://omi.zonarsystems.net/interface.php")
  req <- httr2::req_url_query(
    req,
    customer=creds["customer"],
    username=creds["user"],
    password=creds["pass"])
  ## TODO: maybe set up some checking here to make sure it works before continuing
  return(req)
}

#' Cleanup data from the Zonar API
#'
#' @description The Zonar API sometimes returns badly formatted data. Use
#' `zonar_cleanup` to tidy it up.
#'
#' @param data Data returned by one of the `zonar_get_*` functions.
#' @export
zonar_cleanup <- function(data) {
  UseMethod("zonar_cleanup")
}

#' Cleanup Zonar schedule data
#'
#' @description Use `zonar_cleanup` to augment and format the sometimes messy
#' data returned by the zonar API.
#'
#' @details Assets can be recorded as repeatedly going in and out of a zone in short
#' periods of time, try to clean that up. Another problem is that assets can pass
#' through zones incidentally. `zonar_cleanup` tries to adress this by grouping
#' repeated gps zone events that occur in a short time, while keeping separate
#' pass-throughs separate.
#'
#' @describeIn zonar_cleanup Cleanup Zonar schedule data
#' @importFrom rlang .data
#' @export
zonar_cleanup.zonarsched <- function(data) {
  ################# Cleanup Zonar schedule data ########################
  data <- dplyr::filter(data, !is.na(.data$Time) & !is.na(.data$`IN/OUT`))
  data <- dplyr::group_by(data, .data$Zone, .data$Asset, .data$`IN/OUT`)
  data <- dplyr::mutate(data, tgroup = 1:dplyr::n())
  data <- data[, setdiff(names(data), c("Time In Zone", "Duration", "Duration Total", "Distance (Miles)"))]
  data <- tidyr::pivot_wider(data, names_from = "IN/OUT", values_from = "Time")
  data <- dplyr::rename(data, time_in = "IN", time_out = "OUT")
  dplyr::mutate(dplyr::ungroup(data), duration = .data$time_out - .data$time_in)
}
# zonar_cleanup.zonarsched <- function(data) {
#   ################# Cleanup Zonar schedule data ########################
#   data <- dplyr::group_by(data, .data$Zone, .data$Asset)
#   if("Asset ID" %in% names(data)) {
#     data <- dplyr::group_by(data, .data$`Asset ID`, .add = TRUE)
#   }
#   if("geometry" %in% names(data)) {
#     data <- dplyr::group_by(data, .data$geometry, .add = TRUE)
#   }
#   data %>%
#     ## these heroics are probably not worth it, but not sure what else we can do
#     dplyr::arrange(.data$Asset, .data$Zone, .data$Time) %>%
#     dplyr::mutate(tgroup = cumsum(ifelse(`IN/OUT` %in% "OUT", 0, c(0, difftime(.data$Time, dplyr::lag(.data$Time), units = "mins")[-1])) > 40)) %>%
#     dplyr::group_by(.data$tgroup, .add = TRUE) %>%
#     dplyr::summarize(
#       type = c("time_in", "time_out"),
#       Time = c(min(Time[`IN/OUT` %in% "IN"], na.rm = TRUE),
#                max(Time[`IN/OUT` %in% "OUT"], na.rm = TRUE))) %>%
#     dplyr::ungroup() %>%
#     tidyr::pivot_wider(names_from = "type", values_from = "Time") %>%
#     dplyr::mutate(duration = .data$time_out - .data$time_in)
# }


zonar_cuttime <- function(start, end, by, timezone="America/New_York") {
  if(is.character(start) && is.character(end)) {
    start <- lubridate::ymd_hms(start, tz = timezone)
    end <- lubridate::ymd_hms(end, tz = timezone)
  }
  timeseq <- seq(
    start,
    end,
    by = by)
  if(length(timeseq) > 1) {
    timeseq <- data.frame(
      start = timeseq[-length(timeseq)],
      end = timeseq[-1] - 1)
  } else {
    timeseq <- data.frame(start = start, end = end)
  }
  timeseq
}


strCollapse <- function(x, collapse = "; ") str_c(sort(as.character(na.omit(unique(x)))), collapse = collapse)


