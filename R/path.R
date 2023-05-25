get_paths <- function(x, timezone) {
  readr::read_csv(
    httr2::resp_body_string(tryperformreq(x)),
    locale = readr::locale(tz = timezone),
    col_types = "ncDtnccnnnccc")
}

#' Get zonar schedule data for a specific zone
#'
#' @export
#' @param assets Character vector of Zonar asset ids to retrieve. If `NULL`
#' (the default) retrieve paths for all assets.
#' @inheritParams zonar_get_schedules
#' @examplesIf Sys.getenv("ZONAR_CUSTOMER")!=""
#' library(RZonar)
#'
#' pathtest_all <- zonar_get_paths(
#'   start = "2022-11-30 08:10:00",
#'   end = "2022-11-30 08:40:00")
#' dplyr::glimpse(pathtest_all)

#' pathtest_586 <- zonar_get_paths(
#'   start = "2022-11-30 08:10:00",
#'   end = "2022-11-30 08:40:00",
#'   assets = "586")
#' dplyr::glimpse(pathtest_586)
#'
zonar_get_paths <- function(start,
                            end,
                            assets = NULL,
                            timezone = "America/New_York"
) {
  req <- zonar_req()

  if(is.character(start)) start <- lubridate::ymd_hms(start, tz = timezone)
  if(is.character(end)) end <- lubridate::ymd_hms(end, tz = timezone)

  r <- httr2::req_url_query(
    req,
    action = "showposition",
    operation = "path",
    version = 2,
    starttime = as.integer(start),
    endtime = as.integer(end),
    format = "csv",
    #target = assets,
    reqtype = "dbid",
    logvers = 3.8
  )
  if(is.null(assets) || isTRUE(assets == "all")) {
    timeseq <- zonar_cuttime(
      lubridate::floor_date(start, unit = "hour"),
      lubridate::ceiling_date(end, unit = "hour"),
      by = "1 hour",
      timezone = timezone)
    timeseq[] <- lapply(timeseq, as.integer)
    r <- purrr::map2(timeseq$start, timeseq$end,
              function(x, y) httr2::req_url_query(r, starttime = x, endtime = y))
  } else {
    r <- lapply(assets, function(x) httr2::req_url_query(r, target = x))
  }
  out <- purrr::map_dfr(
    r,
    memoise::memoise(get_paths, cache = .zonarCache),
    timezone = timezone)
  if(nrow(out) < 1) stop("No Zonar path data for assets ", stringr::str_c(assets, collapse = ";"), "at ", stringr::str_c(as.character(start), str_c(as.character(end))))
  DSTstring  <- ifelse(lubridate::dst(start), "Time(EDT)", "Time(EST)")
  out <- dplyr::mutate(
    out,
    datetime = lubridate::ymd_hms(paste(.data$Date, .data[[!!DSTstring]]), tz = timezone))
  out <- dplyr::filter(
    out,
     .data$datetime >= start & .data$datetime <= end)
  out
}
