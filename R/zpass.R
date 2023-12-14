getzpasstext <- function(xpath, dzp) {
  purrr::map_chr(dzp, function(x) xml2::xml_text(xml2::xml_find_first(x, xpath)))
}
getzpassattr <- function(xpath, dzp) {
  purrr::map_chr(dzp, function(x) xml2::xml_attr(x, xpath))
}

#' Get zonar zpass tapcard data
#'
#' @export
#' @inheritParams zonar_get_schedules
#' @param convert If `TRUE` convert data types, otherwise keep returned values as character
#'
zonar_get_zpass <- function(start,
                            end,
                            timezone = "America/New_York",
                            convert = NULL
                            ) {
  req <- zonar_req()

  if(is.character(start)) start <- lubridate::ymd_hms(start, tz = timezone)
  if(is.character(end)) end <- lubridate::ymd_hms(end, tz = timezone)

  ## for whatever reason it is faster if we split it up by day.
  if(difftime(end, start, units = "days") > 2) {
    timeseq <- zonar_cuttime(start = start, end = end, by = "1 days", timezone = timezone)
    dataZPass <- purrr::map2_dfr(
      timeseq$start, timeseq$end,
      function(x, y) zonar_get_zpass(x, y, timezone, convert = FALSE))
    dataZPass <- readr::type_convert(dataZPass)
    return(dataZPass)
  }

  if(is.null(convert)) convert <- TRUE

  r <- httr2::req_url_query(
    req,
    action = "showevents",
    operation = "list",
    version = 2,
    start = as.integer(start),
    end = as.integer(end),
    format = "xml",
    reqtype = "dbid",
    logvers = 3
  )

  dataZPass <- r %>%
    tryperformreq() %>%
    httr2::resp_body_xml() %>%
    xml2::xml_children()

  if(length(dataZPass) < 1) return(NULL)

  attrnames <- unlist(purrr::map(dataZPass, function(x) names(xml2::xml_attrs(x))))
  attrnames <- sort(unique(attrnames))
  nodenames <- unlist(purrr::map(sample(dataZPass, ceiling(length(dataZPass)/20)),
                                 function(x) names(unlist(xml2::as_list(x)))))
  nodenames <- paste0("./", gsub(".", "/", sort(unique(nodenames)), fixed = TRUE))

  out <- c(purrr::map(attrnames, function(x) getzpassattr(x, dataZPass)),
           purrr::map(nodenames, function(x) getzpasstext(x, dataZPass)))
  names(out) <- gsub("..", "", c(make.names(attrnames), make.names(nodenames)), fixed = TRUE)
  out <- tibble::as_tibble(out)
  out[out == ""] <- NA
  out <- dplyr::mutate(out, timestamp = lubridate::as_datetime(as.integer(timestamp), tz = !!timezone))
  if(convert) out <- readr::type_convert(out)
  out
}
