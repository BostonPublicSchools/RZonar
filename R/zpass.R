
## Internal utility functions to get all node values / attributes from an xml2 nodeset.
## Defined here for readability only, could have been an anonymous function in the
## body of zonar_get_zpass
getzpasstext <- function(xpath, dzp) {
  purrr::map_chr(dzp, function(x) xml2::xml_text(xml2::xml_find_first(x, xpath)))
}
getzpassattr <- function(xpath, dzp) {
  purrr::map_chr(dzp, function(x) xml2::xml_attr(x, xpath))
}

#' Get zonar zpass tapcard data
#'
#' Retrieves tap-card data for the specified time period from the Zonar API
#'
#' @export
#' @inheritParams zonar_get_schedules
#' @param convert If `TRUE` convert data types, otherwise keep returned values as character
#' @returns A `data.frame` containing all available tap card data.
#'
zonar_get_zpass <- function(start,
                            end,
                            timezone = "America/New_York",
                            convert = TRUE
                            ) {
  req <- zonar_req()

  if(is.character(start)) start <- lubridate::ymd_hms(start, tz = timezone)
  if(is.character(end)) end <- lubridate::ymd_hms(end, tz = timezone)

  ## There is a _lot_ of nesting, causes problems if the time span is too big
  ## Here we work around this by splitting the time span into days and processing
  ## each day in sequence
  if(difftime(end, start, units = "days") > 2) {
    timeseq <- zonar_cuttime(start = start, end = end, by = "1 days", timezone = timezone)
    ## note that we recursively call `zonar_get_zpass` here, but always with time span of one day or less
    ## we also override type conversion here since it is more efficient to do it all at once
    ## after all the data has been retrieved.
    dataZPass <- purrr::map2_dfr(
      timeseq$start, timeseq$end,
      function(x, y) zonar_get_zpass(x, y, timezone, convert = FALSE))
    dataZPass <- readr::type_convert(dataZPass)
    return(dataZPass)
  }

  ## fill out request parameters
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

  ## retrieve data from the api and extract the top-level nodeset
  dataZPass <- r %>%
    tryperformreq() %>%
    httr2::resp_body_xml() %>%
    xml2::xml_children()

  ## handle the case were no taps were logged during the specified time span
  if(length(dataZPass) < 1) return(NULL)

  ## Identify all attribute and node names. Could hard-code, but this way we get all available information
  attrnames <- unlist(purrr::map(dataZPass, function(x) names(xml2::xml_attrs(x))))
  attrnames <- sort(unique(attrnames))
  nodenames <- unlist(purrr::map(sample(dataZPass, ceiling(length(dataZPass)/20)),
                                 function(x) names(unlist(xml2::as_list(x)))))
  nodenames <- paste0("./", gsub(".", "/", sort(unique(nodenames)), fixed = TRUE))

  ## Iterate over the names and get all attributes and values.
  out <- c(purrr::map(attrnames, function(x) getzpassattr(x, dataZPass)),
           purrr::map(nodenames, function(x) getzpasstext(x, dataZPass)))
  ## Format the result and optionally convert column data types
  names(out) <- gsub("..", "", c(make.names(attrnames), make.names(nodenames)), fixed = TRUE)
  out <- tibble::as_tibble(out)
  out[out == ""] <- NA
  out <- dplyr::mutate(out, timestamp = lubridate::as_datetime(as.integer(timestamp), tz = !!timezone))
  if(convert) out <- readr::type_convert(out)
  out
}
