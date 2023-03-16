#' Get Zonar asset idle/stop events for all assets
#'
#' @description `zonar_get_idle_events` retrieves zonar asset (bus) idle event
#' information for all assets.
#'
#' @inheritParams zonar_get_schedules
#' @param assets Character vector of asset ids to retrieve. If `NULL` (the
#' default) retrieve all assets.
#' @param zones Character vector of zone **names** (not IDs!) to retrieve. If `NULL` (the default)
#' retrieve all zones.
#' @export
#' @examplesIf Sys.getenv("ZONAR_CUSTOMER")!=""
#'
#' assettest <- zonar_get_idle_events(
#' start = "2022-11-30 08:00:00",
#' end = "2022-11-30 08:30:00",)
#' dplyr::glimpse(assettest)
#'
zonar_get_idle_events <- function(start, end, zones = NULL, assets = NULL, timezone = "America/New_York") {
  if(is.character(start)) start <- lubridate::ymd_hms(start, tz = timezone)
  if(is.character(end)) end <- lubridate::ymd_hms(end, tz = timezone)
  req <- zonar_req()  %>%
    httr2::req_url_query(
      action="showposition", operation="idlestopallstop",
      format = "xml", fromdate = as.integer(start), todate = as.integer(end))
  if(is.null(assets) && is.null(zones)) {
    timeseq <- zonar_cuttime(
      lubridate::floor_date(start, unit = "hour"),
      lubridate::ceiling_date(end, unit = "hour"),
      by = "12 hours",
      timezone = timezone)
    timeseq[] <- lapply(timeseq, as.integer)
    req <- purrr::map2(timeseq$start, timeseq$end,
                       function(x, y) httr2::req_url_query(req, fromdate = x, todate = y))
  } else if(!is.null(assets) && is.null(zones)) {
    req <- lapply(assets, function(x) httr2::req_url_query(req, target = x, reqtype="dbid"))
  } else if(is.null(assets) && !is.null(zones)) {
    req <- lapply(zones, function(x) httr2::req_url_query(req, loilocation = x, loiinclude="include"))
  } else if (!is.na(assets) && !is.null(zones)) {
    grid <- tidyr::expand_grid(assets = assets, zones = zones)
    req <- purrr::map2(
      grid$assets, grid$zones,
      function(x, y) httr2::req_url_query(req, target = x, reqtype="dbid", loilocation = x, loiinclude="include"))
  }
  out <- purrr::map_dfr(
    req,
    memoise::memoise(process_idle_events, cache = .zonarCache),
    timezone = timezone)
  if(nrow(out) < 1) return(out)
  out <- dplyr::filter(
    out,
    .data$event.from >= start & .data$event.from <= end)
  out
}

process_idle_events <- function(idleRequest, timezone="America/New_York") {
  dataidle <- idleRequest %>%
    tryperformreq() %>%
    httr2::resp_body_xml() %>%
    xml2::xml_children()
  ## sometimes there really are none...
  if(length(dataidle) < 1) {
    query <- unlist(httr2::url_parse(idleRequest$url)$query)
    query <- query[setdiff(names(query), c("customer", "username", "password"))]
    message("Zero rows found for query:\n  ", paste(names(query), query, sep = "=", collapse = "\n  "))
    outnames <- c("tag", "fleet", "type", "id", "exsid", "event.from", "event.to",
                  "event.length", "lat", "long")
    out <- as.data.frame(as.list(rep(NA, length(outnames))))[0, , drop = FALSE]
    names(out) <- outnames
    return(out)
  }
  values <- dplyr::bind_rows(
    purrr::map(
      dataidle,
      function(x) {
        out <- c(xml2::xml_attrs(x), unlist(xml2::as_list(x)))
        }))
  values <- dplyr::mutate(
    values,
    event.from = lubridate::as_datetime(as.integer(.data$event.from), tz = timezone),
    event.to = lubridate::as_datetime(as.integer(.data$event.to), tz = timezone))
  tidyr::separate(values, col = "event.latlong", into = c("lat", "long"), sep = ",")
}

