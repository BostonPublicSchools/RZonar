## Description: Retrieves Zonar asset arrival time at each zone               ##
##      Author: Ista Zahn                                                     ##
##        Date: 11/17/2022                                                    ##
##     Contact: izahn@bostonpublicschools.org                                 ##
##                                                                            ##
##        Note: The schedule API wrapper is kind of a mess, mostly because    ##
##              the API it wraps is a mess. It could use some cleanup, e.g.   ##
##              there is a fair bit of duplicated code, but it will probably  ##
#               always be a bit of a mess.                                    ##

#' Get zonar schedule data for all routes
#'
#' @description Use `zonar_get_schedules` to retrieve the time each bus
#' (asset) reaches a route anchor (zone) from the
#' [Zonar API](https://support.zonarsystems.net/hc/en-us/categories/360000020371-API-Portal)
#'
#' @details The Zonar
#' [schedule report](https://support.zonarsystems.net/hc/en-us/articles/360018372852-GPS-Schedule-Report)
#' API can give us that information, but not all at once. Using this R wrapper
#' makes it easy to retrieve bulk schedule reports from the Zonar API in a
#' convenient format that can easily be saved to a database or spreadsheet.
#'
#' Note that retrieving data for short intervals (one day or less) will generally
#' be faster when setting `by = "time"`, while bulk retrieval of weeks or months
#' of data will be faster when setting `by = "zone"`. By default `zonar_get_shedules_all`
#' will try to pick the best setting for you depending on the `start` and `end`
#' parameters you give it.
#'
#' The default is to retrieve all zones and all assets for the time period specified
#' by `start` and `end`. You can retrieve a specified list of zones or assets
#' via the `zones` and `assets` arguments. However, due to the way the Zonar API
#' works you cannot specify both. Note that `zones` must be specified by name, but
#' `assets` must be specified by ID; see [zonar_get_assets()] if you need to look
#' up IDs from asset names.
#'
#' @export
#' @param start Character vector of length one giving the start time in
#' yyyy-mm-dd hh:mm:ss format.
#' @param end Character vector of length one giving the end time in
#' yyyy-mm-dd hh:mm:ss format.
#' @param by Whether to split requests by time or zone under the hood. By default
#' a reasonable value is chosen based on the amount of data you request (i.e., the
#' size of the interval between `start` and `end`.
#' @param zones Character vector of zone **names** (not IDs!) to retrieve. If `NULL` (the default)
#' retrieve all zones.
#' @param assets Integer vector of giving the Zonar asset **IDs** (not names) of the asset
#' to retrieve.
#' @param timezone Defaults to "America/New_York"
#' @param reformat If `TRUE` cleanup and re-format the data such that each row
#' represents a "Zone event", i.e., a discrete entry into and/or exit from a
#' zone. Otherwise leave the date in the original format returned by Zonar.
#' Defaults to `FALSE`.
#' @param include_geometry If `TRUE` also return the geographic polygon
#' showing the location and extent of each Zone. Defaults to `FALSE`.
#' @param cachedir Ignored, always uses .zonarCache now.
#' @param omit_categories Character vector containing zone categories to skip/omit.
#' @param test Set to `TRUE` for testing purposes, otherwise it will be too slow.
#' @describeIn zonar_get_schedules Get zonar schedule data for all routes
#' @examplesIf Sys.getenv("ZONAR_CUSTOMER")!=""
#'
#' library(RZonar)
#' ## use by="time" for short duration, e.g. < 1 day
#' schedtest1 <- zonar_get_schedules(
#'   start = "2022-11-30 08:00:00",
#'   end = "2022-11-30 08:40:00",
#'   by = "time")
#' dplyr::glimpse(schedtest1)
#'
#' ## use by="zone" for bulk retrieval (e.g. weeks or months of data)
#' schedtest2 <- zonar_get_schedules(
#'   start = "2022-11-20 08:00:00",
#'   end = "2022-11-28 08:30:00",
#'   by = "zone",
#'   zones = c("Adams", "Burke") # not required, leave out for all zones
#'   )
#' dplyr::glimpse(schedtest2)
#'
#' ## use by="zone" for bulk retrieval (e.g. weeks or months of data)
#' schedtest3 <- zonar_get_schedules(
#'   start = "2022-11-20 08:00:00",
#'   end = "2022-11-28 08:30:00",
#'   assets = c("1024", "1030") # not required, leave out for all assets
#'   )
#' dplyr::glimpse(schedtest3)
#'
zonar_get_schedules <- function(start,
                                end,
                                by = c("time", "zone", "asset"),
                                zones = NULL,
                                assets = NULL,
                                timezone = "America/New_York",
                                reformat = FALSE,
                                include_geometry = FALSE,
                                cachedir = NULL,
                                omit_categories = NULL,
                                test = FALSE) {

  if(all(by == c("time", "zone", "asset"))) {
    by <- ifelse(difftime(lubridate::ymd_hms(end, tz = timezone), lubridate::ymd_hms(start, tz = timezone), units = "days") > 1, "zone", "time")
  }

  dataLOI <- zonar_get_zones()
  dataLOI <- dplyr::rename(dataLOI, Zone = "name", zoneID = "id")
  if(!include_geometry) dataLOI <- dplyr::select(dataLOI, "zoneID", "Zone", "category")

  if(!is.null(zones) && is.null(assets)) {
    by <- "zone"
    dataLOI <- dplyr::filter(dataLOI, .data$Zone %in% zones)
  }
  if(!is.null(omit_categories)) dataLOI <- dplyr::filter(dataLOI, !.data$category %in% omit_categories)
  if(test) {
    dataLOI <- dataLOI[1:5,]
    message("NOTE: only pulling schedule data for the first five zones; omit 'test' parameter if you want everything")
  }

  if(!is.null(assets) && is.null(zones)) {
    by <- "asset"
  }

  if(!is.null(assets) && !is.null(zones)) {
    stop("Only one of `zones` or `assets` can be specified. This is due to the way the Zonar API works, sorry for the inconvience.")
  }

  if(by == "time") {
    ## Set up time periods to iterate over. We need to do this because the API
    ## limits us to retrieving 15 minutes at a time
    timeseq <- zonar_cuttime(start = start, end = end, by = "15 min", timezone = timezone)
    timeseq[] <- lapply(timeseq, as.integer)

    ## Retrieve schedule info
    dataSched <- purrr::map2_dfr(
      timeseq$start, timeseq$end,
      memoise::memoise(zonar_get_schedules_for_time, cache = .zonarCache),
      timezone = timezone)
  } else if( by == "zone") {
    start <- as.integer(lubridate::ymd_hms(start, tz = timezone))
    end <- as.integer(lubridate::ymd_hms(end, tz = timezone))

    ## Retrieve schedule info
    dataSched <- purrr::map_dfr(dataLOI$zoneID, memoise::memoise(zonar_get_schedules_for_zone, cache = .zonarCache),
                                start = start, end = end, timezone = timezone)
  } else if(by == "asset") {
    dataSched <- purrr::map_dfr(assets, memoise::memoise(zonar_get_schedules_for_asset, cache = .zonarCache),
                                start = start, end = end, timezone = timezone)
  } else {
    stop("`by` must be either 'zone', or 'time'")
  }
  if(! "Asset ID" %in% names(dataSched) && "Asset" %in% names(dataSched)) {
    dataAssets <- zonar_get_assets()
    dataSched$Asset <- gsub("^'|'$", "", dataSched$Asset)
    dataSched <- dplyr::right_join(
      dplyr::select(dataAssets, "Asset", "Asset ID" = "assetID"),
      dataSched,
      by = "Asset")
  }
  if(! "Asset" %in% names(dataSched) && "Asset ID" %in% names(dataSched)) {
    if(!exists("dataAssets")) dataAssets <- zonar_get_assets()
    dataSched <- dplyr::right_join(
      dplyr::select(dataAssets, "Asset", "Asset ID" = "assetID"),
      dataSched,
      by = "Asset ID")
  }
  if(! "zoneID" %in% names(dataSched) && "Zone" %in% names(dataSched)) {
    dataSched$Zone <- gsub("^'|'$", "", dataSched$Zone)
    dataSched <- dplyr::right_join(
      dataLOI,
      dataSched,
      by = "Zone")
  }
  if(! "Zone" %in% names(dataSched) && "zoneID" %in% names(dataSched)) {
    dataSched <- dplyr::right_join(
      dataLOI,
      dataSched,
      by = "zoneID")
  }

  class(dataSched) <- c("zonarsched", class(dataSched))

  if(reformat) dataSched <- zonar_cleanup(dataSched)

  return(dataSched)
}


#' Get zonar schedule data for a given 15 minute time interval
#'
#' @description Usually `zonar_get_schedules_for_time` and `zonar_get_schedules_for_zone`
#' will not be called directly, consider using `zonar_get_schedules` instead.
#'
#' @noRd
#' @examplesIf Sys.getenv("ZONAR_CUSTOMER")!=""
#'
#' schedtest <- zonar_get_schedules_for_time(
#'   start = "2022-11-30 08:00:00", # the interval between start and end must be < 15
#'   end = "2022-11-30 08:14:00"  # minutes, otherwise use zonar_get_schedules
#'   )
#' dplyr::glimpse(schedtest)
#'
zonar_get_schedules_for_time <- function(start,
                                         end,
                                         timezone = "America/New_York",
                                         cachedir = NULL) {
  req <- zonar_req()

  if(is.character(start)) start <- as.integer(lubridate::ymd_hms(start, tz = timezone))
  if(is.character(end)) end <- as.integer(lubridate::ymd_hms(end, tz = timezone))

  r <- httr2::req_url_query(
    req,
    action = "showposition",
    operation = "schedule",
    starttime = start,
    endtime = end,
    format = "csv",
    logvers = "2"
  )
  out <- readr::read_csv(
    httr2::resp_body_string(tryperformreq(r)),
    skip = 3,
    col_types = readr::cols(Time = "T", .default = "c"),
    locale = readr::locale(tz = timezone),
    show_col_types = FALSE)

  out <- dplyr::mutate(out, Zone = gsub("^'|'$", "", .data$Zone))
  out
}

#' Get zonar schedule data for a specific zone
#'
#' @noRd
#' @param zoneid Integer vector of length one giving the Zonar id of the zone
#' to retrieve.
#' @examplesIf Sys.getenv("ZONAR_CUSTOMER")!=""
#'
#' schedtest <- zonar_get_schedules_for_zone(
#'   zoneid = 291,
#'   start = "2022-11-30 08:00:00",
#'   end = "2022-11-30 09:00:00")
#' dplyr::glimpse(schedtest)
#'
zonar_get_schedules_for_zone <- function(zoneid,
                                         start,
                                         end,
                                         timezone = "America/New_York",
                                         cachedir = NULL) {
  req <- zonar_req()

  if(is.character(start)) start <- as.integer(lubridate::ymd_hms(start, tz = timezone))
  if(is.character(end)) end <- as.integer(lubridate::ymd_hms(end, tz = timezone))

  r <- httr2::req_url_query(
    req,
    action = "showposition",
    operation = "schedule",
    starttime = start,
    endtime = end,
    format = "csv",
    reporttype = 2,
    target = zoneid,
    reqtype = "dbid"
  )
  out <- readr::read_csv(
    httr2::resp_body_string(tryperformreq(r)),
    skip = 5,
    col_types = readr::cols(Time = "T", .default = "c"),
    locale = readr::locale(tz = timezone),
    show_col_types = FALSE)
  dplyr::mutate(out,zoneID = zoneid)
}

#' Get zonar schedule data for a specific asset
#'
#' @noRd
#' @param assetid Integer vector of length one giving the Zonar id of the asset
#' to retrieve.
#' @examplesIf Sys.getenv("ZONAR_CUSTOMER")!=""
#'
#' schedtest <- zonar_get_schedules_for_asset(
#'   assetid = 291,
#'   start = "2022-11-30 08:00:00",
#'   end = "2022-11-30 09:00:00")
#' dplyr::glimpse(schedtest)
#'
zonar_get_schedules_for_asset <- function(assetid,
                                          start,
                                          end,
                                          timezone = "America/New_York",
                                          cachedir = NULL) {
  req <- zonar_req()

  if(is.character(start)) start <- as.integer(lubridate::ymd_hms(start, tz = timezone))
  if(is.character(end)) end <- as.integer(lubridate::ymd_hms(end, tz = timezone))

  r <- httr2::req_url_query(
    req,
    action = "showposition",
    operation = "schedule",
    starttime = start,
    endtime = end,
    format = "csv",
    reporttype = 1,
    target = assetid,
    reqtype = "dbid"
  )
  out <- readr::read_csv(
    httr2::resp_body_string(tryperformreq(r)),
    skip = 5,
    col_types = readr::cols(Time = "T", .default = "c"),
    locale = readr::locale(tz = timezone),
    show_col_types = FALSE)
  dplyr::mutate(out,`Asset ID` = assetid)
}
