#' Get Zonar asset (bus) data
#'
#' @description `zonar_get_assets` retrieves zonar asset (bus) information from
#' the Zonar API
#'
#' @export
#' @param assets Character vector of asset ids to retrieve. If `NULL` (the default)
#' retrieve all assets. If not `NULL`, a character vector of the must be the
#' Zonar *Asset IDs* to retrieve.
#' @examplesIf Sys.getenv("ZONAR_CUSTOMER")!=""
#'
#' assettestall <- zonar_get_assets()
#' dplyr::glimpse(assettestall)
#'
#' assettest <- zonar_get_assets(assets = c("586", "1046"))
#' dplyr::glimpse(assettest)
#'
zonar_get_assets <- function(assets=NULL) {
  reqAsset <- zonar_req()
  reqAsset <- httr2::req_url_query(
    reqAsset,
    action="showopen", operation="showassets", format = "xml", logvers = 3)
  if(is.null(assets)) {
    reqAsset <- list(reqAsset)
  } else {reqAsset <- lapply(assets, function(x) httr2::req_url_query(reqAsset, target = x, reqtype="dbid"))
  }
  purrr::map_dfr(reqAsset, process_assets)
}

#' Get Zonar gps units
#'
#' @describeIn zonar_get_assets Get gps info
#' @export
#'
zonar_get_gps_units <- function() {
  reqAsset <- zonar_req()
  reqAsset <- httr2::req_url_query(
    reqAsset,
    action="showopen", operation="showgps", format = "xml")
  process_assets(reqAsset)
}

#' Get Zonar asset usage
#'
#' @describeIn zonar_get_assets Get asset usage
#' @export
#'
zonar_get_asset_usage <- function() {
  reqAsset <- zonar_req()
  reqAsset <- httr2::req_url_query(
    reqAsset,
    action="showopen", operation="showusage", format = "xml", reqtype = "dbid",
    starttime = as.integer(Sys.time()), endtime = as.integer(Sys.time()))
  process_assets(reqAsset)
}

process_assets <- function(assetRequest) {
  dataAsset <- assetRequest %>%
    tryperformreq() %>%
    httr2::resp_body_xml() %>%
    xml2::xml_children()
  ids <- sapply(dataAsset, xml2::xml_attr, attr="id")
  values <- dplyr::bind_rows(lapply(dataAsset, function(x) unlist(xml2::as_list(x))))
  values[["assetID"]] <- ids
  values <- dplyr::mutate(
    values,
    Asset = {
      x <- purrr::map(stringr::str_split(.data$fleet, "\\W+"), stringr::str_subset, pattern = "[A-z]")
      x0 <- purrr::map_lgl(x, ~ length(.x) == 0)
      x[x0] <- list("")
      stringr::str_to_upper(purrr::map_chr(x, stringr::str_c, collapse = " / "))
    }
  )
  values <- dplyr::relocate(values, "assetID", "Asset")
  values
}
