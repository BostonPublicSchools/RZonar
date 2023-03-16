#' Get Zonar Zone / Location-of-Interest data
#'
#' @description `zonar_get_zones` retrieves zonar "locations of interest" AKA zones
#'
#' @param zone Optional zone name to retrieve. If `NULL` (the default) retrieve all zones.
#'
#' @export
zonar_get_zones <- function(zone = NULL) {
  ## First retrieve the Zone/Location of Interest data from Zonar
  reqLOI <- zonar_req()  %>%
    httr2::req_url_query(action="editposition", operation="list", format = "csv")
  if(!is.null(zone)) reqLOI <- httr2::req_url_query(reqLOI, target = zone)
  # reqLOI %>% req_dry_run()
  respLOI <- reqLOI %>%
    tryperformreq()
  out <- respLOI %>%
    httr2::resp_body_string() %>%
    readr::read_csv(
      col_names = c("name", "geometry", "category", "id"),
      col_types = c("ccci"))
  if(!is.null(zone)) out <- dplyr::filter(out, name == zone)
  out$type <- ifelse(
    is.na(out$category) | out$category %in% c(
      "Dead End", "Playing Fields", "Service Shops Off Site",
      "Transdev/BPS Bus Yards", "WATCH", "School Lot", "Zonar Yard"),
    "other", "school")
  out
}

#' Get Zonar Zone / Location-of-Interest data
#'
#' @description `zonar_rename_zones` renames zonar "locations of interest" AKA zones
#'
#' @param old Character vector of existing zone names
#' @param new Character vector of replacement names
#'
#' @export
zonar_rename_zones <- function(old, new) {
  if(length(old) != length(new)) stop("`old` and `new` must be the same length")
  out <- purrr::map2(old, new, .f = zonar_rename_zone)
  return(invisible(out))
}

zonar_rename_zone <- function(old, new) {
  if(length(old) != 1 | length(new) != 1) stop("`old` and `new` must both be character vectors of length one.")
  req <- zonar_req()
  z <- zonar_get_zones(old)
  req <- httr2::req_url_query(req,
                              action="editposition",
                              operation="edit",
                              target=old,
                              name=new,
                              reqtype = "name",
                              geometry = z$geometry,
                              logvers = 2,
                              format = "xml")
  resp <- tryperformreq(req)
  out <- httr2::resp_body_xml(resp)
  out <- xml2::as_list(out)
  code <- out[["success"]]
  if(is.null(code)) stop(out)
  code
}
