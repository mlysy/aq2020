#' Query a full year station/pollutant table from the AQO website.
#'
#' @param station_id Recording station code (integer).
#' @param poll_id Pollutant code (integer).
#' @param year Year (between 2007 and 2017).
#' @param start_month Optional starting month (integer, default = 1).
#' @param end_month Optional ending month (integer, default = 12).
#' @param fill Whether to fill tables with missing entries with `NA`.
#'
#' @return A `365 x 26` table of pollutant concentrations, where each row is a day and the columns are `Station` (which is `station_id`) `Date` (Y-m-d), and hourly time in `H01:H23` format.
#'
#' @details Times are always in EST, i.e., on a clock which does not change back and forth from DST.
#' @export
get_aqoyr <- function(station_id, poll_id, year,
                      start_month, end_month, fill = FALSE) {
  if(missing(start_month)) start_month <- 1
  if(missing(end_month)) end_month <- 12
  # generic website components
  aqo_link <- c("http://www.airqualityontario.com/history/index.php?s=",
                "&y=",
                "&p=",
                "&m=",
                "&e=",
                "&t=html&submitter=Search&i=1")
  # specific table link
  link <- paste0(aqo_link,
                 c(station_id, year, poll_id, start_month, end_month, ""),
                 collapse = "")
  rvest::read_html(link) %>%
    rvest::html_node("table.resourceTable") %>%
    rvest::html_table(fill = fill)
}
