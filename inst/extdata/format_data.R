#' Format data for plotting.
#'
#' Converts the pairs [pollutant_data]/[pollutant_pval], [temperature_data]/[temperature_pval], or [irradiance_data]/[irradiance_pval] into the tibble format required for [pval_boxplot()].
#'
#' @param data Data frame of which a subset of columns is `Date`, `Station`, and the string stored in the parameter `value`.  See [pollutant_data], [temperature_data], and [irradiance_data].
#' @param pval Data frame of which a subset of columns is `Station`, `Period`, `Month`, and `Pval`.  See [pollutant_pval], [temperature_pval], and [irradiance_pval].
#' @param value String denoting the variable in `data` for which p-values are calculated.  This is `Concentration` for [pollutant_data], `Mean_Temp` for [temperature_data], and `GHI` for [irradiance_data].
#' @param station_names Named character vector of which the names are the elements of the `Station` variable to filter, and the elements are the display names for the plots.
#' @param month Numeric vector of months to filter.
#' @param pval_y Scalar or vector of the same length as `station_names` indicating the y-value in the plot at which to display the p-values.
#'
#' @return A list with elements:
#' \describe{
#'   \item{`data`}{A tibble with columns: `Station`, `Period`, `Year`, `Month`, `Day`, `Value`.}
#'   \item{`pval`}{A tibble with columns: `Station`, `Pollutant`, `Period`, `Month`, `Ndays`, `Median`, `Pval`, `Value`, `Yvalue`.}
#' }
#' @export
format_data <- function(data, pval, value, station_names, month, pval_y) {
  no_wknd <- TRUE # p-values have weekends excluded
  ## # FIXME: save p-values with numeric month to avoid this
  ## month_labels <- c("January", "February", "March", "April", "May", "June")
  data <- data %>%
    filter(Station %in% names(station_names)) %>%
    filter(lubridate::month(Date) %in% month) %>%
    filter(!wday(Date, label = TRUE) %in% c("Sat", "Sun") | !no_wknd) %>%
    mutate(Year = year(Date),
           Month = month(Date, label = TRUE, abbr = TRUE),
           Day = day(Date),
           Station = factor(station_names[Station],
                            levels = station_names)) %>%
    select(Year, Month, Day, Station, Value = !!value) %>%
    mutate(Year = factor(Year),
           Period = ifelse(Year == 2020, "2020", "2017-2019")) %>%
    select(Station, Period, Year, Month, Day, Value)
  pval <- pval %>%
    filter(Station %in% names(station_names)) %>%
    mutate(Value = Pval,
           ## Month = month(as.numeric(factor(Month, levels = month_labels)),
           ##               label = TRUE, abbr = TRUE),
           Month = lubridate::month(as.Date(paste0(Month, "0101"),
                                            format = "%b%d%y")),
           Station = factor(station_names[Station], levels = station_names)) %>%
    filter(Month %in% month) %>%
    mutate(Month = lubridate::month(Month, label = TRUE, abbr = TRUE))
  if(length(pval_y) == 1) {
    pval <- pval %>%
      mutate(Yvalue = pval_y)
  } else {
    pval <- pval %>%
      mutate(Yvalue = setNames(pval_y, station_names)[Station])
  }
  list(data = data, pval = pval)
}

