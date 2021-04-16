#' Pollutant information from the AQO website.
#'
#' A dataset listing the monitoring stations and pollutants from the Air Quality Ontario website.
#'
#' @format A data frame with 180 rows and columns:
#' \describe{
#'   \item{station}{The name of the monitoring station.}
#'   \item{station_id}{The ID of the monitoring station.}
#'   \item{pollutant}{The type of air pollutant: `O3`, `PM25`, `NO2`, `CO`, or `SO2`.}
#'   \item{poll_id}{The ID of the air pollutant.}
#'   \item{has_poll}{Whether the AIQ website has any records for the given station/pollutant combination.}
#' }
#'
#' @source \url{http://www.airqualityontario.com/}.
"pollutant_info"

#' Pollutant data from the AIQ website.
#'
#' A dataset containing pollutant concentrations between 2017-2020 at the station/pollutant combinations in [pollutant_info].
#'
#' @format A data frame with 176,039 rows and columns:
#' \describe{
#'   \item{Date}{The date at which the concentration was measured.}
#'   \item{Station}{The name of the monitoring station.}
#'   \item{Pollutant}{The name of the pollutant being measured: `O3` (ppb), `PM25` (micrograms / m^3), `NO2` (ppb), `SO2` (ppb), `CO` (ppm).}
#'   \item{Concentration}{The average daily concentration from hourly measurements.}
#' }
#' @source \url{http://www.airqualityontario.com/}.
"pollutant_data"

#' Pollutant p-values.
#'
#' A dataset containing randomization test p-values for the pollutant concentrations contained in [pollutant_data].
#'
#' @format A data frame with 2,943 rows and columns:
#' \describe{
#'   \item{Station}{The name of the monitoring station.}
#'   \item{Pollutant}{The name of the pollutant being measured: `O3`, `PM25`, `NO2`, `SO2`, `CO`.}
#'   \item{Period}{2017-2019 or 2020.}
#'   \item{Month}{Name of the month.}
#'   \item{Ndays}{Number of days from which p-value is computed.}
#'   \item{Median}{Median daily concentration in the given month.}
#'   \item{Pval}{Randomization test p-value in the given month.}
#' }
#'
#' @details For a detailed explanation of the p-value calculation and the fields in this dataset, please see `vignettes("aq2020")`.
"pollutant_pval"
