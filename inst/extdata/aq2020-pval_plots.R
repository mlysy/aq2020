#' ---
#' title: "P-value Plots for AQ2020 Paper"
#' params:
#'   crop: FALSE
#' output:
#'   rmarkdown::pdf_document:
#'     keep_tex: yes
#'   rmarkdown::html_document:
#'     theme: readable
#'     highlight: tango
#'     toc: yes
#'     toc_float: true
#' ---

#+ r setup, include = FALSE, message = FALSE
knitr::opts_chunk$set(echo = FALSE, fig.align = "center")
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
knitr::read_chunk("aq2020-pval_plots_calc.R")

#+ aq2020_setup, include = FALSE


#' # CO Concentrations: January-June

#+ S8_concentration_co_jj, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 6.5, crop = params$crop


#' \newpage
#'
#' # CO Concentrations: July-November

#+ S8_concentration_co_jn, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 6.5, crop = params$crop


#' \newpage
#'
#' # NO2 Concentrations: January-June

#+ S7_concentration_no2_jj, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 10, crop = params$crop


#' \newpage
#'
#' # NO2 Concentrations: July-November

#+ S7_concentration_no2_jn, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 10, crop = params$crop


#' \newpage
#'
#' # O3 and PM2.5 Concentrations: January-June

#+ S9_concentration_o3_pm25_jj, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 9.75, crop = params$crop


#' \newpage
#'
#' # O3 and PM2.5 Concentrations: July-November

#+ S9_concentration_o3_pm25_jn, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 9.75, crop = params$crop


#' \newpage
#'
#' # Temperature: January-June

#+ S1_temperature_jj, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 10, crop = params$crop


#' \newpage
#'
#' # Temperature: July-November

#+ S1_temperature_jn, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 10, crop = params$crop


#' \newpage
#'
#' # Solar Irradiance: January-June

#+ S2_irradiance_jj, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 7.5, crop = params$crop


#' \newpage
#'
#' # Solar Irradiance: July-November

#+ S2_irradiance_jn, message = FALSE, warning = FALSE, fig.width = 8, fig.height = 7.5, crop = params$crop
