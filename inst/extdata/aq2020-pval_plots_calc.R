## ---- aq2020_setup ----
require(aq2020)
require(dplyr) # for basic things like %>%
require(tidyr) # for pivot_longer
## require(readr) # read_csv
require(lubridate) # dates
## require(fs) # opening files
require(ggplot2) # for plotting
## require(gridExtra) # for complex layout
## require(grid)
require(ggpubr) # for complex layout
require(rlang) # for %||% in position_jitterdodgepres
## # helper functions
## get_fname <- function(path, ...) {
##   fname <- paste0(list(...), collapse = "_")
##   file.path(path, paste0(fname, ".csv"))
## }
source("position_jitterdodgepres.R")

# station/pollutant data
## no_wknd <- TRUE
## data_period <- "jan_2017_jun_2020"
## wknd_suffix <- ifelse(no_wknd, "no_wknd", "all_days")
## data_path <- path("data", "concentration", paste0("2017-2020_", wknd_suffix))
## pollutant_pval <- read_csv(get_fname("data", "conc-pval",
##                                      data_period, wknd_suffix))
## # temperature data
## temperature_pval <- read_csv(get_fname("data", "temp-pval",
##                                        data_period, wknd_suffix))
## # irradiance data
## irradiance_pval <- read_csv(get_fname("data", "irr-pval",
##                                       data_period, wknd_suffix))

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

#' Generate p-value boxplots.
#'
#' @param data Element `data` from list as output by [format_data()].
#' @param pval Element `pval` from list as output by [format_data()].
#' @param ylim Optional range of values at which to clip the plot.  Default is no clipping.
#' @param same_scale Whether to use the same scale for all plots.  This corresponds to the `fixed` vs `free_y` argument to [ggplot2::facet_wrap()].
#' @param nrow Number of rows when there are multiple stations to plot.  This corresponds to the `nrow` argument to [ggplot2::facet_wrap()].
#' @param pv_size Size of p-value labels.
#' @param pt_size Size of data points.
#' @param lab_size Size of axis labels.  Sets the arguments `axis.title` and `axis.text` in [ggplot2::theme()].
#' @param leg_size Size of legend text.  Sets the arguments `legend.title` and `legend.text` in [ggplot2::theme()].
#' @param main_size Size of station name in plot.
#' @param main_pos String for where to put the station name: "left" or "right".
#'
#' @return Boxplots with p-value labels of `Value` by `Period` within `Month`, tiled by `Station`.
pval_boxplot2 <- function(data, pval, ylim, same_scale = TRUE, nrow = NULL,
                          pv_size, pt_size, lab_size, leg_size, main_size,
                          main_pos = c("right", "left")) {
  if(missing(ylim)) ylim <- c(-Inf, Inf)
  main_pos <- match.arg(main_pos)
  ## main_pos <- ifelse("right", 1, -1)
  data %>%
    ggplot(aes(x = Month, y = pmin(Value, ylim[2]))) +
    ## geom_jitter(aes(fill = Year, group = Period, shape = Year),
    ##             position = position_jitterdodge(.5)) +
    geom_jitter(mapping = aes(fill = Year, group = Period, shape = Year),
                ## data = data %>% filter(Value < ylim[2]),
                position = position_jitterdodge(.5, seed = 1),
                size = pt_size, color = "transparent") +
    ## geom_jitter(mapping = aes(fill = Year, group = Period,
    ##                           shape = Year, y = ylim[2]),
    ##             data = data %>% filter(Value >= ylim[2]),
    ##             position = position_jitterdodge(.5, seed = 1),
    ##             size = pt_size, color = "transparent") +
    geom_text(data = data %>% filter(Value >= ylim[2]),
              mapping = aes(label = round(Value, 2), group = Period),
              check_overlap = TRUE, size = pv_size, vjust = 1.5,
              position = position_jitterdodge(.5, seed = 1)) +
    geom_boxplot(aes(color = Period),
                 ## position = position_dodge(preserve = "single"),
                 outlier.shape = NA, alpha = 0) +
    geom_text(data = pval,
              mapping = aes(x = Month, y = Yvalue, group = Period,
                            label = signif(round(Value, 2), 1)),
              position = position_dodge(.8),
              size = pv_size) +
    geom_text(data=pval,
              mapping = aes(label = Station, y = Inf,
                            x = ifelse(main_pos == "right", Inf, -Inf)),
              size = main_size,
              ## hjust = 1.1,
              hjust = ifelse(main_pos == "right", 1.1, -0.1),
              vjust = 2) +
    facet_wrap(~ Station, scales = ifelse(same_scale, "fixed", "free_y"),
               nrow = nrow) +
    scale_fill_manual(values = c("red", "blue", "orange", "darkgreen")) +
    ## scale_fill_manual(values = cb_palette) +
    ## scale_color_manual(values = c("black", "blue")) +
    scale_shape_manual(values = 21:25) +
    guides(fill = guide_legend(override.aes = list(size=2.5))) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      ## legend.position = c(0,1),
      legend.position = "top",
      legend.direction = "horizontal",
      ## legend.justification = c(-.02,1.1),
      legend.justification = c(0,.8),
      axis.title = element_text(size = lab_size),
      axis.text = element_text(size = lab_size),
      ## plot.margin = margin(t=30),
      ## plot.title = element_text(size = main_size,
      ##                           hjust = .97, vjust = -7),
      legend.text = element_text(size = leg_size),
      legend.title = element_text(size = leg_size),
      legend.spacing = unit(.2, "cm"),
      legend.margin = margin(2, 2, 2, 2),
      legend.key.size = unit(.5, "cm"),
      legend.background = element_rect(color = "black", size = .2),
      panel.border = element_rect(fill = NA, size = 1.5),
      axis.ticks = element_line(size = 1)
    )
}

# common plotting information
poll_label <- c(O3 = "O[3]*' Concentration '*(ppb)",
                PM25 = "PM[2.5]*' Concentration '*(mu*g/m^3)",
                NO2 = "NO[2]*' Concentration '*(ppb)",
                SO2 = "SO[2]*' Concentration '*(ppb)",
                CO = "CO*' Concentration '*(ppm)")
pv_size <- 2.7
pt_size <- 1.2
lab_size <- 10
leg_size <- 8
main_size <- 3


## ---- S8_concentration_co_jj ----
pollutant <- "CO"
station_names <- pollutant_info %>%
  filter(pollutant == !!pollutant & has_poll) %>%
  pull(station)
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))
pval_y <- .08
months <- 1:6

# prepare data
poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)
# plot
pval_boxplot2(data = poll_data$data %>% filter(Value < .6),
              pval = poll_data$pval,
              ## ylim = ylim,
              pv_size = pv_size, pt_size = pt_size,
              lab_size = lab_size, leg_size = leg_size,
              main_size = main_size) +
  ylab(ylab)

## ---- S8_concentration_co_jn ----
pollutant <- "CO"
station_names <- pollutant_info %>%
  filter(pollutant == !!pollutant & has_poll) %>%
  pull(station)
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))
pval_y <- .08
months <- 7:11

# prepare data
poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)
# plot
pval_boxplot2(data = poll_data$data %>% filter(Value < .6),
              pval = poll_data$pval,
              ## ylim = ylim,
              pv_size = pv_size, pt_size = pt_size,
              lab_size = lab_size, leg_size = leg_size,
              main_size = main_size) +
  ylab(ylab)


## ---- S7_concentration_no2_jj ----
pollutant <- "NO2"
station_names <- c("Grand_Bend", "Kitchener", "Toronto_West")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- c(-1, -1, 2)
## ylim <- c(-Inf, Inf)
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))
months <- 1:6

# prepare data
poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)
# plot
pval_boxplot2(data = poll_data$data, pval = poll_data$pval,
              pv_size = pv_size, pt_size = pt_size,
              lab_size = lab_size, leg_size = leg_size,
              main_size = main_size,
              same_scale = FALSE, nrow = 3) + ylab(ylab)


## ---- S7_concentration_no2_jn ----
pollutant <- "NO2"
station_names <- c("Grand_Bend", "Kitchener", "Toronto_West")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- c(-1, -1, 2)
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))
months <- 7:11

# prepare data
poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)
# plot
pval_boxplot2(data = poll_data$data, pval = poll_data$pval,
              pv_size = pv_size, pt_size = pt_size,
              lab_size = lab_size, leg_size = leg_size,
              main_size = main_size,
              same_scale = FALSE, nrow = 3) + ylab(ylab)


## ---- S9_concentration_o3_pm25_jj ----
pollutant <- "O3"
station_names <- c("Sarnia", "Toronto_West", "Windsor_West")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- 0
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))
months <- 1:6

# prepare data
poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)

plt_O3 <- pval_boxplot2(data = poll_data$data, pval = poll_data$pval,
                        pv_size = pv_size, pt_size = pt_size,
                        lab_size = lab_size, leg_size = leg_size,
                        main_size = main_size,
                        same_scale = TRUE, nrow = 3) +
  ylab(ylab) + ggtitle(expression(O[3]*" "*Concentration))

pollutant <- "PM25"
station_names <- c("Hamilton_West", "Ottawa_Downtown", "Windsor_Downtown")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))

poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)

plt_PM25 <- pval_boxplot2(data = poll_data$data, pval = poll_data$pval,
                          pv_size = pv_size, pt_size = pt_size,
                          lab_size = lab_size, leg_size = leg_size,
                          main_size = main_size,
                          same_scale = TRUE, nrow = 3) +
  ylab(ylab) + ggtitle(expression(PM[2.5]*" "*Concentration))

ggarrange(plt_O3, plt_PM25, ncol = 2, common.legend = TRUE)

## ---- S9_concentration_o3_pm25_jn ----
pollutant <- "O3"
station_names <- c("Sarnia", "Toronto_West", "Windsor_West")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- -1
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))
months <- 7:11

# prepare data
poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)

plt_O3 <- pval_boxplot2(data = poll_data$data, pval = poll_data$pval,
                        pv_size = pv_size, pt_size = pt_size,
                        lab_size = lab_size, leg_size = leg_size,
                        main_size = main_size,
                        same_scale = TRUE, nrow = 3) +
  ylab(ylab) + ggtitle(expression(O[3]*" "*Concentration))

pollutant <- "PM25"
station_names <- c("Hamilton_West", "Ottawa_Downtown", "Windsor_Downtown")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
ylab <- parse(text = paste0(poll_label[pollutant],
                            "*' -- Weekdays Only'"))

poll_data <- format_data(data = pollutant_data %>%
                           filter(Pollutant == pollutant),
                         pval = pollutant_pval %>%
                           filter(Pollutant == pollutant),
                         value = "Concentration",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)

plt_PM25 <- pval_boxplot2(data = poll_data$data, pval = poll_data$pval,
                          pv_size = pv_size, pt_size = pt_size,
                          lab_size = lab_size, leg_size = leg_size,
                          main_size = main_size,
                          same_scale = TRUE, nrow = 3) +
  ylab(ylab) + ggtitle(expression(PM[2.5]*" "*Concentration))

ggarrange(plt_O3, plt_PM25, ncol = 2, common.legend = TRUE)



## ---- S1_temperature_jj ----
station_names <- c("Grand_Bend_Goderich", "Kitchener_Waterloo",
                  "Toronto_West")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
station_names[1:2] <- c("Grand Bend", "Kitchener")
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- -22
## ylim <- c(-Inf, Inf)
ylab <- parse(text = paste0("'Daily Mean Temperature '*(degree*C)",
                            "*' -- Weekdays Only'"))
months <- 1:6

# prepare data
temp_data <- format_data(data = temperature_data,
                         pval = temperature_pval,
                         value = "Mean_Temp",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)

plt <- pval_boxplot2(data = temp_data$data, pval = temp_data$pval,
                     ## ylim = ylim,
                     pv_size = pv_size, pt_size = pt_size,
                     lab_size = lab_size, leg_size = leg_size,
                     main_size = main_size, main_pos = "left",
                     same_scale = FALSE, nrow = 3) + ylab(ylab)

## ggarrange(plt,
##           ggplot() + theme_void(),
##           ncol = 2, common.legend = TRUE)
plt

## ---- S1_temperature_jn ----
station_names <- c("Grand_Bend_Goderich", "Kitchener_Waterloo",
                  "Toronto_West")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
station_names[1:2] <- c("Grand Bend", "Kitchener")
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- -12
## ylim <- c(-Inf, Inf)
ylab <- parse(text = paste0("'Daily Mean Temperature '*(degree*C)",
                            "*' -- Weekdays Only'"))
months <- 7:11

# prepare data
temp_data <- format_data(data = temperature_data,
                         pval = temperature_pval,
                         value = "Mean_Temp",
                         station_names = station_names,
                         month = months,
                         pval_y = pval_y)

plt <- pval_boxplot2(data = temp_data$data, pval = temp_data$pval,
                     ## ylim = ylim,
                     pv_size = pv_size, pt_size = pt_size,
                     lab_size = lab_size, leg_size = leg_size,
                     main_size = main_size, main_pos = "left",
                     same_scale = FALSE, nrow = 3) + ylab(ylab)

## ggarrange(plt,
##           ggplot() + theme_void(),
##           ncol = 2, common.legend = TRUE)
plt


## ---- S2_irradiance_jj ----
station_names <- c("Ottawa", "Delhi")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- -300
## ylim <- c(-Inf, Inf)
ylab <- parse(text = paste0("'Daily Global Horizontal Irradiance '*(Wm^{-2}*day^{-1})",
                            "*' -- Weekdays Only'"))
months <- 1:6

irradiance_data2 <- bind_rows(
  irradiance_data,
  tibble(
    Date = ymd("2020-10-02", "2020-11-02", "2020-12-02"),
    Station = "Delhi",
    GHI = -1000
  )
)

# prepare data
irr_data <- format_data(data = irradiance_data2,
                        pval = irradiance_pval,
                        value = "GHI",
                        station_names = station_names,
                        month = months,
                        pval_y = pval_y)

pval_boxplot2(data = irr_data$data, pval = irr_data$pval,
              ## ylim = ylim,
              pv_size = pv_size, pt_size = pt_size,
              lab_size = lab_size, leg_size = leg_size,
              main_size = main_size, main_pos = "left",
              same_scale = FALSE, nrow = 3) + ylab(ylab)


## ---- S2_irradiance_jn ----
station_names <- c("Ottawa", "Delhi")
station_names <- setNames(gsub("_", replacement = " ", station_names),
                         nm = station_names)
## pv_size <- 2.7
## pt_size <- 1.2
## lab_size <- 10
## leg_size <- 8
## main_size <- 3
pval_y <- -300
## ylim <- c(-Inf, Inf)
ylab <- parse(text = paste0("'Daily Global Horizontal Irradiance '*(Wm^{-2}*day^{-1})",
                            "*' -- Weekdays Only'"))
months <- 7:11

# prepare data
irr_data <- format_data(data = irradiance_data2,
                        pval = irradiance_pval,
                        value = "GHI",
                        station_names = station_names,
                        month = months,
                        pval_y = pval_y)

pval_boxplot2(data = irr_data$data, pval = irr_data$pval,
              ## ylim = ylim,
              pv_size = pv_size, pt_size = pt_size,
              lab_size = lab_size, leg_size = leg_size,
              main_size = main_size, main_pos = "left",
              same_scale = FALSE, nrow = 3) +
  ylab(ylab) + coord_cartesian(ylim = c(-250, 8500))


## ---- scratch ----

## format_poll <- function(pollutant, station_names, pval_y) {
##   poll_data <- pollutant_data %>%
##     filter(Pollutant == pollutant & Station %in% names(station_names)) %>%
##     filter(month(Date) %in% 1:6) %>%
##     filter(!wday(Date, label = TRUE) %in% c("Sat", "Sun") | !no_wknd) %>%
##     mutate(Year = year(Date),
##            Month = month(Date, label = TRUE, abbr = TRUE),
##            Day = day(Date),
##            Station = station_names[Station]) %>%
##     select(Year, Month, Day, Station, Value = Concentration) %>%
##     mutate(Year = factor(Year),
##            Period = ifelse(Year == 2020, "2020", "2017-2019"))
##   pval_data <- pollutant_pval %>%
##     filter(Pollutant == pollutant & Station %in% names(station_names)) %>%
##     mutate(Value = Pval_med,
##            Month = month(as.numeric(ordered(Month)),
##                          label = TRUE, abbr = TRUE),
##            Station = station_names[Station])
##   if(length(pval_y) == 1) {
##     pval_data <- pval_data %>%
##       mutate(Yvalue = pval_y)
##   } else {
##     pval_data <- pval_data %>%
##       mutate(Yvalue = setNames(pval_y, station_names)[Station])
##   }
##   list(data = poll_data, pval = pval_data)
## }
## format_temp <- function(station_names, pval_y) {
##   temp_data <- temperature_data %>%
##     filter(Station %in% names(station_names)) %>%
##     filter(month(Date) %in% 1:6) %>%
##     filter(!wday(Date, label = TRUE) %in% c("Sat", "Sun") | !no_wknd) %>%
##     mutate(Year = year(Date),
##            Month = month(Date, label = TRUE, abbr = TRUE),
##            Day = day(Date),
##            Station = station_names[Station]) %>%
##     select(Year, Month, Day, Station, Value = Mean_Temp) %>%
##     mutate(Year = factor(Year),
##            Period = ifelse(Year == 2020, "2020", "2017-2019"))
##   pval_data <- temperature_pval %>%
##     filter(Station %in% names(station_names)) %>%
##     mutate(Value = Pval_med,
##            Month = month(as.numeric(ordered(Month)),
##                          label = TRUE, abbr = TRUE),
##            Station = station_names[Station])
##   if(length(pval_y) == 1) {
##     pval_data <- pval_data %>%
##       mutate(Yvalue = pval_y)
##   } else {
##     pval_data <- pval_data %>%
##       mutate(Yvalue = setNames(pval_y, station_names)[Station])
##   }
##   list(data = temp_data, pval = pval_data)
## }
## get_legend <- function(plt) {
##   tmp <- ggplot_gtable(ggplot_build(plt))
##   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
##   legend <- tmp$grobs[[leg]]
##   return(legend)
## }
## # same arguments as grid.arrange
## grid_arrange_shared_legend <- function(..., ylab) {
##     plots <- list(...)
##     g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
##     legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
##     lheight <- sum(legend$height)
##     grid.arrange(
##         legend,
##         do.call(arrangeGrob, lapply(plots, function(x)
##             x + theme(legend.position="none"))),
##         ncol = 1,
##       heights = unit.c(lheight, unit(1, "npc") - lheight),
##       left = ylab)
## }
