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
source("format_data.R")
source("pval_boxplot.R")
## source("position_jitterdodgepres.R")

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
pval_boxplot(data = poll_data$data %>% filter(Value < .6),
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
pval_boxplot(data = poll_data$data %>% filter(Value < .6),
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
pval_boxplot(data = poll_data$data, pval = poll_data$pval,
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
pval_boxplot(data = poll_data$data, pval = poll_data$pval,
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

plt_O3 <- pval_boxplot(data = poll_data$data, pval = poll_data$pval,
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

plt_PM25 <- pval_boxplot(data = poll_data$data, pval = poll_data$pval,
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

plt_O3 <- pval_boxplot(data = poll_data$data, pval = poll_data$pval,
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

plt_PM25 <- pval_boxplot(data = poll_data$data, pval = poll_data$pval,
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

plt <- pval_boxplot(data = temp_data$data, pval = temp_data$pval,
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

plt <- pval_boxplot(data = temp_data$data, pval = temp_data$pval,
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

pval_boxplot(data = irr_data$data, pval = irr_data$pval,
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

pval_boxplot(data = irr_data$data, pval = irr_data$pval,
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
