#--- batch p-value calculations ------------------------------------------------

use_server <- FALSE
if(!use_server) {
  # can't install package on mfcf server because of rvest dependency
  require(aq2020)
} else {
  .libPaths("/u/mlysy/R/x86_64-pc-linux-gnu-library/3.6")
  source("../../R/fisher_pv.R")
  source("../../R/Tdiff.R")
  source("../../R/Tvar.R")
  source("../../R/Trange.R")
  load("../../data/pollutant_data.rda")
  load("../../data/temperature_data.rda")
  load("../../data/pollutant_info.rda")
}
require(tidyr)
require(dplyr)
require(lubridate)

# where to save pvalue calculations
data_path <- file.path("data", "temperature", "pvalue")

# helper function to create file names for saving data.
get_filename <- function(path, ..., ext = "rds") {
  fname <- paste0(c(...), collapse = "_")
  file.path(path, paste0(fname, ".", ext))
}

# Test statistics for two-group comparison: 2017-2019 vs 2020
Tbin <- function(value, group) {
  if(n_distinct(group) < 2) return(rep(NA, 4))
  c(Tdiff(value, group), Trange(value, group))
}

# Test statistics for multi-group comparisons: 2017 vs 2018 vs 2019
Tmulti <- function(value, group) {
  if(n_distinct(group) < 2) return(rep(NA, 4))
  c(Tvar(value, group), Trange(value, group))
}

# helper function for summarizing
set_tibble <- function(x, nm) {
  tibble(Value = setNames(x, NULL),
         Name = nm)
}

#' P-value calculation.
#'
#' @param station Station name.
#' @param months Vector of integers between 1 and 12.
#' @param no_wknd Logical, whether to exclude weekends.
#' @param nsim Integer number of simulations.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{`Month`}{Month of the year.}
#'   \item{`Value`}{Value of the computed statistic.}
#'   \item{`Name`}{Name of the computed statistic.  These are: `{Tobs/pval}_{mean/med}_{old/new}`, `stat_{mean/med}`, and `n`.}
#'   \item{`Period`}{Either "2017-2019" or "2020".}
#'   \item{`Station`}{Station name.}
#' }
pval_calc <- function(station, months, no_wknd = TRUE, nsim) {
  message("Station: ", station)
  # prepare data for p-values
  pval_data <- temperature_data %>%
    filter(Station == station) %>%
    mutate(Year = year(Date),
           Month = month(Date, label = TRUE, abbr = FALSE),
           Day = day(Date)) %>%
    filter(Month %in% month(months, label = TRUE, abbr = FALSE)) %>%
    filter(!is.na(Mean_Temp))
  if(no_wknd) {
    pval_data <- pval_data %>%
      mutate(Weekday = wday(Date, label = TRUE)) %>%
      filter(!Weekday %in% c("Sat", "Sun")) %>%
      select(-Weekday)
  }
  # pvalue calculations
  stat_names <- c("Tobs_mean_old", "Tobs_med_old",
                  "Tobs_mean_new", "Tobs_med_new",
                  "pval_mean_old", "pval_med_old",
                  "pval_mean_new", "pval_med_new",
                  "stat_mean", "stat_med", "n")
  tm <- system.time({
    # 2017-2019
    pv_ref <- pval_data %>%
      filter(Year != 2020) %>%
      group_by(Month) %>%
      summarize(set_tibble(
        x = c(fisher_pv(group = Year,
                        value = Mean_Temp,
                        nsim = nsim,
                        Tfun = Tmulti),
              mean(Mean_Temp, na.rm = TRUE),
              median(Mean_Temp, na.rm = TRUE),
              sum(!is.na(Mean_Temp))),
        nm = stat_names),
        .groups = "drop") %>%
      mutate(Period = "2017-2019")
    # 2020
    pv_2020 <- pval_data %>%
      mutate(Year = ifelse(Year == 2020, "2020", "2017-2019")) %>%
      group_by(Month) %>%
      summarize(set_tibble(
        x = c(fisher_pv(group = Year,
                        value = Mean_Temp,
                        nsim = nsim,
                        Tfun = Tbin),
              mean(Mean_Temp[Year == "2020"], na.rm = TRUE),
              median(Mean_Temp[Year == "2020"], na.rm = TRUE),
              sum(!is.na(Mean_Temp[Year == "2020"]))),
        nm = stat_names),
        .groups = "drop") %>%
      mutate(Period = "2020")
    pv_data <- bind_rows(pv_ref, pv_2020)
  })
  message("Time: ", round(tm[3], 1), " seconds")
  # append station and pollutant information
  pv_data <- pv_data %>%
    mutate(Station = station)
  pv_data
}


# test
pv <- pval_calc(station = "Grand_Bend_Goderich",
                months = 12, no_wknd = TRUE, nsim = 100)
pv
pv %>% pivot_wider(names_from = "Name", values_from = "Value")

require(parallel)
ncores <- detectCores(logical = FALSE) # number of cores to use
RNGkind("L'Ecuyer-CMRG") # parallel processing seed
cl <- makeCluster(spec = ncores)

nsim <- 1e4 # number of random permutations
months <- 1:12 # months for which to calculate p-values
no_wknd <- TRUE # exclude weekends
data_path <- file.path("data", "temperature", "pvalue") # where to save data
# which stations to look at
station_list <- unique(temperature_data$Station)
job_ind <- 1:length(station_list)

# cluster setup
# load packages
if(!use_server) {
  invisible(
    clusterEvalQ(cl, {
      require(aq2020)
    })
  )
} else {
  invisible(
    clusterEvalQ(cl, {
      .libPaths("/u/mlysy/R/x86_64-pc-linux-gnu-library/3.6")
    })
  )
}
invisible(
  clusterEvalQ(cl, {
    require(tidyr)
    require(dplyr)
    require(lubridate)
  })
)
# send workspace to each worker
clusterExport(cl, varlist = ls()[ls() != "cl"])

system.time({
  bad_ind <- parLapply(cl, job_ind, fun = function(ii) {
    station <- station_list[ii]
    pv_data <- tryCatch(
      pval_calc(station = station,
                months = months, no_wknd = no_wknd, nsim = nsim),
      error = function(e) NULL
    )
    if(!is.null(pv_data)) {
      saveRDS(pv_data,
              file = get_filename(path = data_path,
                                  station))
    }
    is.null(pv_data)
  })
})

# kill cluster
stopCluster(cl)

#--- check if there were any issues --------------------------------------------

bad_ind <- lapply(job_ind, function(ii) {
  station <- station_list[ii]
  pv_data <- tryCatch(
    readRDS(file = get_filename(path = data_path,
                                station)),
    error = function(e) NULL
  )
  is.null(pv_data)
})

any(unlist(bad_ind))


#--- combine datasets ----------------------------------------------------------

#' The dataset will contain the following columns:
#'
#' - Station: station name.
#' - Period: 2017-2019 or 2020.
#' - Month: name of the month.
#' - `Ndays`: number of days in the given Month and Period.
#' - Median concentration over all days in the given Month and Period.
#' - `Pval`: p-value of the randomization test.

temperature_pval <- lapply(job_ind, function(ii) {
  station <- station_list[ii]
  pv_data <- readRDS(file = get_filename(path = data_path,
                                         station))
  pv_data %>%
    pivot_wider(names_from = "Name", values_from = "Value") %>%
    select(Station, Period, Month,
           Ndays = n, Median = stat_med, Pval = pval_med_new)
}) %>% bind_rows()


#--- check boxplot -------------------------------------------------------------

poll_label <- c(O3 = "O[3]*' Concentration '*(ppb)",
                PM25 = "PM[2.5]*' Concentration '*(mu*g/m^3)",
                NO2 = "NO[2]*' Concentration '*(ppb)",
                SO2 = "SO[2]*' Concentration '*(ppb)",
                CO = "CO*' Concentration '*(ppm)")

poll_data %>%
  mutate(Year = factor(Year),
         Period = ifelse(Year == 2020, "2020", "2017-2019")) %>%
  ggplot(aes(x = Month, y = Concentration)) +
  geom_jitter(aes(fill = Year, group = Period),
              pch = 21, position = position_jitterdodge(.5),
              size = 2) +
  geom_boxplot(aes(color = Period), outlier.shape = NA, alpha = 0) +
  geom_text(mapping = aes(x = Month, group = Period,
                          label = signif(pval*100, 2), y = 0),
            data = pv_data, position = position_dodge(.8),
            size = 4) +
  scale_fill_manual(values = c("red", "blue", "orange", "darkgreen")) +
  ylab(parse(text = poll_label[pollutant])) +
  ggtitle(paste0("Station: ", station)) +
  multiplot::theme_min2()
