#--- batch p-value calculations ------------------------------------------------

require(aq2020)
require(tidyr)
require(dplyr)
require(lubridate)

# where to save pvalue calculations
data_path <- file.path("data", "pollutant", "pvalue")

# helper function to create file names for saving data.
get_filename <- function(path, ..., ext = "rds") {
  fname <- paste0(c(...), collapse = "_")
  file.path(path, paste0(fname, ".", ext))
}

# Test statistics for two-group comparison: 2017-2019 vs 2020
Tbin <- function(value, group) {
  c(Tdiff(value, group), Trange(value, group))
}

# Test statistics for multi-group comparisons: 2017 vs 2018 vs 2019
Tmulti <- function(value, group) {
  c(Tvar(value, group), Trange(value, group))
}

# helper function for summarizing
set_tibble <- function(x, nm) {
  tibble(Value = setNames(x, NULL),
         Name = nm)
}

## source("covid-functions.R")

## statpoll <- read_csv("aqo-statpoll_info.csv") %>%
##   filter(has_poll)

## get_fname <- function(path, station, pollutant) {
##   fname <- paste0(station, "_", pollutant, "_Jan_2017_Jun_2020.csv")
##   file.path(path, fname)
## }


#` P-value calculation.
#`
#` @param station Station name.
#` @param pollutant Pollutant name.
#` @param months Vector of integers between 1 and 12.
#` @param no_wknd Logical, whether to exclude weekends.
#` @param nsim Integer number of simulations.
#`
#` @return A tibble with columns:
pval_calc <- function(station, pollutant, months, no_wknd = TRUE, nsim) {
  message("Station: ", station, ", Pollutant: ", pollutant)
  # prepare data for p-values
  poll_data <- pollutant_data %>%
    filter(Station == station & Pollutant == pollutant) %>%
    mutate(Year = year(Date),
           Month = month(Date, label = TRUE, abbr = FALSE),
           Day = day(Date)) %>%
    filter(Month %in% month(1:6, label = TRUE, abbr = FALSE))
  if(no_wknd) {
    poll_data <- poll_data %>%
      mutate(Weekday = wday(Date, label = TRUE)) %>%
      filter(!Weekday %in% c("Sat", "Sun")) %>%
      select(-Weekday)
  }
  ## poll_data <- read_csv(get_fname(path, station, pollutant))
  ## # prepare for p-values
  ## poll_data <- poll_data %>%
  ##   mutate(Year = year(Date),
  ##        Month = month(Date, label = TRUE, abbr = FALSE),
  ##        Day = day(Date)) %>%
  ## filter(Month %in% month(1:6, label = TRUE, abbr = FALSE)) %>%
  ## pivot_longer(cols = H01:H24, names_to = "Hour",
  ##              values_to = "Concentration") %>%
  ## group_by(Year, Month, Day) %>%
  ## summarize(Concentration = mean(Concentration, na.rm = TRUE),
  ##           .groups = "drop")
  # pvalue calculations
  stat_names <- c("Tobs_mean_old", "Tobs_med_old",
                  "Tobs_mean_new", "Tobs_med_new",
                  "pval_mean_old", "pval_med_old",
                  "pval_mean_new", "pval_med_new",
                  "n")
  tm <- system.time({
    pv_data <- bind_rows(
      # 2017-2019
      poll_data %>%
      filter(Year != 2020) %>%
      group_by(Month) %>%
      summarize(set_tibble(
        x = c(fisher_pv(group = Year,
                        value = Concentration,
                        nsim = nsim,
                        Tfun = Tmulti), n()),
        nm = stat_names),
        .groups = "drop") %>%
      mutate(Period = "2017-2019"),
      # 2020
      poll_data %>%
      mutate(Year = ifelse(Year == 2020, "2020", "2017-2019")) %>%
      group_by(Month) %>%
      summarize(set_tibble(
        x = c(fisher_pv(group = Year,
                        value = Concentration,
                        nsim = nsim,
                        Tfun = Tbin), sum(Year == "2020")),
        nm = stat_names),
        .groups = "drop") %>%
      mutate(Period = "2020")
    )
  })
  message("Time: ", round(tm[3], 1), " seconds")
  # append station and pollutant information
  pv_data <- pv_data %>%
    mutate(Station = station,
           Pollutant = pollutant)
  pv_data
}

## # test
## pv <- pval_calc(station = "Kitchener", pollutant = "NO2",
##                 months = 1:6, no_wknd = TRUE, nsim = 1e3)

require(parallel)
ncores <- detectCores(logical = FALSE) # number of cores to use
RNGkind("L'Ecuyer-CMRG") # parallel processing seed
cl <- makeCluster(spec = ncores)

nsim <- 1e3 # number of random permutations
months <- 1:12 # months for which to calculate p-values
no_wknd <- TRUE # exclude weekends
data_path <- file.path("data", "pollutant", "pvalue") # where to save data

# cluster setup
# load packages
invisible(
  clusterEvalQ(cl, {
    require(aq2020)
    require(tidyr)
    require(dplyr)
    require(lubridate)
  })
)
# send workspace to each worker
clusterExport(cl, varlist = ls()[ls() != "cl"])

system.time({
  bad_ind <- parLapply(cl, 1:10, fun = function(ii) {
    station <- pollutant_info$station[ii]
    pollutant <- pollutant_info$pollutant[ii]
    pv_data <- tryCatch(
      pval_calc(station = station, pollutant = pollutant,
                months = months, no_wknd = no_wknd, nsim = nsim),
      error = function(e) NULL
    )
    if(!is.null(pv_data)) {
      saveRDS(pv_data,
              file = get_filename(path = data_path,
                                  station, pollutant))
    }
    is.null(pv_data)
  })
})

# kill cluster
stopCluster(cl)


nsim <- 1e4
no_wknd <- TRUE
data_path <- file.path("data", "2017-2020")
if(no_wknd) data_path <- paste0(data_path, "_no_wknd")

clusterExport(cl, varlist = ls()[ls() != "cl"])
invisible(
  clusterEvalQ(cl, expr = {
    source("covid-functions.R")
  })
)

system.time({
  bad_ind <- parLapply(cl, 1:nrow(statpoll), fun = function(ii) {
    pv_data <- tryCatch(pval_calc(ii, path = data_path),
                        error = function(e) NULL)
    if(!is.null(pv_data)) {
      write_csv(pv_data,
                path = get_fname(file.path(data_path, "pval"),
                                 station = statpoll$station[ii],
                                 pollutant = statpoll$pollutant[ii]))
    }
    is.null(pv_data)
  })
})

# kill cluster
stopCluster(cl)

#--- combine datasets ----------------------------------------------------------

source("covid-functions.R")
statpoll <- read_csv("aqo-statpoll_info.csv") %>%
  filter(has_poll)
get_fname <- function(path, ...) {
  fname <- paste0(list(...), collapse = "_")
  file.path(path, paste0(fname, ".csv"))
}

data_path <- file.path("data", "concentration", "2017-2020")
data_period <- "jan_2017_jun_2020"
no_wknd <- TRUE
wknd_suffix <- ifelse(no_wknd, "no_wknd", "all_days")
data_path <- paste0(data_path, "_", wknd_suffix)

pval_data <- lapply(1:nrow(statpoll), function(ii) {
  read_csv(get_fname(file.path(data_path, "pval"),
                     statpoll$station[ii],
                     statpoll$pollutant[ii],
                     data_period))
})

# save data
pval_data %>% bind_rows() %>%
  pivot_wider(names_from = "Stat",
              names_prefix = "Pval_", values_from = "Pval") %>%
  select(Station, Pollutant, Period, Month, Pval_mean, Pval_med) %>%
  write_csv(path = get_fname("data", "conc-pval", data_period, wknd_suffix))

# save data
## bind_rows(pval_data) %>%
##   select(Station, Pollutant, Period, Month, Pvalue = pval) %>%
##   write_csv(path = "pvalue_data_jan_2017_jun_2020.csv")

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
