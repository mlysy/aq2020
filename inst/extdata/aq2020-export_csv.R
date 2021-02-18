#--- things to save as csv -----------------------------------------------------

require(aq2020)
require(tidyr)
require(dplyr)
require(lubridate)
require(readr)

# helper function to create file names for saving data.
get_filename <- function(path, ..., ext = "rds") {
  fname <- paste0(c(...), collapse = "_")
  file.path(path, paste0(fname, ".", ext))
}


#--- pollutant p-values --------------------------------------------------------

# where to save pvalue calculations
data_path <- file.path("data", "pollutant", "pvalue")

# also, add the pollutants from the previous analysis
pval_old <- read_csv(get_filename(path = file.path("data", "old"),
                                  "conc-pval_jan_2017_jun_2020_no_wknd",
                                  ext = "csv"),
                     col_types = cols()) %>%
  mutate(Month = factor(Month,
                        levels = month(1:12, label = TRUE, abbr = FALSE),
                        ordered = TRUE)) %>%
  select(Station, Pollutant, Period, Month, Pval_old = Pval_med)

# merge
pval_new <- left_join(pollutant_pval, pval_old)

## # save
## write.csv(pval_new,
##           file = get_filename(path = data_path,
##                               "pollutant_pval", ext = "csv"))

# keep only those where they differ
pval_same <- pval_new %>%
  mutate(Pval_same = (Pval < .05 & Pval_old < .05) |
           (Pval >= .05 & Pval_old >= .05)) %>%
  pull(Pval_same)

pval_diff <- pval_new[which(!is.na(pval_same) & !pval_same),]

## # save
## write.csv(pval_diff,
##           file = get_filename(path = data_path,
##                               "pollutant_pval_diff", ext = "csv"))

#--- pollutant medians ---------------------------------------------------------

poll_med <- pollutant_data %>%
  mutate(Weekday = wday(Date, label = TRUE),
         Month = month(Date, label = TRUE, abbr = FALSE),
         Year = year(Date)) %>%
  filter(!Weekday %in% c("Sat", "Sun")) %>%
  select(-Weekday) %>%
  filter(!is.na(Concentration)) %>%
  group_by(Station, Pollutant, Year, Month) %>%
  summarize(Median = median(Concentration), .groups = "drop")

# quick check
full_join(
  poll_med %>%
  filter(Year == 2020) %>%
  select(Station, Pollutant, Year, Month, Median2 = Median),
  pollutant_pval %>%
  filter(Period == 2020) %>%
  mutate(Year = as.numeric(Period)) %>%
  select(Station, Pollutant, Year, Month, Median)
) %>%
  mutate(med_diff = Median - Median2) %>%
  print(n = Inf)

## # save
## write.csv(poll_med,
##           file = get_filename(path = file.path("data", "pollutant"),
##                               "pollutant_median", ext = "csv"))

#--- temperature pvalues -------------------------------------------------------

# where to save pvalue calculations
data_path <- file.path("data", "temperature", "pvalue")

# also, add the pvalues from the previous analysis
pval_old <- read_csv(get_filename(path = file.path("data", "old"),
                                  "temp-pval_jan_2017_jun_2020_no_wknd",
                                  ext = "csv"),
                     col_types = cols()) %>%
  mutate(Month = factor(Month,
                        levels = month(1:12, label = TRUE, abbr = FALSE),
                        ordered = TRUE)) %>%
  select(Station, Period, Month, Pval_old = Pval_med)

# merge
pval_new <- left_join(temperature_pval, pval_old)

# keep only those where they differ
pval_same <- pval_new %>%
  mutate(Pval_same = (Pval < .05 & Pval_old < .05) |
           (Pval >= .05 & Pval_old >= .05)) %>%
  pull(Pval_same)

pval_diff <- pval_new[which(!is.na(pval_same) & !pval_same),]

#--- temperature medians -------------------------------------------------------

temp_med <- temperature_data %>%
  mutate(Weekday = wday(Date, label = TRUE),
         Month = month(Date, label = TRUE, abbr = FALSE),
         Year = year(Date)) %>%
  filter(!Weekday %in% c("Sat", "Sun")) %>%
  select(-Weekday) %>%
  filter(!is.na(Mean_Temp)) %>%
  group_by(Station, Year, Month) %>%
  summarize(Median = median(Mean_Temp), .groups = "drop")

# quick check
full_join(
  temp_med %>%
  filter(Year == 2020) %>%
  select(Station, Pollutant, Year, Month, Median2 = Median),
  pollutant_pval %>%
  filter(Period == 2020) %>%
  mutate(Year = as.numeric(Period)) %>%
  select(Station, Pollutant, Year, Month, Median)
) %>%
  mutate(med_diff = Median - Median2) %>%
  print(n = Inf)

#--- save ----------------------------------------------------------------------

## write.csv(pval_new,
##           file = get_filename(path = file.path("data", "temperature"),
##                               "temperature_pval", ext = "csv"))
## write.csv(pval_diff,
##           file = get_filename(path = file.path("data", "temperature"),
##                               "temperature_pval_diff", ext = "csv"))
## write.csv(temp_med,
##           file = get_filename(path = file.path("data", "temperature"),
##                               "temperature_median", ext = "csv"))

#--- irradiance pvalues --------------------------------------------------------

# where to save pvalue calculations
data_path <- file.path("data", "irradiance")

# also, add the pvalues from the previous analysis
pval_old <- read_csv(get_filename(path = file.path("data", "old"),
                                  "irr-pval_jan_2017_jun_2020_no_wknd",
                                  ext = "csv"),
                     col_types = cols()) %>%
  mutate(Month = factor(Month,
                        levels = month(1:12, label = TRUE, abbr = FALSE),
                        ordered = TRUE)) %>%
  select(Station, Period, Month, Pval_old = Pval_med)

# merge
pval_new <- left_join(irradiance_pval, pval_old)

# keep only those where they differ
pval_same <- pval_new %>%
  mutate(Pval_same = (Pval < .05 & Pval_old < .05) |
           (Pval >= .05 & Pval_old >= .05)) %>%
  pull(Pval_same)

pval_diff <- pval_new[which(!is.na(pval_same) & !pval_same),]

#--- irradiance medians --------------------------------------------------------

irr_med <- irradiance_data %>%
  mutate(Weekday = wday(Date, label = TRUE),
         Month = month(Date, label = TRUE, abbr = FALSE),
         Year = year(Date)) %>%
  filter(!Weekday %in% c("Sat", "Sun")) %>%
  select(-Weekday) %>%
  filter(!is.na(GHI)) %>%
  group_by(Station, Year, Month) %>%
  summarize(Median = median(GHI), .groups = "drop")

# quick check
full_join(
  irr_med %>%
  filter(Year == 2020) %>%
  select(Station, Pollutant, Year, Month, Median2 = Median),
  pollutant_pval %>%
  filter(Period == 2020) %>%
  mutate(Year = as.numeric(Period)) %>%
  select(Station, Pollutant, Year, Month, Median)
) %>%
  mutate(med_diff = Median - Median2) %>%
  print(n = Inf)

#--- save ----------------------------------------------------------------------

## write.csv(pval_new,
##           file = get_filename(path = data_path,
##                               "irradiance_pval", ext = "csv"))
## write.csv(pval_diff,
##           file = get_filename(path = data_path,
##                               "irradiance_pval_diff", ext = "csv"))
## write.csv(irr_med,
##           file = get_filename(path = data_path,
##                               "irradiance_median", ext = "csv"))
