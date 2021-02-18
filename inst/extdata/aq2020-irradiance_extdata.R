#' ---
#' title: "Preparation of the Temperature Data"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_document:
#'     theme: readable
#'     highlight: tango
#'     toc: yes
#'     toc_float: true
#' ---
#'
#' # Convert Raw `csv` Data to `rds` Files
#'
#' First let's load the necessary R packages.

#+ setup
require(dplyr)
require(tidyr)
require(readr)
require(lubridate)
require(fs) # file names
# helper function to create file names for saving data.
get_filename <- function(path, ..., ext = "rds") {
  fname <- paste0(c(...), collapse = "_")
  file.path(path, paste0(fname, ".", ext))
}

#' Next let's create the `irradiance_info` file for parsing.

#+ irradiance_info
# list of all the stations.
data_path <- path("..", "..", "..", "data", "irradiance", "raw")
irradiance_files <- unlist(dir_map(data_path, fun = basename))

irradiance_info <- tibble(station = c("Delhi", "Ottawa"),
                          station_name = c("DELHI", "OTTAWA"))

#' OK.  Now we'll combine the old data and new data for each station and save as an `rds` file.  Station has two files, some with older data some with newer.  For all overlapping dates, the newer file is always used.  This happens for all dates as of Jan 1, 2019.

#+ irradiance_rds
save_path <- path("data", "irradiance", "raw")
station_list <- gsub(".*(DELHI|OTTAWA).*", "\\1", irradiance_files)
year_list <- gsub(".*(2017-2020|2019-2020).*", "\\1", irradiance_files)

for(ii in 1:nrow(irradiance_info)) {
  # station info
  station <- irradiance_info$station[ii]
  station_name <- irradiance_info$station_name[ii]
  # old data: prior to 2019
  fname <- station_list == station_name & year_list == "2017-2020"
  fname <- irradiance_files[fname]
  irr_old <- read_csv(path(data_path, fname), col_types = cols()) %>%
    filter(`year_Z (UTC)` %in% c(2017, 2018))
  # new data: as of 2019
  fname <- station_list == station_name & year_list == "2019-2020"
  fname <- irradiance_files[fname]
  irr_new <- read_csv(path(data_path, fname), col_types = cols()) %>%
    filter(`year_Z (UTC)` %in% c(2019, 2020))
  # combine
  irr_data <- bind_rows(irr_old, irr_new)
  # save
  saveRDS(irr_data, file = get_filename(path = save_path, station, "2017-2020"))
}

#' # Convert Raw `rds` files to Single File
#'
#' This file is included in the **aq2020** package as `irradiance_data`.

data_path <- path("data", "irradiance", "raw") # where to load files from

irradiance_data <- lapply(irradiance_info$station, function(station) {
  irr_data <- readRDS(get_filename(path = data_path, station, "2017-2020"))
  irr_data %>%
    transmute(Year = `year_Z (UTC)`,
              Month = month(`month_Z (UTC)`, label = FALSE, abbr = TRUE),
              Day = `day_Z (UTC)`,
              Hour = `hr_Z(UTC)`,
              GHI = `GHI(Wm-2)`) %>%
    group_by(Year, Month, Day) %>%
    ## print(n = 200)
    summarize(GHI = sum(GHI, na.rm = TRUE), .groups = "drop") %>%
    drop_na() %>%
    mutate(Date = ymd(paste0(Year, "-", Month, "-", Day)),
           Station = station) %>%
    select(Date, Station, GHI)
}) %>% bind_rows()


#--- scratch -------------------------------------------------------------------

# test against old data

left_join(
  aq2020::irradiance_data,
  irradiance_data %>%
  select(Date, Station, GHI2 = GHI)
) %>% mutate(check = GHI == GHI2) %>%
  filter(!check)

# old pull

irradiance_data <- lapply(irradiance_files, function(fname) {
  station_name <- gsub(".*(DELHI|OTTAWA).csv", "\\1", fname)
  station <- irradiance_info %>%
    filter(station_name == !!station_name) %>%
    pull(station)
  irr_data <- read_csv(path(data_path, fname), col_types = cols())
  irr_data %>%
    transmute(Year = `year_Z (UTC)`,
              Month = month(`month_Z (UTC)`, label = FALSE, abbr = TRUE),
              Day = `day_Z (UTC)`,
              Hour = `hr_Z(UTC)`,
              GHI = `GHI(Wm-2)`) %>%
    group_by(Year, Month, Day) %>%
    ## print(n = 200)
    summarize(GHI = sum(GHI, na.rm = TRUE), .groups = "drop") %>%
    drop_na() %>%
    mutate(Date = ymd(paste0(Year, "-", Month, "-", Day)),
           Station = station) %>%
    select(Date, Station, GHI)
}) %>%
  bind_rows() %>%
  filter(!(duplicated(cbind(Date, Station))))
