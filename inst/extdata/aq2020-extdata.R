#' ---
#' title: "Data for Package"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_document:
#'     theme: readable
#'     highlight: tango
#'     toc: yes
#'     toc_float: true
#' ---

#' # General Information about Stations and Pollutants
#'
#' This corresponds to the dataset `pollutant_info`.

#+ eval = FALSE
# load packages
require(tidyverse)
require(rvest)

# pollutant names and codes
pollutants <- c(O3 = 122, PM25 = 124, NO2 = 36, CO = 46, SO2 = 9)

# station names and ids
stations <- c(
  Barrie = 47045,
  Belleville = 54012,
  Brantford = 21005,
  Burlington = 44008,
  Brampton = 46090,
  Chatham = 13001,
  Cornwall = 56051,
  Grand_Bend = 15020,
  Guelph = 28028,
  Hamilton_Downtown = 29000,
  Hamilton_West = 29118,
  Hamilton_Mountain = 29214,
  Kingston = 52023,
  Kitchener = 26060,
  London = 15026,
  Milton = 44029,
  Mississauga = 46108,
  Newmarket = 48006,
  North_Bay = 75010,
  Oakville = 44017,
  Ottawa_Downtown = 51001,
  Oshawa = 45027,
  Parry_Sound = 49005,
  Peterborough = 59006,
  Port_Stanley = 16015,
  Sarnia = 14111,
  Sault_Ste_Marie = 71078,
  St_Catharines = 27067,
  Sudbury = 77233,
  Thunder_Bay = 63203,
  Toronto_Downtown = 31103,
  Toronto_East = 33003,
  Toronto_West = 35125,
  Windsor_Downtown = 12008,
  Windsor_West = 12016,
  Toronto_North = 34021
)

# which combinations of these have data
station_polls <- sapply(stations, function(station_id) {
  station <- paste0("http://www.airqualityontario.com/history/station.php?stationid=",
                    station_id,
                    "&submit_station=Choose+Station")
  station <- read_html(station)
  ## read_html("http://www.airqualityontario.com/history/station.php?stationid=51001&submit_station=Choose+Station")
  station %>%
    html_node("#right_column table.resourceTable") %>%
    html_table(header = FALSE) %>%
    as_tibble() %>%
    filter(X1 == "Pollutants Measured:") %>%
    pull(X2)
})
statpoll_hasdata <- sapply(c("O3", "PM2[.]5", "NO2", "CO", "SO2"),
                           function(poll) {
  grepl(poll, station_polls)
})

# combine into CSV file
statpoll <- as_tibble(statpoll_hasdata) %>%
  mutate(station = names(stations),
         station_id = as.numeric(stations)) %>%
  pivot_longer(O3:SO2, names_to = "pollutant", values_to = "has_poll") %>%
  mutate(pollutant = gsub("\\[[.]\\]", "", pollutant),
         poll_id = as.numeric(pollutants[pollutant])) %>%
  select(station, station_id, pollutant, poll_id, has_poll)
## write_csv(statpoll, path = "aq2020-pollutant_info.csv")


#' # General Information about Temperature Measurements
#'
#' These are provided in the dataset `temperature_info`.

#+ eval = FALSE
# temperature station information

# list of all the stations.
data_path <- "/Users/mlysy/Dropbox/Shared/AQ2020/Martin-UW/Daily Mean Temperature files"
station_names <- unlist(dir_map(data_path, fun = basename))

# load & check data
stattemp <- vector("list", length(station_names))
for(ii in 1:length(station_names)) {
  station <- station_names[ii]
  # daily temperature files
  clim_files <- path(data_path, station) %>%
    dir_ls(glob = "*P1D.csv")
  # clean data
  clim_data <- lapply(clim_files, function(fname) {
    read_csv(fname) %>%
      select(station_name = `Station Name`,
             station_id = `Climate ID`,
             longitude = `Longitude (x)`,
             latitude = `Latitude (y)`)
  }) %>%
    bind_rows()
  # check uniqueness
  ## if(any(sapply(clim_data, function(x) length(unique(x))) != 1)) {
  ##   stop("More than one station in files.")
  ## }
  stattemp[[ii]] <- clim_data[nrow(clim_data),]
}

# format and save
stattemp <- stattemp %>% bind_rows() %>%
  mutate(station_folder = station_names,
         station = gsub("[()]", "",
                              gsub("[^[:alpha:]()]", "_", station_folder))) %>%
  select(station, station_id, station_name, station_folder,
         longitude, latitude)
## write_csv(stattemp, path = "aq2020-temperature_info.csv")


#' # Combine Pollutant Data
#'
#' This creates the dataset `pollutant_data`.
#'
#' ## Convert raw `rds` files to `csv`

#+ eval = FALSE
statpoll <- read_csv("aq2020-pollutant_info.csv") %>%
  filter(has_poll)

year <- c("2017_1_12", "2018_1_12", "2019_1_12", "2020_1_6")

no_wknd <- FALSE
for(ii in 1:nrow(statpoll)) {
  station <- statpoll$station[ii]
  pollutant <- statpoll$pollutant[ii]
  fname <- file.path("data", paste0(station, "_", pollutant, "_", year, ".rds"))
  # remove convert bad data to NA and remove weekends
  poll_data <- lapply(fname, function(fn) {
  readRDS(fn) %>%
    as_tibble()
}) %>% bind_rows() %>%
  mutate(Date = ymd(Date)) %>%
  mutate_at(vars(H01:H24), .funs = ~na_if(., 9999)) %>%
  mutate_at(vars(H01:H24), .funs = ~na_if(., -999))
  if(no_wknd) {
    poll_data <- poll_data %>%
      mutate(Day = wday(Date, label = TRUE)) %>%
      filter(!Day %in% c("Sat", "Sun")) %>%
      select(-c(Station, Day))
  } else {
    poll_data <- poll_data %>%
      select(-Station)
  }
  # store
  write_csv(poll_data,
            path = file.path("data",
                             paste0("2017-2020",
                                    ifelse(no_wknd, "_no_wknd", "")),
                             paste0(station, "_", pollutant, "_",
                                    "Jan_2017_Jun_2020.csv")))
}

#' ## Merge station and pollutant combinations

#+ eval = FALSE
# station/pollutant data
statpoll <- read_csv("aq2020-pollutant_info.csv") %>%
  filter(has_poll) %>% arrange(pollutant)
poll_label <- c(O3 = "O[3]*' Concentration '*(ppb)",
                PM25 = "PM[2.5]*' Concentration '*(mu*g/m^3)",
                NO2 = "NO[2]*' Concentration '*(ppb)",
                SO2 = "SO[2]*' Concentration '*(ppb)",
                CO = "CO*' Concentration '*(ppm)")
no_wknd <- FALSE
data_period <- "jan_2017_jun_2020"
wknd_suffix <- ifelse(no_wknd, "no_wknd", "all_days")
data_path <- path("data", "concentration", paste0("2017-2020_", wknd_suffix))

# helper function to format filenames
get_fname <- function(path, ...) {
  fname <- paste0(list(...), collapse = "_")
  file.path(path, paste0(fname, ".csv"))
}

pollutant_data <- lapply(1:nrow(statpoll), function(ii) {
  station <- statpoll$station[ii]
  pollutant <- statpoll$pollutant[ii]
  poll_data <- read_csv(get_fname(data_path, station, pollutant, data_period))
  poll_data %>%
    ## mutate(Year = year(Date),
    ##        Month = month(Date, label = TRUE, abbr = FALSE),
    ##        Day = day(Date)) %>%
    ## filter(Month %in% month(1:6, label = TRUE, abbr = FALSE)) %>%
    pivot_longer(cols = H01:H24, names_to = "Hour",
                 values_to = "Concentration") %>%
    ## group_by(Year, Month, Day) %>%
    group_by(Date) %>%
    summarize(Concentration = mean(Concentration, na.rm = TRUE),
              .groups = "drop") %>%
    transmute(Date = Date,
              Station = station,
              Pollutant = pollutant,
              Concentration = Concentration)
})

pollutant_data %>% bind_rows() %>%
  write_csv("aq2020-pollutant_data.csv")

#' # Merge temperature stations
#'
#' This provides the dataset `temperature_data`.

#+ eval = FALSE
stattemp <- read_csv("aq2020-temperature_info.csv")
data_path <- file.path("data", "temperature", "2017-2020")
no_wknd <- FALSE
if(no_wknd) {
  data_path <- paste0(data_path, "_no_wknd")
} else {
  data_path <- paste0(data_path, "_all_days")
}

temperature_data <- lapply(1:nrow(stattemp), function(ii) {
  station <- stattemp$station[ii]
  temp_data <- read_csv(get_fname(data_path, station, data_period)) %>%
    mutate(Station = station) %>%
    select(Date, Station, Max_Temp:Mean_Temp)
}) %>% bind_rows()
## write_csv(temperature_data, path = "aq2020-temperature_data.csv")
