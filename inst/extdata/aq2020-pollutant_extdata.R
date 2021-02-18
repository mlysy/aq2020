#' ---
#' title: "Preparation of the Pollutant Data"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_document:
#'     theme: readable
#'     highlight: tango
#'     toc: yes
#'     toc_float: true
#' ---
#'
#' # Summary
#'
#' This document shows how to obtain the pollutant data from the AQO website, convert it to the desired format, calculate p-values, and produce the corresponding plots in the paper.
#'
#' First let's load the R packages needed to perform all calculations.

#+ setup
# load packages
## require(tidyverse)
require(aq2020)
require(tidyr)
require(dplyr)
require(rvest)
require(lubridate)

#' # General Information about Stations and Pollutants
#'
#' This will create a data frame with the following columns:
#'
#' - `station`: The station name.
#' - `station_id`: The numeric station ID (for webscraping).
#' - `pollutant`: The pollutant name (`O3`, `PM25`, `NO2`, `CO`, `SO2`).
#' - `poll_id`: The numeric pollutant ID (for webscraping).
#' - `has_poll`: `TRUE/FALSE`, depending on whether the given station has records for the given pollutant.
#'
#' This data frame has already been saved in the **aq2020** package and can be accessed as follows:

#+ pollutant_info
pollutant_info

#' The following code shows how to calculate this data from scratch and save it as a CSV file.
#'
#' **WARNING:** The dataset `pollutant_info` in the **aq2020** package may differ from a more recent webscrape version using the code below.  This is because the AQO periodically updates its station IDs.

#+ pollutant_info_calc
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

# keep only the combinations of stations and pollutants
# for which there is data on the AQO website
station_polls <- sapply(stations, function(station_id) {
  ## station <- paste0("http://www.airqualityontario.com/history/station.php?stationid=",
  ##                   station_id,
  ##                   "&submit_station=Choose+Station")
  station <- paste0("http://www.airqualityontario.com/history/station.php?stationid=",
                    station_id)
  station <- read_html(station)
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

# combine into a tibble
statpoll <- as_tibble(statpoll_hasdata) %>%
  mutate(station = names(stations),
         station_id = as.numeric(stations)) %>%
  pivot_longer(O3:SO2, names_to = "pollutant", values_to = "has_poll") %>%
  mutate(pollutant = gsub("\\[[.]\\]", "", pollutant),
         poll_id = as.numeric(pollutants[pollutant])) %>%
  select(station, station_id, pollutant, poll_id, has_poll)

# uncomment the line below to save this data as a CSV file
## write_csv(statpoll, path = "aq2020-pollutant_info.csv")

#' # Webscrape the Pollutant Data
#'
#' For each station and pollutant combination for which there is data on the AQO website, the following code pulls this data from the website by year, with the option to pick only specific months.  Here we'll take all available data from 2017-2020.
#'
#' The first step is to save each webscrape as a file of the form `Barrie_NO2_2020_1_12.rds`.  This is a compressed format consisting here of the `NO2` data from the Barrie station for 2020, for months January through December.
#'
#' **Warning:** The webscraping below may not provide the same results as those saved in the **aq2020** package.  This is because the AQO periodically updates the data on its website.

#+ webscrape_initial
# where to save raw queries
data_path <- file.path("data", "pollutant", "raw")

# helper function to create file names for saving data.
get_filename <- function(path, ..., ext = "rds") {
  fname <- paste0(c(...), collapse = "_")
  file.path(path, paste0(fname, ".", ext))
}

# setup webscrape query information
query_dates <- tibble(
  year = 2017:2020,
  start_month = 1,
  end_month = 12
)
# combine with station/pollutant information
query_info <- pollutant_info %>%
  filter(has_poll) %>% # keep only stat/poll combo for which there is data
  mutate(by = rep(1, n())) %>%
  full_join(query_dates %>%
            mutate(by = rep(1, n()))) %>%
  select(year, start_month, end_month,
         station, station_id, pollutant, poll_id)
nquery <- nrow(query_info)

# helper function to:
#
# - webscrape a query (given station/pollutant/year)
# - format consistently (see [aq2020::get_aqoyr()])
# - save as `data_path/station_pollutant_year_startmonth_endmonth.rds`
#
# if any of the queries fail, it saves the file with the content `NA`.
get_query <- function(query) {
  message("Station: ", query$station,
          ", Pollutant: ", query$pollutant,
          ", Year: ", query$year)
  query_yr <- tryCatch(
    get_aqoyr(station_id = query$station_id,
              poll_id = query$poll_id,
              year = query$year,
              start_month = query$start_month,
              end_month = query$end_month,
              fill = TRUE),
    error = function(e) NA)
  # save data
  saveRDS(query_yr,
          file = get_filename(path = data_path,
                              query$station, query$pollutant,
                              query$year, query$start_month, query$end_month))
}

# number of seconds to wait between website scrapes
wait_time <- c(min = .5, max = 1)

# query subset
# in this case, we just need 2020 as the other years
# were saved in a previous run
sub_query <- which(query_info$year < 2020)

system.time({
  for(ii in sub_query) {
    query <- query_info[ii,]
    get_query(query)
    # wait between queries for a random amount of time
    Sys.sleep(runif(1, min = wait_time["min"], max = wait_time["max"]))
  }
})

#' ## Testing
#'
#' Now let's check that all stations and pollutant datasets have been successfully queried for years 2017-2020:

#+ webscrape_check_years
# check if we have all yearly datasets
sub_query <- which(query_info$year %in% 2017:2020)
bad_ind <- sapply(sub_query, function(ii) {
  query <- query_info[ii,]
  ## message("Station: ", query$station,
  ##         ", Pollutant: ", query$pollutant,
  ##         ", Year: ", query$year)
  query_yr <- readRDS(get_filename(path = data_path,
                                   query$station, query$pollutant,
                                   query$year, query$start_month,
                                   query$end_month))
  all(is.na(query_yr))
})

query_info[sub_query[bad_ind],] %>% print(n = Inf)

#' We can see that the webscrape failed in a few years for a few of the station/pollutant combinations.  We can manually check the AQO website to see what's going on.  So for example, to see what happend in Milton for O3 in 2017, take the following URL:
#'
#' ```
#' http://www.airqualityontario.com/history/index.php?c=Academic&s={station_id}&y={year}&p={poll_id}&m={start_month}&e={end_month}&t=html&submitter=Search&i=1
#' ```
#'
#' and replace the following terms:
#'
#' - `{station_id}`: The station ID for Milton, which is 44029.
#' - `{year}`: The year in question, which in this case is 2017.
#' - `{poll_id}`: The pollutant ID, which in this case is 122.
#' - `{start_month}`: The start month as a number, which is 1 (January).
#' - `{end_month}`: The end month as a number, which is 12 (December).
#'
#' So the URL above becomes:
#'
#' ```
#' http://www.airqualityontario.com/history/index.php?c=Academic&s=44029&y=2017&p=122&m=1&e=12&t=html&submitter=Search&i=1
#' ```
#'
#' At the time of this writing, following this URL above produces the message:
#'
#' ```
#' Sorry no results found for Ozone at Milton(44029) between January and December of the year 2017.
#' ```
#'
#'
#' # Process the Raw Data
#'
#' A raw data file from the webscrape looks like this:

#+ webscrape_display
query <- query_info[1,] # look at the first query
query_yr <- readRDS(get_filename(path = data_path,
                                 query$station, query$pollutant,
                                 query$year, query$start_month,
                                 query$end_month))
# display
message("Station: ", query$station,
        ", Pollutant: ", query$pollutant,
        ", Year: ", query$year)
as_tibble(query_yr)

#' Cleaning the raw concentration data involves the following steps:
#'
#' - Replacing erronous hourly concentration values of `-999` or `9999` by `NA`.
#' - Convert hourly concentrations to daily averages.
#' - Combine all stations/pollutants into a single data frame.
#' - Keep only columns `Date`, `Station`, `Pollutant`, and `Concentration`.
#'
#' The result of these processing steps are available in the **aq2020** package in the `pollutant_data` object:

#+ pollutant_data
pollutant_data

#' **Warning:** The 2019 and 2020 data are slightly different now (Jan 23, 2021) from what they were when last scraped (July 7, 2020).  The original data are save in the folder `/Users/mlysy/Documents/proj/chemODE/covid/data/raw`.

#+ pollutant_data_calc
# merge webscrape files
pollutant_data <- lapply(which(!bad_ind), function(ii) {
  query <- query_info[ii,]
  readRDS(get_filename(path = data_path,
                       query$station, query$pollutant,
                       query$year, query$start_month,
                       query$end_month)) %>%
    as_tibble() %>%
    mutate(Station = query$station,
           Pollutant = query$pollutant) %>%
    select(Date, Station, Pollutant, H01:H24)
}) %>% bind_rows()

# process
pollutant_data <- pollutant_data %>%
  mutate(Date = ymd(Date)) %>%
  mutate_at(vars(H01:H24), .funs = ~na_if(., 9999)) %>%
  mutate_at(vars(H01:H24), .funs = ~na_if(., -999)) %>%
  pivot_longer(cols = H01:H24, names_to = "Hour",
               values_to = "Concentration") %>%
  group_by(Date, Station, Pollutant) %>%
  summarize(Concentration = mean(Concentration, na.rm = TRUE),
            .groups = "drop") %>%
  rownames(pollutant_data) <- NULL

#' # P-value Calculation

#' # Scratch
#'
#' The following code is depreciated test code kept only for reference.

#+ scratch, eval = FALSE
# now test
left_join(x = pollutant_data,
          y = pollutant_data) %>%
  mutate(check = Concentration == Concentration2) %>%
  filter(!check) %>%
  arrange(Date) %>% print(n = Inf)

# ok let's take a closer look

new_path <- data_path
old_path <- "/Users/mlysy/Documents/proj/chemODE/covid/data/raw"

get_filename <- function(path, ..., ext = "rds") {
  fname <- paste0(c(...), collapse = "_")
  file.path(path, paste0(fname, ".", ext))
}

station <- "Brampton"
pollutant <- "NO2"
date <- "2019-01-02"
new_data <- get_filename(path = new_path,
                         station, pollutant,
                         2019, 1, 12) %>%
  readRDS() %>%
  as_tibble()
old_data <- get_filename(path = old_path,
                         station, pollutant,
                         2019, 1, 12) %>%
  readRDS() %>%
  as_tibble()

new_data %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date == date)
old_data %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date == date)
