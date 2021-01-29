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


#' Next let's create the `temperature_info` file for parsing.

#+ temperature_info
# list of all the stations.
data_path <- path("..", "..", "..", "data", "temperature", "raw")
station_names <- unlist(dir_map(data_path, fun = basename))

# load & check data
temperature_info <- vector("list", length(station_names))
for(ii in 1:length(station_names)) {
  station <- station_names[ii]
  # daily temperature files
  clim_files <- path(data_path, station) %>%
    dir_ls(glob = "*P1D.csv")
  # clean data
  clim_data <- lapply(clim_files, function(fname) {
    read_csv(fname, col_types = cols()) %>%
      select(station_name = `Station Name`,
             station_id = `Climate ID`,
             longitude = `Longitude (x)`,
             latitude = `Latitude (y)`)
  }) %>%
    bind_rows()
  # check uniqueness.
  # note: windsor dt/west changes station ids in 2019.
  # however, the long/lat remains identical.
  # we'll keep the id of just the last one for now.
  ## if(any(sapply(clim_data, function(x) length(unique(x))) != 1)) {
  ##   stop("More than one station in files.")
  ## }
  temperature_info[[ii]] <- clim_data[nrow(clim_data),]
}

# format
temperature_info <- temperature_info %>% bind_rows() %>%
  mutate(station_folder = station_names,
         station = gsub("[()]", "",
                              gsub("[^[:alpha:]()]", "_", station_folder))) %>%
  select(station, station_id, station_name, station_folder,
         longitude, latitude)

#' Finally we can convert the relevant `csv` files in each of the folders into `rds` files all in the same `raw` folder contained in the package.  Let's name these: `{station}_{year}.rds`.

save_path <- path("data", "temperature", "raw") # where to put the files

for(ii in 1:nrow(temperature_info)) {
  station_folder <- temperature_info$station_folder[ii]
  station <- temperature_info$station[ii]
  # files
  clim_files <- path(data_path, station_folder) %>%
    dir_ls(glob = "*P1D.csv")
  for(fname in clim_files) {
    clim_data <- read_csv(fname, col_types = cols()) %>%
      mutate(Station = station) %>%
      select(Station, !Station)
    # determine save name
    station_name <- unique(clim_data$`Station Name`)
    station_id <- unique(clim_data$`Climate ID`)
    station_year <- unique(year(clim_data$`Date/Time`))
    if(any(sapply(list(station, station_id, station_year), length) > 1)) {
      stop("File has more than one station_name, station_id and/or year.")
    }
    saveRDS(clim_data,
            file = get_filename(path = save_path,
                                station, station_year))
  }
}

#' # Convert Raw `rds` files to Single File
#'
#' This file is included in the **aq2020** package as `temperature_data`.

data_path <- path("data", "temperature", "raw")

temperature_data <- lapply(dir_ls(data_path), function(fname) {
  # clean data
  readRDS(fname) %>%
    select(Date = `Date/Time`,
           Station = `Station`,
           Station_Name = `Station Name`,
           Station_ID = `Climate ID`,
           Longitude = `Longitude (x)`,
           Latitude = `Latitude (y)`,
           Max_Temp = `Max Temp (°C)`,
           Min_Temp = `Min Temp (°C)`,
           Mean_Temp = `Mean Temp (°C)`,
           Heat_Deg_Days = `Heat Deg Days (°C)`,
           Cool_Deg_Days = `Cool Deg Days (°C)`,
           Total_Precip = `Total Precip (mm)`,
           Snow_Grnd = `Snow on Grnd (cm)`,
           Dir_Max_Gust = `Dir of Max Gust (10s deg)`,
           Spd_Max_Gust = `Spd of Max Gust (km/h)`) %>%
    mutate(Spd_Max_Gust = as.character(Spd_Max_Gust)) # contains e.g, "<31 km/h"
}) %>% bind_rows()

#' # Scratch

#+ scratch, eval = FALSE

# check against old data
is_equal <- function(x, y) {
  ifelse(is.na(x), is.na(x) & is.na(y), x == y)
}
left_join(aq2020::temperature_data,
          temperature_data %>%
          select(Date, Station,
                 Max_Temp2 = Max_Temp, Min_Temp2 = Min_Temp,
                 Mean_Temp2 = Mean_Temp)) %>%
  mutate(check = is_equal(Max_Temp, Max_Temp2) &
           is_equal(Min_Temp, Min_Temp2) &
           is_equal(Mean_Temp, Mean_Temp2)) %>%
  filter(!check)


#--- p-value calculations ------------------------------------------------------

source("covid-functions.R")

temperature_info <- read_csv("temperature_info_info.csv")

get_fname <- function(path, station) {
  fname <- paste0(station, "_Jan_2017_Jun_2020.csv")
  file.path(path, fname)
}

pval_tibble <- function(x)
  tibble(Pval = setNames(x, NULL), Stat = c("mean", "med"))


# write as function to use parallel package
pval_calc <- function(ii, nsim, path) {
  station <- temperature_info$station[ii]
  message("Station: ", station)
  temp_data <- read_csv(get_fname(path, station))
  # prepare for p-values
  temp_data <- temp_data %>%
    mutate(Year = year(Date),
           Month = month(Date, label = TRUE, abbr = FALSE),
           Day = day(Date)) %>%
    filter(Month %in% month(1:6, label = TRUE, abbr = FALSE))
  # pvalue calculations
  tm <- system.time({
    pv_data <- bind_rows(
      # 2017-2019
      temp_data %>%
      filter(Year != 2020) %>%
      group_by(Month) %>%
      summarize(pval_tibble(fisher_pv(group = Year, value = Mean_Temp,
                                      nsim = nsim,
                                      Tfun = Tmulti)),
                .groups = "drop") %>%
      mutate(Period = "2017-2019"),
      # 2020
      temp_data %>%
      mutate(Year = ifelse(Year == 2020, "2020", "2017-2019")) %>%
      group_by(Month) %>%
      summarize(pval_tibble(fisher_pv(group = Year, value = Mean_Temp,
                                      nsim = nsim,
                                      Tfun = Tbin)),
                .groups = "drop") %>%
      mutate(Period = "2020")
    )
  })
  message("Time: ", round(tm[3], 1), " seconds")
  # append station and pollutant information
  pv_data <- pv_data %>%
    mutate(Station = station)
  pv_data
}

require(parallel)
ncores <- detectCores(logical = FALSE) # number of cores to use
RNGkind("L'Ecuyer-CMRG") # parallel processing seed
cl <- makeCluster(spec = ncores)

data_path <- file.path("data", "temperature", "2017-2020")
no_wknd <- TRUE
if(no_wknd) {
  data_path <- paste0(data_path, "_no_wknd")
} else {
  data_path <- paste0(data_path, "_all_days")
}
nsim <- 1e4

## pv_data <- pval_calc(ii = 2, nsim = nsim, path = paste0(data_path, "_all_days"))

clusterExport(cl, varlist = ls()[ls() != "cl"])
invisible(
  clusterEvalQ(cl, expr = {
    source("covid-functions.R")
  })
)

system.time({
  bad_ind <- parLapply(cl, 1:nrow(temperature_info), fun = function(ii) {
    pv_data <- tryCatch(pval_calc(ii, nsim = nsim, path = data_path),
                        error = function(e) NULL)
    if(!is.null(pv_data)) {
      write_csv(pv_data,
                path = get_fname(file.path(data_path, "pval"),
                                 station = temperature_info$station[ii]))
    }
    is.null(pv_data)
  })
})

# kill cluster
stopCluster(cl)

#--- save p-values as a single file --------------------------------------------

source("covid-functions.R")
temperature_info <- read_csv("temperature_info_info.csv")
get_fname <- function(path, ...) {
  fname <- paste0(list(...), collapse = "_")
  file.path(path, paste0(fname, ".csv"))
}


data_path <- file.path("data", "temperature", "2017-2020")
data_period <- "jan_2017_jun_2020"
no_wknd <- FALSE
wknd_suffix <- ifelse(no_wknd, "no_wknd", "all_days")
data_path <- paste0(data_path, "_", wknd_suffix)

pval_data <- lapply(temperature_info$station, function(station) {
  read_csv(get_fname(path(data_path, "pval"), station, data_period))
})

pval_data %>%
  bind_rows() %>%
  pivot_wider(names_from = "Stat", names_prefix = "Pval_",
              values_from = "Pval") %>%
  select(Station, Period, Month, Pval_mean, Pval_med) %>%
  write_csv(path = get_fname("data", "temp-pval", data_period, wknd_suffix))

#--- boxplot for a given station -----------------------------------------------

data_path <- file.path("data", "temperature", "2017-2020")
no_wknd <- TRUE
if(no_wknd) {
  data_path <- paste0(data_path, "_no_wknd")
} else {
  data_path <- paste0(data_path, "_all_days")
}

station <- "Kitchener_Waterloo"

temp_data <- read_csv(get_fname(data_path, station))
pval_data <- read_csv(get_fname(path(data_path, "pval"), station)) %>%
  pivot_wider(names_from = "Stat", names_prefix = "Pval_", values_from = "Pval")

temp_data %>%
  mutate(Year = factor(year(Date)),
         Month = month(Date, label = TRUE, abbr = FALSE),
         Period = ifelse(Year == 2020, "2020", "2017-2019")) %>%
  filter(Month %in% month(1:6, label = TRUE, abbr = FALSE)) %>%
  ggplot(aes(x = Month, y = Mean_Temp)) +
  geom_jitter(aes(fill = Year, group = Period),
              pch = 21, position = position_jitterdodge(.5),
              size = 2) +
  geom_boxplot(aes(color = Period), outlier.shape = NA, alpha = 0) +
  geom_text(mapping = aes(x = Month, group = Period,
                          label = paste0(signif(Pval_med*100, 2), "%"),
                          y = min(temp_data$Mean_Temp, na.rm = TRUE) - 1),
            data = pval_data, position = position_dodge(.8),
            size = 4) +
  scale_fill_manual(values = c("red", "blue", "orange", "darkgreen")) +
  ylab(parse(text = '"Temperature "*(degree*C)')) +
  ggtitle(paste0("Station: ", station)) +
  multiplot::theme_min2()

#--- scratch -------------------------------------------------------------------

station_names <- unlist(dir_map(data_path, fun = basename))
# pretty names
names(station_names) <- gsub("[()]", "",
                             gsub("[^[:alpha:]()]", "_", station_names))

# capitalize words
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                       {s <- substring(s, 2); if(strict) tolower(s) else s},
                       sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

format_station <- function(x) {
  x <- gsub("[^[:alpha:]]", " ", x) # replace punctuation by space
  x <- capwords(x, strict = TRUE) # capitalize words
  gsub("[[:space:]]+", "_", x) # replace white space by underscore
}


read_csv(clim_files[4]) %>%
  select(`Date/Time`, `Mean Temp (°C)`) %>% print(n = Inf)

clim_files <- dir_map(path(data_path, station), fun = read_csv)

clim_files[[4]] %>%
  select(`Date/Time`) %>% print(n = Inf)
