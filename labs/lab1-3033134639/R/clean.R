# A function for cleaning the data
library(tidyverse)
library(lubridate)

## Separating out and grabbing data from sonoma-dates file --------------------
epoch_nums <- strsplit(unlist(sonoma_dates[1]), ' ')
epoch_dates <- strsplit(unlist(sonoma_dates[3]), '\' \'')

## Cleaning epoch_nums --------------------------------------------------------
epoch_nums[[1]][[1]] <- NA
epoch_nums[[1]][[2]] <- NA
epoch_nums[[1]][[13003]] <- NA
epoch_nums[[1]][[3]] <- 1
epoch_nums <- lapply(epoch_nums, function(x) x[!is.na(x)])

## Cleaning epoch_dates -------------------------------------------------------
epoch_dates[[1]][[1]] <- 'Tue Apr 27 17:10:00 2004'

## Combing all three epoch lists into a data frame and coerce to integers -----
epoch_df <- data.frame(epoch_nums, epoch_dates, stringsAsFactors = FALSE)
colnames(epoch_df) <- c('epoch_nums', 'epoch_dates')
epoch_df$epoch_nums <- as.integer(epoch_df$epoch_nums)

## Merging data ---------------------------------------------------------------
all_data <- left_join(data, motes, by = c('nodeid' = 'ID'))
all_data <- left_join(all_data, epoch_df, by = c('epoch' = 'epoch_nums'))

## Transforming epoch_dates into POSIXct format and removing result-time ------
all_data$epoch_dates <- parse_date_time(all_data$epoch_dates, order = 'a b! d! H!:M!:S! Y!')

## Removing NAs ---------------------------------------------------------------
all_data <- all_data %>%
  filter(!is.na(Height)) %>%  # Removed Height and Dist NAs
  filter(!is.na(humidity))  # Removed humidity, humid_temp, humid_adj, hamatop, hamabot NAS

## Create unclean data for voltage plot
voltage_data <- all_data %>%
  select(voltage)

## Removing values outside of sensor range -----------------------------------
all_data <- all_data %>%
  filter(voltage < 600 | is.na(voltage)) %>% # Voltage
  filter(humidity >= 16.4, humidity <= 100.2) %>% # Humidity
  filter(humid_temp >= 6.6, humid_temp <= 32.6) %>% # Temperature
  filter(hamatop >= 0, hamatop <= 2154) %>% # Incident PAR
  filter(hamabot >= 0, hamabot <= 180) # Reflected PAR

## Remove Voltage -----------------------------------------------------------
all_data <- all_data %>%
  select(-c(result_time, voltage))

## Rename variables ---------------------------------------------------------
all_data <- all_data %>%
  rename(date_time = epoch_dates) %>%
  rename(height = Height) %>%
  rename(distance = Dist) %>%
  rename(direction = Direc) %>%
  rename(temperature = humid_temp) %>%
  rename(humidity_adj = humid_adj) %>%
  rename(par_incident = hamatop) %>%
  rename(par_reflected = hamabot) %>%
  rename(tree = Tree)

## Preparing date and time columns for easy analysis / plotting --------------
all_data$date <- as.Date(all_data$date_time)
all_data$time <- format(all_data$date_time, '%H:%M:%S')
all_data$time <- as.POSIXct(all_data$time, format = '%H:%M:%S')

## Creating date labels for box plot ------------------------------------------
date_df <- all_data %>%
  group_by(date) %>%
  summarize(median = median(humidity_adj)) %>%
  arrange(median)

date_labels <- format(date_df$date, format = '%b %d')


###############################################################################
########  MISCELLANEOUS CODE - USED FOR EXP AND PLOTS, NOT CLEANING  ##########
###############################################################################

## Sample data for exploration ------------------------------------------------
sample_data <- sample_n(all_data, 10000)

## Data for plotting ----------------------------------------------------------
high_humid <- all_data %>%
  filter(date == '2004-05-25' | date == '2004-05-26' | date == '2004-05-27')

low_humid <- all_data %>%
  filter(date == '2004-05-01' | date == '2004-05-02' | date == '2004-05-03')









