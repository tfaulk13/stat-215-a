# functions for cleaning the data

#### redwood ####
cleanDatesData <- function(date_df) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
    # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day)),
           # make number a continuous variable
           number = as.numeric(as.character(number))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep)
  
  return(date_df)
}


cleanRedwoodData <- function(redwood_df) {
  # Arguments:
  #   redwood_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function
  # Returns:
  #   a data.frame similar to the input `redwood_df` but with cleaned variables
  
  # convert result_time to lubridate ymd_hms format
  redwood_df <- redwood_df %>% 
    mutate(result_time = ymd_hms(result_time))
  
  # continue cleaning df
  redwood_df <- redwood_df %>%
    # rename columns
    rename(temp = humid_temp, iPAR = hamatop, rPAR = hamabot) %>%
    # remove irrelevant column; not sure what this variable is
    select(-humid_adj) %>%
    # remove observations with NAs
    na.omit() %>%
    # order by epoch column and then by nodeid column
    arrange(epoch, nodeid) %>%
    # remove duplicated rows 
    distinct()
  
  return(redwood_df)
}


mergeRedwoodData <- function(date_df, mote_loc_df, 
                             redwood_net_df, redwood_log_df,
                             rm.outliers = F) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the cleanDatesData() 
  #     function
  #   mote_loc_df: a data.frame in the format of the output of the 
  #     loadMoteLocationData() function
  #   redwood_net_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function; redwood network data
  #   redwood_log_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function; redwood local log data
  # Returns:
  #   a data.frame which merges the following information: dates/times, mote 
  #     location, redwood network data, redwood local log data
  
  # add column with source of entry: either log or net
  redwood_net_df <- redwood_net_df %>%
    mutate(source = "net")
  redwood_log_df <- redwood_log_df %>%
    mutate(source = "log")
  
  # combine network and log data
  redwood_df <- rbind(redwood_net_df,
                      redwood_log_df) %>%
    arrange(source) %>%
    # don't allow for duplicates, i.e. a copy on both the local log and network
    distinct(epoch, nodeid, temp, humidity, iPAR, rPAR, .keep_all = T) %>%
    arrange(epoch, nodeid)
  
  
  # note: result_time = "2004-11-10 14:25:00" for data from the log
  # so must merge with the dates df to get the correct times
  
  # merge datetime from dates with redwood_df (via epochs)
  redwood_df <- merge(x = date_df, y = redwood_df,
                      by.x = "number", by.y = "epoch") %>%
    # rename number column to epoch
    rename(epoch = number) %>%
    # add a `numeric` time column
    mutate(time_num = mapply(X = strsplit(time, ":"), 
                             FUN = function(X) 
                             {return(as.numeric(X[1]) + as.numeric(X[2])/60)}))
  
  
  # merge with the node locations data
  redwood_df <- merge(x = redwood_df, y = mote_loc_df, 
                      by.x = "nodeid", by.y = "ID")
  
  # remove edge nodes (don't have data past week 2 for edge nodes)
  redwood_df <- redwood_df %>%
    filter(Tree != "edge", nodeid != 29) # remove node 29

  
  # voltage for network data and log data are not comparable;
  # after investigations, turns out 1/(log_voltage * net_voltage) ~ 0.001683729
  # transform voltage for network data
  redwood_df[redwood_df$source == "net","voltage"] <- 
    1/(0.001683729 * redwood_df[redwood_df$source == "net","voltage"])
  
  return(redwood_df)
}



rmOutliersRedwoodData <- function(redwood_df) {
  # Arguments:
  #   redwood_df: a data.frame in the format of the output of the 
  #     mergeRedwoodData() function
  # Returns:
  #   a list of two:
  #     redwood_rm_out: a data.frame of the format `redwood_df` 
  #       with outliers removed
  #     redwood: a data.frame of the format `redwood_df` with an extra column
  #       which serves to indicate whether the data point is an outlier
  
  # data frame with the thresholded voltages and epochs which were identified 
  # from the exploration in the lab1.Rmd file
  thresh_df <- data.frame(node = c(78, 3, 123, 59, 138, 141, 145),
                          voltage_thresh = c(2.39, 2.35, 2.34, 2.34, 2.35,
                                             NA, NA),
                          epoch_thresh = c(NA, NA, NA, NA, NA, 8902, 2871))
  
  # initialize outlier column
  redwood_df <- redwood_df %>% mutate(outlier = F) 
  for (idx in 1:nrow(thresh_df)) { # for each outlier node
    node <- thresh_df$node[idx]
    voltage_thresh <- thresh_df$voltage_thresh[idx] # get voltage threshold
    if (is.na(thresh_df$epoch_thresh[idx])) { # or get epoch threshold
      epoch_thresh <- redwood_df %>% 
        filter(nodeid == node, voltage < voltage_thresh) %>%
        select(epoch) %>% min()
      thresh_df$epoch_thresh[idx] <- epoch_thresh
    }else {
      epoch_thresh <- thresh_df$epoch_thresh[idx]
    }
    
    # set all observations after thresholded time point to be outliers 
    redwood_df$outlier[redwood_df$nodeid == node & 
                         redwood_df$epoch >= epoch_thresh] <- T
  }
  
  return(list(redwood = redwood_df, 
              redwood_rm_out = redwood_df %>% filter(outlier == F)))
}


#### linguistics ####
cleanLingData <- function(ling_df) {
  # Arguments:
  #   ling_df: a data.frame in the format of the output of the 
  #     loadLingData() function
  # Returns:
  #   a data.frame similar to the input `ling_df` but with cleaned variables
 
  ling_df <- ling_df %>%
    # CITY column: some cities start with lowercase and other with uppercase, so 
    # capitalize first letter of each word for uniformity
    mutate(CITY = factor(capitalize(as.character(CITY)))) %>%
    # some ZIP codes are only 4 digits long because leading 0 was dropped when 
    # converting to numeric; create ZIP strings column
    mutate(ZIP_str = str_pad(string = ZIP, width = 5, side = "left", pad = 0))
    
  # Note: some STATE and CITY entries are strange, but we will assume that the
  # ZIP is ok
    
  # STATE column: 3 NAs; cities don't correspond to zip, so unclear whether 
  # these locations are correct/relevant (e.g. out of country?)
  # ling_df %>% filter(is.na(STATE))
  ling_df <- ling_df %>% filter(!is.na(STATE))
  
  # ZIP column: 0 NAs
  
  # lat and long columns: each has 1020 NAs; impute these using ZIP and state
  # (use state as a double checking; just in case they put incorrect ZIP)
  data("zipcode") # load in zipcode data from library(zipcode)
  ling_df_missing <- ling_df %>% filter(is.na(lat)) # entries with missing lat
  missing_zip <- merge(x = ling_df_missing, y = zipcode, 
                       by.x = c("ZIP_str", "STATE"), by.y = c("zip", "state"),
                       all.x = T) %>%
    select(ID, latitude, longitude) # get lat/long for missing entries
  # merge with existing data
  ling_df <- merge(x = ling_df, y = missing_zip, by = "ID", all.x = T) %>%
    mutate(lat = ifelse(is.na(lat), latitude, lat),
           long = ifelse(is.na(long), longitude, long)) %>%
    select(-latitude, -longitude) %>%
    arrange(ID)
  
  # after merging with zipcodes, now only have 648 missing latitude/longitudes
  # table(ling_df %>% filter(is.na(latitude)) %>% select(ZIP))
  # some of the zips have missing lat/long but are repeated many times
  # e.g. 12462 (36 entries), 32620 (41 entries), 76350 (35 entries), 
  #      95411 (86 entries)
  # can look into these missing latitude/longitudes to see that many zips are
  # old and have been changed
  # this may introduce biased against some locations, but can't do much about it
  ling_df <- ling_df %>% filter(!is.na(lat))
  
  # Note: Question columns: 0 NAs, but entry = 0 means no response
  # let's look and see how many people didn't answer all of the questions
  # add column which counts the number of unanswered questions per person
  quest_dat <- ling_df %>% select(starts_with("Q")) # only get questions data
  ling_df <- ling_df %>%
    mutate(n_unanswered = apply(quest_dat, 1, 
                                FUN = function(X) {return(sum(X == 0))}))
  
  # remove rows with too many unanswered questions
  ling_df <- ling_df %>%
    filter(n_unanswered < 10)
  
  return(ling_df)
}

cleanLingLocationData <- function(ling_df) {
  # Arguments:
  #   ling_df: a data.frame in the format of the output of the 
  #     loadLingLocationData() function; binned by counties
  # Returns:
  #   a data.frame similar to the input `ling_df` but with cleaned variables,
  #   binned by county
  
  
  # transform categorical variables to binary indicator
  quest <- ling_df %>% select(starts_with("Q")) # only get the question/answers
  rownames(quest) <- ling_df$ID
  quest_binary <- binary_encoding(df = quest)
  ling_binary <- cbind(lat = ling_df$lat, long = ling_df$long, quest_binary)
  
  # find the county, state for each (latitude, longitude) pair
  ling_counties <- latlong2county(loc_df = ling_binary %>%
                                    select(long, lat))
  
  # add county, state to ling binary data
  ling_binary <- cbind(ling_counties, ling_binary) %>%
    # some cities didn't have matches, but only not too many
    na.omit()
  
  # group observations by county
  binned_loc <- ling_binary %>%
    select(-lat, -long) %>%
    group_by(county, state)
  
  sum_binned <- binned_loc %>% summarise_all(funs(sum)) # aggregate and sum
  n_binned <- binned_loc %>% summarise(n = n()) # count responses in each county
  
  # get sum of the bin answers and the number of observations in each bin
  ling_loc_df <- merge(x = n_binned, y = sum_binned, 
                       by = c("county", "state")) %>%
    arrange(state, county)
  
  # normalize by the number of people in each bin to get "average"
  avg_binned <- ling_loc_df[,4:ncol(ling_loc_df)] / ling_loc_df$n
  ling_loc_df[,4:ncol(ling_loc_df)] <- avg_binned
  
  counties_unique <- map_data("county") %>% 
    distinct(region, subregion, .keep_all = T) %>%
    select(region, subregion, long, lat)
  ling_loc_df <- merge(x = ling_loc_df, y = counties_unique,
                       by.x = c("county", "state"), 
                       by.y = c("subregion", "region"))
  
  return(ling_loc_df)
}

cleanLingLocationData2 <- function(ling_df) {
  # Arguments:
  #   ling_df: a data.frame in the format of the output of the 
  #     loadLingLocationData() function (i.e. binned lat/long squares with 
  #     binary encoding)
  # Returns:
  #   a data.frame similar to the input `ling_df` but with cleaned variables

  # transform categorical variables to binary indicator
  quest <- ling_df %>% select(starts_with("Q")) # only get the question/answers
  rownames(quest) <- ling_df$ID
  
  quest_binary <- binary_encoding(df = quest)
  
  # bin observations by 1 degree latitude x 1 degree longitude squares
  binned_loc <- cbind(lat = ling_df$lat, long = ling_df$long, quest_binary) %>%
    mutate(lat = round(lat), long = round(long)) %>%
    group_by(lat, long)
  
  sum_binned <- binned_loc %>% summarize_all(funs(sum))
  n_binned <- binned_loc %>% summarise(n = n())
  
  # get sum of the bin answers and the number of observations in each bin
  ling_loc_df <- merge(x = n_binned, y = sum_binned, 
                       by = c("lat", "long")) %>%
    arrange(lat, long)
  
  # normalize by the number of people in each bin to get "average"
  avg_binned <- ling_loc_df[,4:ncol(ling_loc_df)] / ling_loc_df$n
  ling_loc_df[,4:ncol(ling_loc_df)] <- avg_binned
  
  return(ling_loc_df)
}

binary_encoding <- function(df) {
  # function to convert data frame of categorical variables to binary indicators
  # (i.e. one-hot encoding)
  #
  # input:
  # -df = data frame of variables to convert to binary indicators
  #
  # output:
  # -binary_df = data frame where columns have been expanded to binary indicator
  
  # convert all columns to factor
  factor_df <- mutate_all(df, as.factor)
  
  # convert all answers to dummy variable i.e. one-hot encoding (caret package)
  dummy_df <- dummyVars(formula = ~., data = factor_df)
  binary_df <- predict(dummy_df, factor_df) %>%
    as.data.frame() %>%
    select(-ends_with(".0")) # removes the "didn't respond" column
  
  rownames(binary_df) <- rownames(df)
  return(binary_df)
}

latlong2county <- function(loc_df) {
  # latlong2county: function to convert latitude and longitudes to county, state
  # 
  # input:
  # -loc_df = data.frame of (long, lat) points to match to county, state
  #
  # output:
  # -county_df = data.frame of the same length as loc_df with two columns
  #       county and state
  
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill = TRUE, col = "transparent", plot = FALSE)
  ids <- sapply(strsplit(counties$names, ":"), function(x) x[1]) 
  counties_sp <- map2SpatialPolygons(counties, IDs=ids,
                                     proj4string = CRS("+proj=longlat +datum=wgs84"))
  
  # Convert loc_df to a SpatialPoints object 
  points_sp <- SpatialPoints(loc_df, 
                            proj4string = CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(points_sp, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  county_names <- sapply(counties_sp@polygons, function(x) x@ID)
  state_names <- sapply(strsplit(county_names, ","), function(x) x[1]) 
  county_names <- sapply(strsplit(county_names, ","), function(x) x[2])
  
  county_df <- data.frame(county = county_names, state = state_names)
  return(county_df[indices,])
}

