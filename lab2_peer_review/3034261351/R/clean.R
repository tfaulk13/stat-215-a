# A function for cleaning the data
# be sure to load the packages from lab1.Rnw first!

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
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep)
 
  return(date_df)
}


cleanRedwoodData <- function(redwood_df) {
  # Arguments:
  #   redwood_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function
  # Returns:
  #   a data.frame similar to the input 'redwood.log' and 'redwood.net' 
  #       but without outliers, missing values, nor inconsistencies 
  
  # match real date for redwood_df
  colnames(dates)[1] <- c("epoch")
  dates$epoch <- as.numeric(as.character(dates$epoch))
  # full join dates and redwood_df
  redwood_df <- redwood_df %>%
    full_join(dates[, c(1, 3, 5)], by = c("epoch")) %>%
    # delect incorrect date and time
    select( -result_time) %>%
    # rename datetime as original result_time
    dplyr::rename(result_time = datetime) %>%
    # convert result_time to lubridate ymd_hms format
    mutate(result_time = ymd_hms(result_time)) %>%
    # remove extra large node id and rows with zero observation
    filter( nodeid < 201 & !is.na(nodeid)) %>% 
    # round values to 1 digit
    mutate_if(is.numeric, round, 1) %>%
    # remove duplicate row in each data set
    distinct()
  
  log.sum <- redwood_df %>%
    # group rows by epoch and node id
    group_by(epoch, nodeid) %>%
    # count the number of the same nodes at the same time
    summarise(length = n()) %>%
    # filter rows which simultaneously observe the same nodes at the same time
    filter(length > 1)
  
  redwood_df <- redwood_df %>%
    # remove all observations which have different values for the same nodes at the same time
    # There are only 74 observations and 1 observation which have different values for the same node at the same time. 
    # We remove all of them (75) as the total number is quite small and neglectable, 
    # and there is very likely to be some measurement error which causes multiple different observations for the same
    # mote at the same timepoint. 
    anti_join(log.sum, by = c("epoch", "nodeid"))
  
  return(redwood_df)
}

