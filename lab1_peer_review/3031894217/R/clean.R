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

MergeCleanRedwoodData <- function(log_df, net_df) {
  #find epoch where battery falls below 2.45 volts, and eliminate the following network entries
  min_volt_epoch <- log_df %>% filter(voltage < 2.45) %>% group_by(nodeid) %>% summarize(base_epoch = min(epoch))
  net_voltclean <- net_df
  for (i in 1:nrow(min_volt_epoch)) {
    net_voltclean <- net_voltclean %>% filter(!(nodeid == min_volt_epoch$nodeid[i] & epoch > min_volt_epoch$base_epoch[i]))
  }
  
  #merge the clean network entries with the local log data
  merged_orig<-rbind(log_df,net_voltclean)
  #remove duplicate entries (local logs have preference)
  merged_clean<-merged_orig[!duplicated(merged_orig[,c("nodeid","epoch")]),]
  return(merged_clean)
} 

cleanRedwoodData <- function(redwood_df) {
  # convert result_time to lubridate ymd_hms format
  
  # do anything else you feel is necessary
  # sanity cleaning
  clean_df <- redwood_df %>% filter(nodeid < 6000) %>% filter(humidity >= 0, humid_temp > -30, humid_temp < 45) %>% filter(!is.na(humid_temp))
  # we don't know anything about the location of node 135 or node 100
  clean_df <- clean_df %>% filter(!nodeid == 135) %>% filter(!nodeid == 100)
  # node 40 had unreliable light measurments
  clean_df <- clean_df %>% filter(!nodeid == 40)
  # node 122 had wrong humidity readings compared to other nodes in the same height, tree, and direction
  clean_df <- clean_df %>% filter(!nodeid == 122)
  return(clean_df)
}

