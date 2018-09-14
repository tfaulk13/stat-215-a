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


cleanRedwoodData <- function(log,net) {
  
  ###############
  # Process the log data first
  ###############
  
  # check the voltage time series
  #ggplot(log)+geom_point(aes(x=epoch, y=voltage))
  
  # get those that has very low voltage values
  #low.voltage <- filter(log, voltage <1)
  
  # check these low voltage nodes for values at other variables
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y = humid_temp))
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y = hamatop))
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y = hamabot))
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y = humidity))
  
  # seems ok, so we just remove the voltage values
  #unique(low.voltage["nodeid"])
  log$voltage[log$nodeid == 128] <- NA
  log$voltage[log$nodeid == 134] <- NA
  log$voltage[log$nodeid == 135] <- NA
  log$voltage[log$nodeid == 141] <- NA
  log$voltage[log$nodeid == 142] <- NA
  log$voltage[log$nodeid == 143] <- NA
  log$voltage[log$nodeid == 145] <- NA
  
  # double check that voltage data is clean
  #ggplot(log)+geom_point(aes(x=epoch, y=voltage))
  
  #####
  
  # now check for humidity
  #ggplot(log)+geom_point(aes(x=epoch, y=humidity))
  
  # get those with negative humidity
  #low.humid <- filter(log, humidity <0)
  
  # check these negative humidity nodes for values at other variables
  #ggplot(low.humid)+geom_point(aes(x=epoch, y = humidity))
  #ggplot(low.humid)+geom_point(aes(x=epoch, y = humid_temp))
  #ggplot(low.humid)+geom_point(aes(x=epoch, y = hamatop))
  #ggplot(low.humid)+geom_point(aes(x=epoch, y = hamabot))
  
  # what are these nodes?
  #group_by(low.humid, nodeid)%>%summarise(n=n())
  # check each node 1 by 1
  
  # get node 29
  #node.29 <- filter(log, nodeid==29)
  #ggplot(node.29) + geom_point(aes(x=epoch, y=humidity)) 
  #ggplot(node.29) + geom_point(aes(x = epoch, y = humid_temp)) 
  #ggplot(node.29) + geom_point(aes(x = epoch, y = hamatop)) 
  #ggplot(node.29) + geom_point(aes(x = epoch, y = hamabot)) 
  
  # probably the sensor isn't working correctly
  log$humidity[log$nodeid == 29] <- NA
  log$humid_temp[log$nodeid == 29] <- NA
  log$humid_adj[log$nodeid == 29] <- NA
  
  # remove all others with negative humidity
  log <- filter(log, humidity >-1)
  
  # double check that humidity data is now clean
  #ggplot(log)+geom_point(aes(x=epoch, y=humidity))
  
  #####
  
  # check temperature data, seems ok after those removal earlier
  #ggplot(log)+geom_point(aes(x=epoch, y=humid_temp))
  
  #####
  
  # check ambient PAR data
  #ggplot(log)+geom_point(aes(x=epoch, y=hamatop))
  
  # there are too high values, let's check
  #high.hamatop <-filter(log, hamatop >150000)
  #group_by(high.hamatop, nodeid)%>%summarise(n=n())
  
  # so it is just node 40
  #node.40 <- filter(log, nodeid==40)
  #ggplot(node.40)+geom_point(aes(x=epoch, y=hamatop))
  #ggplot(node.40)+geom_point(aes(x=epoch, y=hamabot))
  
  # clean up for node 40
  log$hamatop[log$nodeid == 40] <- NA
  log$hamabot[log$nodeid == 40] <- NA
  
  # double check that the data is now clean
  #ggplot(log)+geom_point(aes(x=epoch, y=hamatop))
  
  #####
  
  # check for direct PAR, seems ok
  #ggplot(log)+geom_point(aes(x=epoch, y=hamabot))
  
  #############################################
  # Now we're done with log, we clean up net data
  #############################################
  
  # start with voltage time series
  #ggplot(net)+geom_point(aes(x=epoch, y=voltage))
  
  # there are >1000 voltage values, who's that?
  #low.voltage <- filter(net, voltage >1000)
  
  # cross check for other variables for these voltage levels
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y=humidity))
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y=humid_temp))
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y=hamatop))
  #ggplot(low.voltage)+geom_point(aes(x=epoch, y=hamabot))
  
  # seems ok, so just remove the voltage values
  #unique(low.voltage["nodeid"])
  net$voltage[net$nodeid == 134] <- NA
  net$voltage[net$nodeid == 135] <- NA
  net$voltage[net$nodeid == 141] <- NA
  net$voltage[net$nodeid == 145] <- NA
  
  # double check to see that it's clean
  #ggplot(net)+geom_point(aes(x=epoch, y=voltage))
  
  #####
  
  # check for humidity time series
  #ggplot(net)+geom_point(aes(x=epoch, y=humidity))
  
  # got negative humidity values, get those
  #low.humid <- filter(net, humidity <0)
  #group_by(low.humid, nodeid)%>%summarise(n=n())
  
  # 3 nodes that look erroneous, get node 78 first
  #node.78 <- filter(net, nodeid==78)
  
  # plot other variables for node 78
  #ggplot(node.78)+geom_point(aes(x=epoch, y=humidity))
  #ggplot(node.78)+geom_point(aes(x=epoch, y=humid_temp)) 
  #ggplot(node.78)+geom_point(aes(x=epoch, y=hamatop))
  #ggplot(node.78)+geom_point(aes(x=epoch, y=hamabot))
  
  # so it has relatively ok hamatop hamabot
  # but humidity and temperature values becomes weird at epoch > 3190, remove that
  net$humidity[net$nodeid == 78 & net$epoch >= 3190] <- NA
  net$humid_temp[net$nodeid == 78 & net$epoch >= 3190] <- NA
  net$humid_adj[net$nodeid == 78 & net$epoch >= 3190] <- NA
  
  # now check node 123
  #node.123 <-filter(net, nodeid==123)
  
  # plot other variables for 123
  #ggplot(node.123)+geom_point(aes(x=epoch, y=humidity)) 
  #ggplot(node.123)+geom_point(aes(x=epoch, y=humid_temp)) 
  #ggplot(node.123)+geom_point(aes(x=epoch, y=hamatop))
  #ggplot(node.123)+geom_point(aes(x=epoch, y=hamabot)) 
  
  # so it has relatively ok hamatop hamabot
  # but humidity and temperature values becomes weird at epoch > 4917, remove that
  net$humidity[net$nodeid == 123 & net$epoch >= 4917] <- NA
  net$humid_temp[net$nodeid == 123 & net$epoch >= 4917] <- NA
  net$humid_adj[net$nodeid == 123 & net$epoch >= 4917] <- NA
  
  # now check node 141
  #node.141 <-filter(net, nodeid==141)
  
  # plot other variables for 141
  #ggplot(node.141)+geom_point(aes(x=epoch, y=humidity)) 
  #ggplot(node.141)+geom_point(aes(x=epoch, y=humid_temp)) 
  #ggplot(node.141)+geom_point(aes(x=epoch, y=hamatop)) 
  #ggplot(node.141)+geom_point(aes(x=epoch, y=hamabot)) 
  
  # so it has relatively ok hamatop hamabot
  # but humidity and temperature values becomes weird at epoch > 9001, remove that
  net$humidity[net$nodeid == 141 & net$epoch >= 9001] <- NA
  net$humid_temp[net$nodeid == 141 & net$epoch >= 9001] <- NA
  net$humid_adj[net$nodeid == 141 & net$epoch >= 9001] <- NA
  
  # double check to see that it's clean
  #ggplot(net)+geom_point(aes(x=epoch, y=humidity))
  
  #####
  
  # now check for temperature time series
  #ggplot(net)+geom_point(aes(x=epoch, y=humid_temp))
  
  # outlier at temp > 50? who are those?
  #low.humid_temp <- filter(net, humid_temp >50)
  #group_by(low.humid_temp, nodeid)%>%summarise(n=n())
  
  # check node 145
  #node.145 <- filter(net, nodeid==145)
  
  # plot other variables for 145
  #ggplot(node.145)+geom_point(aes(x=epoch, y=humidity))
  #ggplot(node.145)+geom_point(aes(x=epoch, y=humid_temp))
  #ggplot(node.145)+geom_point(aes(x=epoch, y=hamatop))
  #ggplot(node.145)+geom_point(aes(x=epoch, y=hamabot))
  
  # so it has relatively ok hamatop hamabot
  # but humidity and temperature values becomes weird at epoch > 3500, remove that
  net$humidity[net$nodeid == 145 & net$epoch >= 3500] <- NA
  net$humid_temp[net$nodeid == 145 & net$epoch >= 3500] <- NA
  net$humid_adj[net$nodeid == 145 & net$epoch >= 3500] <- NA
  
  # check node 3
  #node.3 <- filter(net, nodeid==3)
  
  # plot other variables for 3
  #ggplot(node.3)+geom_point(aes(x=epoch, y=humidity))
  #ggplot(node.3)+geom_point(aes(x=epoch, y=humid_temp))
  #ggplot(node.3)+geom_point(aes(x=epoch, y=hamatop))
  #ggplot(node.3)+geom_point(aes(x=epoch, y=hamabot))
  
  # so it has relatively ok hamatop hamabot
  # but humidity and temperature values becomes weird at epoch > 3775, remove that
  net$humidity[net$nodeid == 3 & net$epoch >= 3775] <- NA
  net$humid_temp[net$nodeid == 3 & net$epoch >= 3775] <- NA
  net$humid_adj[net$nodeid == 3 & net$epoch >= 3775] <- NA
  
  # double check to see that it's clean
  #ggplot(net)+geom_point(aes(x=epoch, y=humid_temp))
  
  #####
  
  # plot hamatop, seems ok
  #ggplot(net)+geom_point(aes(x=epoch, y=hamatop))
  
  #####
  
  # plot hamabot, seems ok
  #ggplot(net)+geom_point(aes(x=epoch, y=hamabot))
  
  
  #############################################
  # now we combine the log and net data
  # phase 1: process the intersection data
  #############################################
  
  # remove result_time, it's not meaningful anyway since we have epoch
  net <- net %>% select(-result_time)
  log <- log %>% select(-result_time)
  
  # their intersections, to remove redundant data and perform proper scaling
  net$row.id <- 1:nrow(net)
  log$row.id <- 1:nrow(log)
  intersect.log.net <- inner_join(net,log, by=c("nodeid", "epoch"))
  
  # ok, there are points that have same nodeid and epoch, but different data? let's see
  # if ever there is a difference, we can get the average between these diff data
  
  #####
  
  # get the difference of humidities
  intersect.log.net <- mutate(intersect.log.net, humid.diff= (humidity.x-humidity.y)/humidity.x)
  select(filter(intersect.log.net, humid.diff != 0), humid.diff) # check % error
  
  # get the mean of the humidities
  try <- intersect.log.net %>% select(humidity.x, humidity.y)
  intersect.log.net <- mutate(intersect.log.net, humidity.x = rowMeans(try,na.rm=TRUE))
  
  # create just a single humidity column
  intersect.log.net <- select(intersect.log.net, -(humid.diff))
  intersect.log.net <- select(intersect.log.net, -(humidity.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="humidity.x"] <- "humidity"
  
  #####
  
  # get the difference of temperatures
  intersect.log.net <- mutate(intersect.log.net, temp.diff= (humid_temp.x-humid_temp.y)/humid_temp.x)
  select(filter(intersect.log.net, temp.diff != 0), temp.diff) # check % error
  
  # get the mean of the temperatures
  try <- intersect.log.net %>% select(humid_temp.x, humid_temp.y)
  intersect.log.net <- mutate(intersect.log.net, humid_temp.x = rowMeans(try,na.rm=TRUE))
  
  # create just a single temperature column
  intersect.log.net <- select(intersect.log.net, -(temp.diff))
  intersect.log.net <- select(intersect.log.net, -(humid_temp.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="humid_temp.x"] <- "humid_temp"
  
  #####
  
  # get the difference of humid_adj
  intersect.log.net <- mutate(intersect.log.net, humid_adj.diff= (humid_adj.x-humid_adj.y)/humid_adj.x)
  select(filter(intersect.log.net, humid_adj.diff != 0), humid_adj.diff) # check % error
  
  # get the mean of the humid_adjs
  try <- intersect.log.net %>% select(humid_adj.x, humid_adj.y)
  intersect.log.net <- mutate(intersect.log.net, humid_adj.x = rowMeans(try,na.rm=TRUE))
  
  # create just a single humid_adj column
  intersect.log.net <- select(intersect.log.net, -(humid_adj.diff))
  intersect.log.net <- select(intersect.log.net, -(humid_adj.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="humid_adj.x"] <- "humid_adj"
  
  #####
  
  # get the difference of hamatops
  intersect.log.net <- mutate(intersect.log.net, hamatop.diff= (hamatop.x-hamatop.y)/hamatop.x)
  select(filter(intersect.log.net, hamatop.diff != 0), hamatop.diff) # check % error
  
  # get the mean of the hamatops
  try <- intersect.log.net %>% select(hamatop.x, hamatop.y)
  intersect.log.net <- mutate(intersect.log.net, hamatop.x = rowMeans(try,na.rm=TRUE))
  
  # create just a single hamatop column
  intersect.log.net <- select(intersect.log.net, -(hamatop.diff))
  intersect.log.net <- select(intersect.log.net, -(hamatop.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="hamatop.x"] <- "hamatop"
  
  #####
  
  # get the difference of hamabots
  intersect.log.net <- mutate(intersect.log.net, hamabot.diff= (hamabot.x-hamabot.y)/hamabot.x)
  select(filter(intersect.log.net, hamabot.diff != 0), hamabot.diff) # check % error
  
  # get the mean of the hamabots
  try <- intersect.log.net %>% select(hamabot.x, hamabot.y)
  intersect.log.net <- mutate(intersect.log.net, hamabot.x = rowMeans(try,na.rm=TRUE))
  
  # create just a single hamabot column
  intersect.log.net <- select(intersect.log.net, -(hamabot.diff))
  intersect.log.net <- select(intersect.log.net, -(hamabot.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="hamabot.x"] <- "hamabot"
  
  #####
  
  # get the difference of depths
  intersect.log.net <- mutate(intersect.log.net, depth.diff= (depth.x-depth.y)/depth.x)
  select(filter(intersect.log.net, depth.diff != 0), depth.diff) # check % error
  
  # apparently it's all good, no need for averaging
  # create just a single depth column
  intersect.log.net <- select(intersect.log.net, -(depth.diff))
  intersect.log.net <- select(intersect.log.net, -(depth.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="depth.x"] <- "depth"
  
  #####
  
  # get the difference of parents
  intersect.log.net <- mutate(intersect.log.net, parent.diff= (parent.x-parent.y)/parent.x)
  select(filter(intersect.log.net, parent.diff != 0), parent.diff) # check % error
  
  # apparently it's all good, no need for averaging
  # create just a single parent column
  intersect.log.net <- select(intersect.log.net, -(parent.diff))
  intersect.log.net <- select(intersect.log.net, -(parent.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="parent.x"] <- "parent"
  
  #####
  
  # now for the voltage! create a scatter plot of the voltage values
  #ggplot(intersect.log.net)+geom_point(aes(x=voltage.y, y= voltage.x))
  
  # ok so there's almost a linear relationship, do a line fit so that we know the scaling factor
  regression <- summary(lm(voltage.x ~ voltage.y, intersect.log.net))
  
  # plot the fit line if you want
  #ggplot(intersect.log.net)+geom_point(aes(x=voltage.y, y= voltage.x))+geom_abline(aes(intercept=442.59, slope= -82.41 ),color="blue")
  
  # just get the scaled voltages, make it single column, we'll use the coefficients later
  intersect.log.net <- mutate(intersect.log.net, voltage.x = voltage.y)
  intersect.log.net <- select(intersect.log.net, -(voltage.y))
  colnames(intersect.log.net)[colnames(intersect.log.net)=="voltage.x"] <- "voltage"
  
  #############################################
  # now we combine the log and net data
  # phase 2: scale the non-intersection data and combine
  #############################################
  
  # get unique points for the net dataset
  colnames(intersect.log.net)[colnames(intersect.log.net)=="row.id.x"] <- "row.id"
  try <- left_join(net,intersect.log.net,by=c("nodeid","epoch","row.id", "parent","depth"))
  net.unique <- filter(try, is.na(row.id.y))
  net.unique <- select(net.unique, -(voltage.y), -(humidity.y), -(humid_temp.y), -(humid_adj.y), -(hamatop.y), -(hamabot.y), -(row.id.y))
  
  # get the unique points for the log dataset
  intersect.log.net <- select(intersect.log.net, -row.id)
  colnames(intersect.log.net)[colnames(intersect.log.net)=="row.id.y"] <- "row.id"
  intersect.log.net <- mutate(intersect.log.net, row.id.y = row.id)
  try <- left_join(log,intersect.log.net,by=c("nodeid","epoch", "row.id", "parent","depth"))
  log.unique <- filter(try, is.na(row.id.y))
  log.unique <- select(log.unique, -(voltage.y), -(humidity.y), -(humid_temp.y), -(humid_adj.y), -(hamatop.y), -(hamabot.y), -(row.id.y))
  
  # renaming of columns, is there a shorter way for this? :(
  intersect.log.net <- intersect.log.net %>% select(epoch,nodeid,parent,voltage,depth,humidity,humid_temp,humid_adj,hamatop,hamabot)
  colnames(net.unique)[colnames(net.unique)=="voltage.x"] <- "voltage"
  colnames(net.unique)[colnames(net.unique)=="humidity.x"] <- "humidity"
  colnames(net.unique)[colnames(net.unique)=="humid_temp.x"] <- "humid_temp"
  colnames(net.unique)[colnames(net.unique)=="humid_adj.x"] <- "humid_adj"
  colnames(net.unique)[colnames(net.unique)=="hamatop.x"] <- "hamatop"
  colnames(net.unique)[colnames(net.unique)=="hamabot.x"] <- "hamabot"
  net.unique <- net.unique %>% select(epoch,nodeid,parent,voltage,depth,humidity,humid_temp,humid_adj,hamatop,hamabot)
  colnames(log.unique)[colnames(log.unique)=="voltage.x"] <- "voltage"
  colnames(log.unique)[colnames(log.unique)=="humidity.x"] <- "humidity"
  colnames(log.unique)[colnames(log.unique)=="humid_temp.x"] <- "humid_temp"
  colnames(log.unique)[colnames(log.unique)=="humid_adj.x"] <- "humid_adj"
  colnames(log.unique)[colnames(log.unique)=="hamatop.x"] <- "hamatop"
  colnames(log.unique)[colnames(log.unique)=="hamabot.x"] <- "hamabot"
  log.unique <- log.unique %>% select(epoch,nodeid,parent,voltage,depth,humidity,humid_temp,humid_adj,hamatop,hamabot)
  
  # here's the fun part, scale the net voltage using the line fit coefficients computer earlier
  net.unique$voltage <- (442.59 - net.unique$voltage)/82.41
  
  # final merging of the data
  final <- rbind(net.unique,log.unique,intersect.log.net)
  
  return(final)
}