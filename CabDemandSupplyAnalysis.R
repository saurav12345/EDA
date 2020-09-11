# Required libraries
          library(stringr)
          library(dplyr)
          library(cowplot)

# Reading data
          master <- read.csv("Uber Request Data.csv",stringsAsFactors = F)

# A glance at data
          str(master)
          summary(master)
          head(master)
          nrow(master)

# Taking backup of data
          master1 <- master

# Data cleaning and preparation
# Replacing "/" by "-" in request and drop timestamp columns
          master1$Request.timestamp <- str_replace_all(master1$Request.timestamp,"/","-")
          master1$Drop.timestamp <- str_replace_all(master1$Drop.timestamp,"/","-")
  
# Concatenating seconds in the request and drop timestamp columns
          master1$Request.timestamp <- paste(master1$Request.timestamp,":00", sep = "")
          master1$Drop.timestamp <- paste(master1$Drop.timestamp,":00", sep = "")
    
# Breaking request and drop timestamp columns into request.date, request.time and drop.date, drop.time 
# columns respectively
          master1 <- separate(master1, Request.timestamp, c("request.date","request.time"),sep = " ",
            remove=TRUE)
          master1 <- separate(master1, Drop.timestamp, c("drop.date","drop.time"),sep = " ",remove=TRUE)
    
# Extracting upto seconds in request.time and drop.time columns
          master1$request.time <- str_extract(master1$request.time, pattern = "[0-9]+:[0-9]+:[0-9]+")
          master1$drop.time <- str_extract(master1$drop.time, pattern = "[0-9]+:[0-9]+:[0-9]+")
  
# Concatenating correctly formatted time with date in a new column
          master1$Request.timestamp <- paste(master1$request.date,master1$request.time,sep=" ")
          master1$Drop.timestamp <- paste(master1$drop.date,master1$drop.time,sep=" ")
  
# Removing un-necessary columns
          cols.dont.want <- c("request.date", "request.time","drop.date","drop.time")
          master1 <- master1[, ! names(master1) %in% cols.dont.want, drop = F]

# Converting data to a suitable format
          master1$Request.timestamp <- as.POSIXct(master1$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
          master1$Drop.timestamp <- as.POSIXct(master1$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")
          master1$Pickup.point <- as.factor(master1$Pickup.point)
          master1$Status <- as.factor(master1$Status)
  
# Further extraction of data
# Calculating request and drop hours
          master1$Request_Hour <- format(master1$Request.timestamp,"%H")
          master1$Request_Hour <- as.numeric(master1$Request_Hour)
          master1$Drop_Hour <- format(master1$Drop.timestamp,"%H")
          master1$Drop_Hour <- as.numeric(master1$Drop_Hour)
    
# Calculating Request dates and request days
          master1$Request_Date <- format(master1$Request.timestamp,"%d-%m-%y")
          request_date <- factor(master1$Request_Date)
          master1$Request_Day <- weekdays(master1$Request.timestamp,abbreviate = TRUE)
          request_day <- factor(master1$Request_Day)
      
# Calculating trip duration
          master1$Trip_duration <- NA
          for(i in 1:nrow(master1))
          {
            if (!(is.na(master1$Drop_Hour[i]))) 
            {
              if (master1$Drop_Hour[i] > master1$Request_Hour[i]) 
              {
                master1$Trip_duration[i] <- (master1$Drop_Hour[i] - master1$Request_Hour[i])
              } 
              else 
              {
                if((master1$Request_Hour[i] - master1$Drop_Hour[i]) > 20)
                {
                  master1$Trip_duration[i] <- (24 - (master1$Request_Hour[i] - master1$Drop_Hour[i]))
                }
                else
                  master1$Trip_duration[i] <- (master1$Request_Hour[i] - master1$Drop_Hour[i])
              } 
            } 
          }
          master1$Trip_duration <- as.numeric(master1$Trip_duration)
    
# Diving day into 3 different time slots - "Early Morning", "Day Time" and "Late Evening"
          master1$Time_slot <- NA
          for(i in 1:nrow(master1))
          {
            if (master1$Request_Hour[i] >= 0 & master1$Request_Hour[i] <= 7) 
            {
              master1$Time_slot[i] <- "Early Morning"
            } 
            else if (master1$Request_Hour[i] >= 8 & master1$Request_Hour[i] <= 15) 
            {
              master1$Time_slot[i] <- "Day Time"
            } 
            else 
              master1$Time_slot[i] <- "Late Evening"
          }
          master1$Time_slot <- as.factor(master1$Time_slot)

# Viewing manipulated data at hand
          str(master1)
          summary(master1)
          head(master1)
          nrow(master1)
          View(master1)
  
# Analysing data
# Calculating mean trip duration w.r.t. time slots and pickup points
          master1 %>% group_by(Time_slot,Pickup.point) %>% summarise("mean trip duration" = 
            mean(Trip_duration,na.rm = TRUE))
          
# Calculating Pickup point wise, time slot wise, status wise request counts
          master1 %>% group_by(Pickup.point,Time_slot,Status) %>% 
            summarise("request count" = length(Request.id))

# Visualizing request statuses for different dates and days
          ggplot(data=master1, aes(request_date, fill = Status)) + stat_count()
          ggplot(data=master1, aes(request_day, fill = Status)) + stat_count()

# Plotting graphs
          plot1 <- ggplot(master1,aes(Pickup.point, fill = Status)) + stat_count()
          plot2 <- ggplot(master1,aes(Time_slot, fill = Status)) + stat_count()
          plot3 <- ggplot(master1,aes(Pickup.point, fill = Time_slot)) + stat_count()
          plot4 <- ggplot(master1,aes(Pickup.point,fill=factor(Trip_duration))) + stat_count()
          plot_grid(plot1, plot2, plot3,plot4)