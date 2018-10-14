library(readxl)
library(dplyr)
library(lubridate)
library(timeDate)
library(zoo)
#library(tseries)
#library(forecast)
#library(ggplot2)

#read in well csv
well_orig <- read_excel("C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Final Project\\Well Data\\G-3549.xlsx", sheet = 3)
rain_orig <- read_excel("C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Final Project\\Well Data\\G-3549.xlsx", sheet = 1)
tide_orig <- read_excel("C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Final Project\\Well Data\\G-3549.xlsx", sheet = 2)

#roll-up to hourly data
#average well height by hour
well_hourly <- well_orig %>% 
  group_by(date(date), hour(time), tz_cd) %>% 
  summarise(well_ft = mean(Well_ft, na.rm = TRUE), 
            corrected = mean(Corrected, na.rm=TRUE)) %>%
  rename(hours = `hour(time)`, date=`date(date)`)

#combine date and hour columns
well_hourly$date <- as_datetime(well_hourly$date)
hour(well_hourly$date) <- well_hourly$hours

#adjust time for daylight savings
#create new column, adjust time, convert back to datetime
well_hourly$datetime_correct <- well_hourly$date
well_hourly$datetime_correct <- ifelse(well_hourly$tz_cd == "EDT", well_hourly$datetime_correct - hours(1), well_hourly$datetime_correct)
well_hourly$datetime_correct <- as_datetime(well_hourly$datetime_correct)

well_hourly <- well_hourly[,c(6,5)]

#sum rainfall by hour
rain_orig$Date <- rain_orig$Date + seconds(1) #fix seconds because off by 1 when read in from excel

rain_hourly <- rain_orig %>% 
  group_by(date(Date), hour(Date)) %>% 
  summarise(rain = sum(RAIN_FT, na.rm = FALSE)) %>%
  rename(hours = `hour(Date)`, datetime=`date(Date)`)

rain_hourly$datetime <- as_datetime(rain_hourly$datetime)
hour(rain_hourly$datetime) <- rain_hourly$hours
rain_hourly <- rain_hourly[,c(1,3)]


#tide data is already hourly, no need to roll up. just put column in similar format
#as well and rain
tide_hourly <- tide_orig
tide_hourly$datetime <- as_datetime(tide_hourly$Date)
tide_hourly$hour <- hour(tide_hourly$Time)
hour(tide_hourly$datetime) <- tide_hourly$hour
tide_hourly <- tide_hourly[,c(4,3)]



#Make time sequence to join in and make missing values where there are gaps
ts <- data.frame(timeSequence(from = "2007-10-01", to = "2018-06-13", by = "hours"))
colnames(ts) <- "time_series" 

#match well, tide and rain to time series
join <-  ts %>%
  left_join(well_hourly, by = c("time_series"= "datetime_correct")) %>%
  left_join(tide_hourly, by = c("time_series"= "datetime")) %>%
  left_join(rain_hourly, by = c("time_series"= "datetime")) 
  

#remove extra 2 hours at end of join
join <- join[-c(93792:93793),]

#only well data has missing values
#impute missing values
well <- join[,c(1,2)]

#create zoo object, impute, convert back to df, rename columns
well.z <- zoo(x = well$corrected, well$time_series) #Convert to zoo object to impute
well.z <- na.approx(well.z) #Impute the missing values
well <- fortify.zoo(well.z)
names(well)[1] <- "datetime"
names(well)[2] <- "height"

#join the imputed values with tide an rain level
join <-  join %>%
  left_join(well, by = c("time_series"= "datetime"))

colnames(join)[colnames(join)=="time_series"] <- "datetime"

#remove column of well data with missing values, and we now have clean data!
clean <- join[,c(1,5,3,4)]

#export to csv for analysis in SAS
write.csv(clean, "C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Final Project\\Well Data\\clean.csv")
