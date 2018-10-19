# Time Series 2 Final Project
# Homework Team 2 Fall 2
# Modeling well elevation for well G-3549

#-----------------------------------------INSTALL PACKAGES-----------------------------------------------------------
install.packages(c('tidyverse','readxl','forecast','haven','fma','expsmooth','lmtest','zoo',
                   'seasonal','ggplot2', 'tseries', 'tsoutliers'))
library(tidyverse)
library(dplyr)
library(readxl)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(ggplot2)
library(lubridate)
library(tseries)
library(timeDate)
library(tsoutliers)
install.packages('DataCombine')
library(DataCombine)

#-------------------------------------------------------IMPORT DATA------------------------------------------------------
#read in well, rain, and tide data (csv)
well_orig <- read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Time Series 2/Homework/G-3549.xlsx", sheet = 3)
rain_orig <- read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Time Series 2/Homework/G-3549.xlsx", sheet = 1)
tide_orig <- read_excel("/Users/rogerdugas/Desktop/MSA NC State/1.1 Fall 2/Time Series 2/Homework/G-3549.xlsx", sheet = 2)

#-----------------------------------------------DATA CLEANING/TRANSFORMATION---------------------------------------------
#------------------------------------------------------Well Depth Data---------------------------------------------------
# Aggregate time series data to hourly data
well_hourly <- well_orig %>% 
  group_by(date(date), hour(time), tz_cd) %>% 
  summarise(well_ft = mean(Well_ft, na.rm = TRUE), 
            corrected = mean(Corrected, na.rm=TRUE)) %>%
  rename(hours = `hour(time)`, date=`date(date)`)

# Combine date and hour columns
well_hourly$date <- as_datetime(well_hourly$date)
hour(well_hourly$date) <- well_hourly$hours

# Adjust time for daylight savings
# Create new column, adjust time, convert back to datetime
well_hourly$datetime_correct <- well_hourly$date
well_hourly$datetime_correct <- ifelse(well_hourly$tz_cd == "EDT", well_hourly$datetime_correct - hours(1), well_hourly$datetime_correct)
well_hourly$datetime_correct <- as_datetime(well_hourly$datetime_correct)
well_hourly <- well_hourly[,c(6,5)]

#--------------------------------------------------------RAIN DATA-----------------------------------------------------
# Sum rainfall by hour
rain_orig$Date <- rain_orig$Date
rain_hourly <- rain_orig %>% 
  group_by(date(Date), hour(Date)) %>% 
  summarise(rain = sum(RAIN_FT, na.rm = FALSE)) %>%
  rename(hours = `hour(Date)`, datetime=`date(Date)`)

rain_hourly$datetime <- as_datetime(rain_hourly$datetime)
hour(rain_hourly$datetime) <- rain_hourly$hours
rain_hourly <- rain_hourly[,c(1,3)]

#---------------------------------------------------------TIDE DATA------------------------------------------------------
# Tide data is already hourly, no need to roll up. just put column in similar format as well and rain
tide_hourly <- tide_orig
tide_hourly$datetime <- as_datetime(tide_hourly$Date)
tide_hourly$hour <- hour(tide_hourly$Time)
hour(tide_hourly$datetime) <- tide_hourly$hour
tide_hourly <- tide_hourly[,c(4,3)]

#-------------------------------------------------------MISSING VALUES------------------------------------------------

# Make time sequence to join in and create placeholders for missing values where there are gaps in between hours
well_ts <- data.frame(timeSequence(from = "2007-10-01", to = "2018-06-13", by = "hours"))
colnames(well_ts) <- "time_series" 

#----------------------------------------------------------------------------------------------------------------------

# Join well_hourly to ts dataframe
join <- well_ts %>% 
  left_join(well_hourly, by = c("time_series" = "datetime_correct")) %>%
  left_join(tide_hourly, by = c("time_series"= "datetime")) %>%
  left_join(rain_hourly, by = c("time_series"= "datetime")) 

# Remove extra 2 hours at end of join
join <- join[-c(93792:93793),]
summary(join)

# Only well data has missing values
# Impute missing values
well <- join[,c(1,2)]
summary(well)

# Impute missing values using na.approx
# Create zoo object, impute, convert back to dataframe, then rename columns for better interpretation
well.z <- zoo(x = well$corrected, well$time_series) #Convert to zoo object to impute
well.z <- na.approx(well.z) #Impute the missing values
well <- fortify.zoo(well.z)
names(well)[1] <- "datetime"
names(well)[2] <- "height"

# Join the imputed values with tide an rain level
join <- join %>%
  left_join(well, by = c("time_series"= "datetime"))
colnames(join)[colnames(join)=="time_series"] <- "datetime"

# Remove column of well data with missing values, and we now have clean data
well <- join[,c(1,5,3,4)]

# We want to use 2 years of data.
# Create new df with data starting at June 6, 2016
well_short <- well[76271:93791,] # 2 years - 76271, 4 years - 58727, 3 years - 67487

# Check ccf for rain
ccf_plot <- ccf(well$height, well$rain, 20)
ccfvalues = ccf(well$height, well$rain)
ccfvalues
# The correlation is stronger starting at lag 0 up to lag ~30

# Check ccf for tide
ccf_plot <- ccf(well$height, well$Tide_ft, 20)
ccfvalues = ccf(well$height, well$Tide_ft)
ccfvalues
# The correlation is strong at lags 1-3 and 12 to 13 - but tide did not show as significant

# Create the predictor variable lags for Rain before creating the training and test datasets 
well_short <- slide(well_short, "rain", NewVar = "xLag0", slideBy = 0)
well_short <- slide(well_short, "rain", NewVar = "xLag1", slideBy = -1)
well_short <- slide(well_short, "rain", NewVar = "xLag2", slideBy = -2)
well_short <- slide(well_short, "rain", NewVar = "xLag3", slideBy = -3)
well_short <- slide(well_short, "rain", NewVar = "xLag4", slideBy = -4)
well_short <- slide(well_short, "rain", NewVar = "xLag5", slideBy = -5)
well_short <- slide(well_short, "rain", NewVar = "xLag6", slideBy = -6)
well_short <- slide(well_short, "rain", NewVar = "xLag7", slideBy = -7)
well_short <- slide(well_short, "rain", NewVar = "xLag8", slideBy = -8)
well_short <- slide(well_short, "rain", NewVar = "xLag9", slideBy = -9)
well_short <- slide(well_short, "rain", NewVar = "xLag10", slideBy = -10)
well_short <- slide(well_short, "rain", NewVar = "xLag11", slideBy = -11)
well_short <- slide(well_short, "rain", NewVar = "xLag12", slideBy = -12)
well_short <- slide(well_short, "rain", NewVar = "xLag13", slideBy = -13)
well_short <- slide(well_short, "rain", NewVar = "xLag14", slideBy = -14)
well_short <- slide(well_short, "rain", NewVar = "xLag15", slideBy = -15)
well_short <- slide(well_short, "rain", NewVar = "xLag16", slideBy = -16)
well_short <- slide(well_short, "rain", NewVar = "xLag17", slideBy = -17)
well_short <- slide(well_short, "rain", NewVar = "xLag18", slideBy = -18)
well_short <- slide(well_short, "rain", NewVar = "xLag19", slideBy = -19)
well_short <- slide(well_short, "rain", NewVar = "xLag20", slideBy = -20)
well_short <- slide(well_short, "rain", NewVar = "xLag21", slideBy = -21)
well_short <- slide(well_short, "rain", NewVar = "xLag22", slideBy = -22)
well_short <- slide(well_short, "rain", NewVar = "xLag23", slideBy = -23)
well_short <- slide(well_short, "rain", NewVar = "xLag24", slideBy = -24)
well_short <- slide(well_short, "rain", NewVar = "xLag25", slideBy = -25)
well_short <- slide(well_short, "rain", NewVar = "xLag26", slideBy = -26)
well_short <- slide(well_short, "rain", NewVar = "xLag27", slideBy = -27)
well_short <- slide(well_short, "rain", NewVar = "xLag28", slideBy = -28)
well_short <- slide(well_short, "rain", NewVar = "xLag29", slideBy = -29)
well_short <- slide(well_short, "rain", NewVar = "xLag30", slideBy = -30)
well_short <- slide(well_short, "rain", NewVar = "xLag31", slideBy = -31)
well_short <- slide(well_short, "rain", NewVar = "xLag32", slideBy = -32)
well_short <- slide(well_short, "rain", NewVar = "xLag33", slideBy = -33)
well_short <- slide(well_short, "rain", NewVar = "xLag34", slideBy = -34)
well_short <- slide(well_short, "rain", NewVar = "xLag35", slideBy = -35)
well_short <- slide(well_short, "rain", NewVar = "xLag36", slideBy = -36)
well_short <- slide(well_short, "rain", NewVar = "xLag37", slideBy = -37)
well_short <- slide(well_short, "rain", NewVar = "xLag38", slideBy = -38)
well_short <- slide(well_short, "rain", NewVar = "xLag39", slideBy = -39)
well_short <- slide(well_short, "rain", NewVar = "xLag40", slideBy = -40)
well_short <- slide(well_short, "rain", NewVar = "xLag41", slideBy = -41)
well_short <- slide(well_short, "rain", NewVar = "xLag42", slideBy = -42)
well_short <- slide(well_short, "rain", NewVar = "xLag43", slideBy = -43)
well_short <- slide(well_short, "rain", NewVar = "xLag44", slideBy = -44)
well_short <- slide(well_short, "rain", NewVar = "xLag45", slideBy = -45)

# Create time series object
well_short.ts <- ts(well_short$height,start = c(0), frequency=8766)

# Matrix for tide and rain
tide_rain <- well_short[1:(17521-169),]
tide_rain_test <- well_short[(17521-168):17521,]

# Split into training and validation data sets
train <- subset(well_short.ts, end = length(well_short.ts) - 169)
test <- subset(well_short.ts, start = length(well_short.ts) - 168)

# Plot the time series data of our training set (2 years)
plot(well_short.ts, xlab = '# of Years starting at June 6 2016', ylab= "Well Depth (ft)", main="Well G-3549")
# It is clear that we have an Additive Outlier (AO) which represents an isolated spike in 2017

# Look at last protion of training with test set 
plot(c(train[17000:17352],test))

# Nicer looking plot of time series data
autoplot(train) +
  ggtitle("Well G-3549") +
  ylab("Well Depth (ft)") +
  xlab("# of years starting from June 12, 2016") +
  scale_x_continuous(breaks=seq(0,2,.5)) +
  scale_y_continuous(breaks=seq(0,2.5,.5)) 

# Check for multiple seasonal components (check to see if we have daily seasonal component and weekly)
# This shows us that we only have a weekly seasonally component in our hourly time series data set 
well.msts <- msts(train, start=1, seasonal.periods = c(168,4383))
well.msts %>% mstl() %>%
  autoplot() + xlab("# of years starting from June 12, 2016")
# The mstl is showing that we do have a semi-annual and annual seasonal component in the well depth data, however it
# it does not have a significant impact in our time series data. So we will continue the analysis assuming there is 
# no seasonality in the well depth data

# Check ACF and PACF plots 
ggAcf(train, main="", lag.max = 168)
ggPacf(train, main="", lag.max = 168)

# Run the Augmented Dickey Fuller test to check if it is deterministic or stochastic
adf.test(train, alternative = "stationary", k=0)
# Fail to reject null, we have non-stationary data and will take differences of 1 

#--------------------------------------------MODEL BUILDING----------------------------------------------

# ***********WARNING***************
# BEFORE RUNNING MODELS, PLEASE NOTE THAT THE RUNNING PROCESS IS VERY LONG AND CAN TAKE UP TO 
# +30 MINTUES TO COMPLETE

# Check best combination of rain and tide after looking at the ccf plots 
fit5 <- Arima(train, order=c(20,1,20), xreg = tide_rain[,5:38], method="ML")
summary(fit5)
coeftest(fit5)
fit4 <- Arima(train, order=c(13,1,13), xreg = tide_rain[,5:46], method="ML")
summary(fit4)
coeftest(fit4)
fit3 <- Arima(train, order=c(16,1,16), xreg = tide_rain[,5:50], method="ML") # ***Chosen model***
summary(fit3)
coeftest(fit3)
fit2 <- Arima(train, order=c(14,1,14), xreg = tide_rain[,3:4], method="ML")
summary(fit2)
coeftest(fit2)

#----------------------------------------------------MODEL SELECTED/RESULTS-----------------------------------------
# Final Selected Model based on AIC values and AR and MA terms, and number of lags for each predictor variable
fit3 <- Arima(train, order=c(16,1,16), xreg = tide_rain[,5:50], method="ML") 
summary(fit3)
coeftest(fit3)

# Check ACF and PACF of Residuals 
ggAcf(fit3$residuals, main="", lag.max = 150)
ggPacf(fit3$residuals, main="", lag.max = 150)

# Check white noise
White.LB <- rep(NA, 150)
for(i in 60:150){
  White.LB[i] <- Box.test(fit3$residuals, lag = i, type = "Ljung", fitdf = 32)$p.value
}
White.LB <- pmin(White.LB,0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.06))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Create QQ plot for residuals
qqnorm(fit3$residuals, pch = 1, frame = FALSE)
qqline(fit3$residuals, col = "steelblue", lwd = 2)

# Actual versus predicted
fc <- forecast(fit3,xreg = tide_rain_test[,5:50], h=168)
plot(as.numeric(test), ylim = c(-.4,.8))
points(as.numeric(fc$mean), col='orange')

# Graphical display of Actual vs predicted for well elevation with Rain and Tide as predictors
start2 = nrow(well_short)-168
end2 = nrow(well_short)
actual_2 <- well_short[start2:end2, ]
actual_2$pred = fc$mean

ggplot(actual_2, aes(actual_2$datetime)) +
  geom_line(aes(y=actual_2$height), size = 1) +
  geom_line(aes(y=as.numeric(actual_2$pred)), colour='orange', size=1) +
  geom_line(aes(y=fc$lower[,2]), colour='red') +
  geom_line(aes(y=fc$upper[,2]), colour='red') +
  ggtitle('Well G-3549 Actual VS Predicted Well Water Elevation')+
  ylim(-.4,1)+
  scale_colour_manual(values=c("black", "orange"))+
  labs(color = "Legend") +
  xlab("June 2018") +
  ylab("Well Height ft") 

#Calculate MAE and MAPE
error=test-fc$mean
error
MAE=mean(abs(error))
MAE # MAE of 3.7%
MAPE=mean(abs(error)/abs(test)) # MAPE of 10.5%  
MAPE
