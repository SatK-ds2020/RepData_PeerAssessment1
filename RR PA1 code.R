getwd()
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
unzip("activity.zip")
activitydata<-read.csv("activity.csv")
str(activitydata)
summary(activitydata)

1. #What is mean total number of steps taken per day?
stepsperday<-aggregate(steps ~ date,data = activitydata, FUN = sum,na.rm=TRUE)
str(stepsperday)
#plot histogram for steps per Day
hist(stepsperday$steps, xlab = "Steps per Day", main= "Total no. of steps taken per day",col="red")
summary(stepsperday)# this mention mean and median steps taken per Day
spdmean<-mean(stepsperday$steps, na.rm=TRUE) 
spdmean # mean steps per Day
spdmedian<-median(stepsperday$steps, na.rm=TRUE) 
spdmedian# median steps per Day
abline(v=spdmean,lwd=2,lty=5) # annotation to the plot

2. #What is the average daily activity pattern?

# create subset for average steps taken per interval
stepsperinterval<-aggregate(steps~interval,data=activitydata,FUN=mean,na.rm=TRUE)
str(stepsperinterval)
summary(stepsperinterval) 
# line Plot for steps per 5min-interval
#plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot(stepsperinterval$interval, stepsperinterval$steps, type = "l", col = "blue", xlab = "Intervals",
     ylab = "Total steps per interval", main = "Average number of steps per interval")

#2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
max_stepsperinterval<-max(stepsperinterval$steps)# max steps per interval
max_stepsperinterval
# which 5 min interval have steps==max steps
interval_maxsteps<- stepsperinterval$interval[which(stepsperinterval$steps == max_stepsperinterval)]
interval_maxsteps
abline(v=interval_maxsteps,lwd=2,lty=5,col="red")

3.#Imputing missing values

#total number of missing values in the dataset 
total_missing_values<-sum(is.na(activitydata))
total_missing_values

#Create new dataset with the missing values filled in

# calculate mean steps per interval, we end up with a mean steps for all 288 intervals
MeanStepsPerInterval<- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
head (MeanStepsPerInterval,10)
# split the  activitydata in 2 parts (with and without NAs)
activitydata_na<- activitydata[is.na(activitydata$steps),]
head(activitydata_na,10)
activitydata_no_na<- activitydata[!is.na(activitydata$steps),]
head(activitydata_no_na,10)
#replace missing values in activitydata_na
activitydata_na$steps<- as.factor(activitydata_na$interval)
head(activitydata_na,10)
levels(activitydata_na$steps)<- MeanStepsPerInterval
head(activitydata_na,10)
#change the vector back as integer 
levels(activitydata_na$steps)<- round(as.numeric(levels(activitydata_na$steps)))
activitydata_na$steps <- as.integer(as.vector(activitydata_na$steps))
#merge/rbind the two datasets together and make a new activity dataset
new_activitydata <- rbind(activitydata_na, activitydata_no_na)
head(new_activitydata,10)


#Plotting parameters to place previous histogram and new one next to each other
par(mfrow = c(1,2))

#Plot again the histogram from the first part of the assignment
hist(stepsperday$steps, xlab = "Steps per Day", main = "Total steps per day with No NAs", col = "red")
abline(v=spdmean,lwd=2,lty=5)

#Plot new histogram, with imputed missing values
new_stepsperday <- aggregate(steps ~ date, data = new_activitydata, FUN = sum)
hist(new_stepsperday$steps, xlab = "Steps per Day", main = "Total steps per day with NA fill", col = "green")

# We calculate like previously the mean and median values, and store the new and old results 
# in a data frame for easier comparison:
new_mean_steps <- mean(new_stepsperday$steps)
new_mean_steps
new_median_steps <- median(new_stepsperday$steps)
new_median_steps
abline(v=new_mean_steps,lwd=2,lty=5)

4.# Create a new factor variable in the dataset with two levels -“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

#create a new variable weekday mentioning weekdays names
new_activitydata$weekday<-wday(as.Date(new_activitydata$date,"%Y-%m-%d"),label=TRUE,abbr = FALSE)
head(new_activitydata$weekday,10)
tail(new_activitydata$weekday,10)
head(new_activitydata,10)

# create a new variable DayType indicating Daytype(weekdays & weekends)
new_activitydata$DayType<-ifelse(new_activitydata$weekday=="Saturday"| new_activitydata$weekday=="Sunday", "weekend","weekday")
head(new_activitydata,10)

# create dataset with average steps per interval across weekdays or weekends
step_interval_DT<-aggregate(steps ~ interval + DayType, data = new_activitydata, FUN=mean)
head(step_interval_DT,10)

# plotting with ggplot2
qplot(interval,steps,data=step_interval_DT,geom = "line",xlab="Interval",
      ylab="Average Steps",main="Activity patterns between weekdays and weekends",facets = DayType ~.)
