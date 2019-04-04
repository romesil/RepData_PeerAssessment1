---
title: "PA1_template"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Library readr, to be used:
```{r , echo=TRUE}

library(readr)
```
##Loading and preprocessing the data
Show any code that is needed to

Load the data (i.e. read.csv())

```{r , echo=TRUE}
activity <- read_csv("activity.csv")
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day.

Calculate and report the mean and median total number of steps taken per day


```{r , echo=TRUE}
StepsPerDay1 <- tapply(activity$steps, activity$date, sum, na.rm = TRUE)
hist(StepsPerDay1, xlab = "Number of Steps", main = "Histogram: Total Daily Steps")

mean(StepsPerDay1, na.rm = TRUE)
# mean is 9354.23

median(StepsPerDay1, na.rm = TRUE)
# median is 10395
```

##What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r , echo=TRUE}
StepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(as.numeric(names(StepsPerInterval)), 
     StepsPerInterval, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Average Daily Activity Pattern", 
     type = "l")
```
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r , echo=TRUE}
maxInterval <- names(sort(StepsPerInterval, decreasing = TRUE)[1])
maxSteps <- sort(StepsPerInterval, decreasing = TRUE)[1]
```
The Interval 835 on average contains max number of steps: 206

#Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

# 1) Missing Values:
```{r , echo=TRUE}
sum(is.na(activity$steps))
#There are 2304 values missing in the dataset
```
# 2) Strategy for filling the missing values: using the mean
```{r , echo=TRUE}
StepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
# split activity data by interval
activity.split <- split(activity, activity$interval)
# fill in missing data for each interval
for(i in 1:length(activity.split)){
  activity.split[[i]]$steps[is.na(activity.split[[i]]$steps)] <- StepsPerInterval[i]
}
activity.filled <- do.call("rbind", activity.split)
activity.filled <- activity.filled[order(activity.filled$date) ,]
```
# 3) Histogram of the total number of daily steps, calculating mean and median
```{r , echo=TRUE}
StepsPerDay2 <- tapply(activity.filled$steps, activity.filled$date, sum)
hist(StepsPerDay2, xlab = "Number of Steps", main = "Histogram: Total Daily Steps")
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
mean(StepsPerDay2, na.rm = TRUE)
#10766.19 vs 9354.23, previously
median(StepsPerDay2, na.rm = TRUE)
#10766.19 vs 10395, previously
```
##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r , echo=TRUE}
activity.filled$day <- ifelse(weekdays(as.Date(activity.filled$date)) == "Saturday" | weekdays(as.Date(activity.filled$date)) == "Sunday", "weekend", "weekday")
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
# Calculate average steps per interval for weekends
StepsPerInterval.weekend <- tapply(activity.filled[activity.filled$day == "weekend" ,]$steps, activity.filled[activity.filled$day == "weekend" ,]$interval, mean, na.rm = TRUE)

# Calculate average steps per interval for weekdays
StepsPerInterval.weekday <- tapply(activity.filled[activity.filled$day == "weekday" ,]$steps, activity.filled[activity.filled$day == "weekday" ,]$interval, mean, na.rm = TRUE)

# Panel for 2 graphs
par(mfrow=c(1,2))

# weekday activity
plot(as.numeric(names(StepsPerInterval.weekday)), 
     StepsPerInterval.weekday, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Weekdays", 
     type = "l")

# weekend activity
plot(as.numeric(names(StepsPerInterval.weekend)), 
     StepsPerInterval.weekend, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Weekends", 
     type = "l")


```
