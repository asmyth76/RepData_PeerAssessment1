---
title: "PA1_template"
author: "Abigail Smyth"
date: "March 17, 2019"
output: 
  html_document:
    keep_md: true
    self_contained: false
---

```{r include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(mice)
library(dplyr)
library(ggplot2)
```


## Reproducible Research - Course Assignment #1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

```{r}
data <- read.csv("activity.csv", header=TRUE, sep=",", colClasses=c("numeric","Date","numeric"))

data$FormattedDate <- as.Date(data$date,format="%Y/%m/%d")

#Add column for day information

uniqueDates <- unique(data$FormattedDate)

uniqueIntervals <- unique(data$interval)

```

## Histogram of the total number of steps taken each day

```{r}

# Get Steps per day

stepsSplit <- split(data$steps, data$date)
totalStepsPerDay <- sapply(stepsSplit,sum,na.rm=TRUE)

# Mean of total steps per day
meanx <- mean(totalStepsPerDay,na.rm=TRUE)

# Median of total steps per day
medianx <- median(totalStepsPerDay,na.rm=TRUE)

par(mar=c(4,7,2,1))
hist(totalStepsPerDay, col="grey", main="Total number of steps taken each day",xlab="Steps",breaks=8)

lines(c(meanx,meanx), c(0,14), col="red", lwd=2)
text(meanx, 15, "Mean", col="red", adj=c(1,0))
text(meanx, 14, round(meanx,0), col="red", adj=c(1,0))

lines(c(medianx,medianx), c(0,14), col="blue", lwd=2)
text(medianx, 15, "Median", col="blue", adj=c(0,0))
text(medianx, 14, round(medianx,0), col="blue", adj=c(0,0))


```

## Mean and median number of steps taken each day

```{r}
# Get mean per day
meanSteps <- sapply(stepsSplit, mean, na.rm=TRUE)
meanDF <- data.frame(date=uniqueDates, meanSteps=meanSteps, row.names=NULL)
meanDF

# Get median per day
medianSteps <- sapply(stepsSplit, median, na.rm=TRUE)
medianDF <- data.frame(date=uniqueDates, medianSteps=medianSteps, row.names=NULL)
medianDF
```
## Time series plot of the average number of steps taken (using time intervals)

```{r}

# Get Steps interval

stepsSplitperInterval <- split(data$steps, data$interval)
avgStepsPerInterval <- sapply(stepsSplitperInterval,mean,na.rm=TRUE)

par(mar=c(4,7,2,1))
plot(uniqueIntervals,avgStepsPerInterval, main="Avg number of steps taken per interval", type="l", xlab="Interval", ylab="Avg # Steps", col="orange")

#Add line with the max avg

maxAvg <- uniqueIntervals[which.max(avgStepsPerInterval)]
abline(v=maxAvg, col="red", lwd=2)

```


## The 5-minute interval that, on average, contains the maximum number of steps

```{r}

maxAvg

```

## Imputing Missing Values

```{r}

# Replace NAs using MICE package Predictive Mean Matching (PMM)

# Create new data set 
data2 <- data

## Number of NAs before impute

# Determine how many NAs
md.pattern(data2)

tempData <- mice(data2,meth='pmm')

#Select 2nd of 5 complete data set (new data set)
completeData <- complete(tempData,2)

#Make sure there are no NAs
md.pattern(completeData)

#Histogram, mean and median

# Get Steps per day

stepsSplitNew <- split(completeData$steps, completeData$date)
totalStepsPerDayNew <- sapply(stepsSplitNew,sum,na.rm=TRUE)

# Mean of total steps per day
meany <- mean(totalStepsPerDayNew,na.rm=TRUE)

# Median of total steps per day
mediany <- median(totalStepsPerDayNew,na.rm=TRUE)

par(mar=c(4,7,2,1))
hist(totalStepsPerDayNew, col="grey", main="Total number of steps taken each day",xlab="Steps",breaks=8)

lines(c(meany,meany), c(0,14), col="red", lwd=2)
text(meany, 15, "Mean", col="red", adj=c(1,0))
text(meany, 14, round(meany,0), col="red", adj=c(1,0))

lines(c(mediany,mediany), c(0,14), col="blue", lwd=2)
text(mediany, 15, "Median", col="blue", adj=c(0,0))
text(mediany, 14, round(mediany,0), col="blue", adj=c(0,0))


# Get mean per day
meanStepsNew <- sapply(stepsSplitNew, mean, na.rm=TRUE)
meanDFNew <- data.frame(date=uniqueDates, meanStepsNew=meanStepsNew, row.names=NULL)
meanDFNew

# Get median per day
medianStepsNew <- sapply(stepsSplitNew, median, na.rm=TRUE)
medianDFNew <- data.frame(date=uniqueDates, medianStepsNew=medianStepsNew, row.names=NULL)
medianDFNew

## Answer: Using the imputed values the median and mean are closer in value

```

## Are there differences in activity patterns between weekdays and weekends?  Make a panel plot with the comparison.

```{r}

# Create new factor variable in dataset denoting weekend or weekday


completeData$WeekendorWeekday <- ifelse(as.POSIXlt(completeData$date)$wday %in% c(0,6), 'weekend', 'weekday')

completeDataAvgStepsPerInterval <- aggregate(steps ~ interval + WeekendorWeekday, data=completeData, mean)

#Create a panel plot showing weekend and weekday activity

ggplot(data=completeDataAvgStepsPerInterval,aes(interval, steps, colour=WeekendorWeekday)) + geom_line() + facet_grid(WeekendorWeekday ~ .) + labs(title="Avg Step Activity per Interval", x="Interval", y="Avg Steps")


```

## Weekdays show a higher amount of activity in the morning and decreases throughout the day; Weekends show a sustained amount of activity throuhout the morning and afternoon.
