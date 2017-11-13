---
title: "Reproducible Research: Project 1"
author: "Kiefer Maddex"
date: 'NOV 12, 2017'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(lattice)
library(dplyr)
```

## Load and process data
* Load the data with read.csv() and format the data for analysis.

```{r load data , cache=TRUE}
data <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
data$date <- as.Date(data$date, "%Y-%m-%d")
data <- as.data.frame(data)
```

### What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```{r total steps per day}
#Aggregate steps by date
maxsteps <- aggregate(data$steps ~ data$date, FUN=sum)
colnames(maxsteps)<- c("Date", "Steps")
head(maxsteps)
```


2. Make a histogram of the total number of steps taken each day

```{r histogram}
#Plot histogram
hist(x=maxsteps$Steps, col="gray", breaks=50, xlab="Daily Total Steps", ylab="Frequency",
     main="Daily Total Step Distribution (missing data omitted)")
```


3. Calculate and report the mean and median of the total number of steps taken per day
  
  Mean 
```{r mean}
mean_steps   <- as.integer(mean(maxsteps$Steps, na.rm=TRUE))
mean_steps
```

  Median
```{r median}
median_steps   <- as.integer(median(maxsteps$Steps, na.rm=TRUE))
median_steps
``` 



## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r activity pattern}
Avg_Int <- data %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarise(avg_steps = mean(steps))
g <- ggplot(Avg_Int, aes(x = interval , y = avg_steps)) 
g + geom_line() +   labs(title="Average Steps/Interval", x = "Interval", y = "# of steps")
```


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r max interval}
interval <- as.integer(Avg_Int[which.max(Avg_Int$avg_steps),][1])
maxsteps <- as.integer(Avg_Int[which.max(Avg_Int$avg_steps),][2])
```

The maximum number of steps is at interval 835 with 206 steps



## Imputing missing values


1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r Total NAs}
Total_NAs <- sum(is.na(data$steps))
Total_NAs
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r replace NAs}
Imputed <- merge(data, Avg_Int, by = "interval" , by.all = FALSE) 
Imputed$steps[is.na(Imputed$steps)] <- Imputed$avg_steps[is.na(Imputed$steps)]
Imputed <- Imputed[names(data)]
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r NA fixed}
head(Imputed)
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r histogram2}
Totalsteps <- Imputed%>% filter(!is.na(steps)) %>% group_by(date) %>% summarise( total_steps = sum(steps))
ggplot(Totalsteps, aes(x = total_steps)) + geom_histogram(fill = "grey" ,color = "black", binwidth = 1000) + 
  labs(title="Steps/Day (Histogram)", x = "# of Steps/Day", y = "# of days")
```

  Mean
```{r mean2}
steps_day_mean   <- as.integer(mean(Totalsteps$total_steps, na.rm=TRUE))
steps_day_mean
```
  
  Median
```{r median2}
steps_day_median <- as.integer(median(Totalsteps$total_steps, na.rm=TRUE))
steps_day_median
```



##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels 'weekday' and 'weekend' indicating whether a given date is a weekday or weekend day.

```{r week}
Imputed$weekday <- as.factor(ifelse(weekdays(Imputed$date) %in% 
                                      c("Sunday", "Saturday"),"Weekend","Weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r plot}
Avg_Interval <- Imputed  %>% group_by(weekday, interval) %>% summarise( avg_steps = mean(steps))
with( Avg_Interval, xyplot( avg_steps ~ interval | weekday, type = "l",layout = c(1,2), 
                                  main = "Interval Average Steps by weekday and weekends",
                                  xlab = "Interval", ylab = "Avg # of Steps"))
```

Weekday mornings are more active than weekends.




