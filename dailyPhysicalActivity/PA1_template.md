
## Loading and preprocessing the data

```r
library(dplyr)
library(lattice)
activity <- read.csv("activity.csv")
activity <- mutate(activity, date = as.Date(date))
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
sumSteps <-activity %>%
           group_by(date) %>%
           summarise(dailyTotal=sum(steps,na.rm=FALSE))
hist(sumSteps$dailyTotal,
     col = "green",
     xlab = "Total Steps per Day",
     ylab = "Frequency",
     main = "Histogram of Total Steps per Day")         
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean(sumSteps$dailyTotal, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(sumSteps$dailyTotal, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanSteps <- activity %>%
           group_by(interval) %>%
           summarise(stepsInterval=mean(steps,na.rm=TRUE))
plot(meanSteps$interval, meanSteps$stepsInterval, 
     type = "l", 
     xlab = "5 Minute Interval", 
     ylab = "Average Steps", 
     main = "Average Daily Activity", 
     col = "green")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
with(meanSteps,interval[which.max(stepsInterval)])
```

```
## [1] 835
```
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Will fill in missing values using the average interval value across all days. 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityFilled<- inner_join(activity, meanSteps) %>%
                 mutate(steps=ifelse(is.na(steps),stepsInterval,steps)) %>%
                 select(-stepsInterval)
```

```
## Joining by: "interval"
```

```r
sum(is.na(activityFilled))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
activityFilled <- activityFilled %>% group_by(date) %>% summarise(dailyTotal=sum(steps))
hist(activityFilled$dailyTotal, 
     col = "green", 
     xlab = "TotalSteps Per Day", 
     ylab = "Frequency", 
     main = "Histogram of Total Steps Per Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(activityFilled$dailyTotal)
```

```
## [1] 10766.19
```

```r
median(activityFilled$dailyTotal)
```

```
## [1] 10766.19
```
The filled data is very similar to the original data. The histogram is of similar shape, and the mean and median are both close too. The filled data has a greated number of total steps because it has more observations. 

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
days <- weekdays(activity$date)
activity$day <- ifelse(days == "Saturday" | days == "Sunday", "Weekend", "Weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
activity<- activity %>% 
            group_by(day, interval) %>%
            summarise(meansteps=mean(steps,na.rm = TRUE))
with (activity, xyplot(meansteps ~ interval|day, 
                       type="l", 
                       ylab="Number of steps",
                       layout=c(1,2)))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
