# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(data.table)
data <- fread("activity.csv")
data$date <- as.Date(data$date)
```

Summary information of the dataset:

```r
str(data)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
summary(data)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

Compute the total number of steps takend each day and draw the histogram:

```r
dailySteps <- tapply(data$steps, as.factor(data$date), FUN=sum, na.rm=TRUE)
hist(dailySteps, xlab="Number of steps", main="Histogram of total number of steps taken per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Compute the mean and median total number of steps taken per day:

```r
mean(dailySteps)
```

```
## [1] 9354
```

```r
median(dailySteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Compute the averaged total number of steps taken for each 5-minute interval and plot the time series graph:

```r
interSteps <- tapply(data$steps, as.factor(data$interval), mean, na.rm=TRUE)
plot(names(interSteps), interSteps, type="l", xlab="The 5-minute intervals", ylab="Averaged total number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

The 5-minute interval that on average across all the days in the dataset contains the maximum number of steps is:

```r
which.max(interSteps)
```

```
## 835 
## 104
```
Hence the "835" 5-minute interval contains the maximum number of averaged steps.

## Imputing missing values

Calculate and report the total number of missing values in the dataset:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
sum(is.na(data$date))
```

```
## [1] 0
```

```r
sum(is.na(data$interval))
```

```
## [1] 0
```
From the above, only the "steps" have NAs and totally there are 2304 rows with NAs.  


We will fill those NAs with the mean of that day, which is done by following steps:

```r
idx <- is.na(data$steps)
x <- data.frame(data[idx])
y <- data.frame(as.numeric(dailySteps), as.Date(names(dailySteps)))
names(y) <- c("avg", "date")
z <- merge(x, y, all.x=TRUE)
```

Create the new dataset:

```r
newData <- data
newData[idx]$steps <- z$avg
```

```
## Warning: Coerced 'double' RHS to 'integer' to match the column's type; may
## have truncated precision. Either change the target column to 'double'
## first (by creating a new 'double' vector length 17568 (nrows of entire
## table) and assign that; i.e. 'replace' column), or coerce RHS to 'integer'
## (e.g. 1L, NA_[real|integer]_, as.*, etc) to make your intent clear and for
## speed. Or, set the column type correctly up front when you create the
## table and stick to it, please.
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
dailySteps <- tapply(newData$steps, as.factor(newData$date), FUN=sum, na.rm=TRUE)
hist(dailySteps, xlab="Number of steps", main="Histogram of total number of steps taken per day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(dailySteps)
```

```
## [1] 9354
```

```r
median(dailySteps)
```

```
## [1] 10395
```
The values are the same as the original data. Imputing missing data on the estimates of the total daily number of steps may hidden the truth but may not affect the mean and median estimations.


## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
day <- weekdays(newData$date)
day <- gsub("(Saturday|Sunday)", "weekend", day)
day <- gsub("(Monday|Tuesday|Wednesday|Thursday|Friday)", "weekday", day)
newData$day <- as.factor(day)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
idx <- newData$day == "weekday"
wd <- tapply(newData[idx]$steps, as.factor(newData[idx]$interval), mean, na.rm=TRUE)
idx <- newData$day == "weekend"
we <- tapply(newData[idx]$steps, as.factor(newData[idx]$interval), mean, na.rm=TRUE)
par(mfrow=c(2,1))
plot(names(wd), wd, type="l", xlab="The 5-minute intervals", ylab="Averaged total number of steps", main="weekday")
plot(names(we), we, type="l", xlab="The 5-minute intervals", ylab="Averaged total number of steps", main="weekend")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

From the above plots, there are differences between patterns on weekdays and weekends.
