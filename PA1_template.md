# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
rowdata <- read.csv(unz(description="activity.zip", filename="activity.csv"), stringsAsFactors=F, colClasses=c(NA, "Date", NA))
str(rowdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(rowdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(rowdata)
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

```r
daytotal <- aggregate(steps ~ date, rowdata, sum)
hist(daytotal$steps, main="Total number steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The median of steps per day is  **10765** (code used to compute:`sprintf("%.0f", median(daytotal$steps))`)  
The mean of steps per day is    **10766** (code used to compute:`sprintf("%.0f", mean(daytotal$steps))`)  


## What is the average daily activity pattern?

```r
intervaltotals <- aggregate(steps ~ interval, rowdata, mean)
plot(intervaltotals, type="l", main="Average steps number via daytime")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Maximum interval is **835** (code used to compute:`intervaltotals[which(intervaltotals$steps==max(intervaltotals$steps)),1]`)


## Imputing missing values
Total number of missing values in the dataset is 2304 (code used to compute:`sum(is.na(rowdata$steps))`)

There are simple strategy for filling NA values:  
Take average for this interval for over all days.


```r
df <- data.frame(rowdata)
df <- merge(df, intervaltotals, by.x="interval", by.y="interval", all.x=T, all.y=F)
na_idx <- which(is.na(df$steps.x))
df[na_idx, "steps.x"] <- df[na_idx, "steps.y"]
df <- data.frame(df[, c("date", "interval", "steps.x")])
colnames(df) <- c("date", "interval", "steps")
daytotal_f <- aggregate(steps ~ date, df, sum)
hist(daytotal_f$steps, main="Total number steps per day for filled NA's")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The median of steps per day (with filled NA's) is  **10766** (code used to compute:`sprintf("%.0f", median(daytotal_f$steps))`)  
The mean of steps per day (with filled NA's) is    **10766** (code used to compute:`sprintf("%.0f", mean(daytotal_f$steps))`)  

It seems there are no influence of filling NA's to mean, medium and histogram.


## Are there differences in activity patterns between weekdays and weekends?

```r
df$cal_typen <- factor(is.element(weekdays(df$date), c("Sunday", "Saturday")), labels = c("weekday", "weekend"))
par(mfcol=c(2,1), mar=c(0, 4, 4, 2))
plot(aggregate(steps~interval, df[df$cal_type=='weekday', ], mean), type="l", xlab=NA, ylab="Weekdays", xaxt="n")
par(mar=c(5, 4, 0, 2))
plot(aggregate(steps~interval, df[df$cal_type=='weekend', ], mean), type="l", ylab="Weekends")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

It looks that weekends more 'flat' than weekdays (which have max aprox 9 am)
