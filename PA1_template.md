# Reproducible Research: Peer Assessment 1


```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data

Following code loads and transfers the data.

```r
d <- read.csv("F:\\r\\activity.csv")
dt <- tbl_df(d)
stepsTotal <- summarise(group_by(dt, date), steps = sum(steps))
stepsTotal <- summarise(group_by(dt, date), steps = sum(steps))
stepsMean <- summarise(group_by(dt, interval), steps = mean(steps, na.rm=T))
meanSteps <- round(mean(stepsTotal$steps, na.rm=TRUE),2)
medianSteps <- round(median(stepsTotal$steps, na.rm=TRUE),2)
maxSteps <- filter(stepsMean, steps == max(stepsMean$steps))
```

## What is mean total number of steps taken per day?

Following code draws the chart.

```r
par(mar = c(5, 4, 2, 2))
barplot(stepsTotal$steps, 
        names.arg = stepsTotal$date, 
        ylab = "Number of Steps", 
        xlab = "",
        ylim = c(0,max(stepsTotal$steps, na.rm=T) * 1.2),
        main = "Total Number of Steps Taken each Day", 
        col = "steelblue",
        cex.axis = .8,
        cex.names = .7,
        las = 2)
```

![](PA1_template_files/figure-html/average number of steps-1.png) 

Mean of total number of steps taken each day is 10766.19 steps.
The median is 10765 steps.

## What is the average daily activity pattern?

Following code draws the chart.

```r
par(mar = c(5, 4, 2, 2))
plot(stepsMean$interval, stepsMean$steps, 
        ylab = "Average Number of Steps", 
        xlab = "Minutes in a Day (5-minutes Interval)",
        ylim = c(0,c(max(stepsMean$steps) * 1.3)),
        main = "Average Daily Activity Pattern", 
        col = "steelblue",
        type = "l")
lines(c(min(stepsMean$interval),max(stepsMean$interval)), 
      c(mean(stepsMean$steps),mean(stepsMean$steps)), 
      type = "l", 
      xlab = "", 
      ylab = "", 
      col = "red") 
lines(c(mean(maxSteps$interval), mean(maxSteps$interval)), 
      c(-10, max(maxSteps$steps)), 
      type = "l", 
      xlab = "", 
      ylab = "", 
      col = "black")
text(mean(maxSteps$interval),
     max(maxSteps$steps), 
     labels = c(paste(as.character(round(max(maxSteps$steps))), " steps")))
legend(x = "topright", 
       legend = c("Average Number of Steps in Certain Time in a Day",
                  "Total Average Number of Steps"), 
       col=c("steelblue", "red"), 
       lwd = 1) 
```

![](PA1_template_files/figure-html/activity pattern-1.png) 

The 5-minute interval 835, on average across all the days in the dataset, contains the maximum number of steps 206. The point is marked in the chart as black vertical line.

## Imputing missing values

Following code calculates rows.

```r
missing <- filter(dt, is.na(steps))
mrows <- length(missing$steps)
trows <- length(dt$date)
```

The total number of rows with NA's is 2304 of 17568 (13.1 %).

The following dataset has filled those NA rows with average of same 5-minutes interval across the whole dataset.

The code for filling missing values.

```r
dtFix <- right_join(dt, stepsMean, by = "interval")
dtFix <- mutate(dtFix, steps = ifelse(is.na(steps.x), round(steps.y), round(steps.x)))
stepsTotalFix <- summarise(group_by(dtFix, date), steps = sum(steps))
stepsMeanFix <- summarise(group_by(dtFix, interval), steps = mean(steps))
meanStepsFix <- round(mean(stepsTotalFix$steps, na.rm=TRUE), 2)
medianStepsFix <- round(median(stepsTotalFix$steps, na.rm=TRUE), 2)
```

Following code draws the chart.

```r
par(mar = c(6, 6, 2, 2))
barplot(stepsTotalFix$steps, 
        names.arg = stepsTotalFix$date, 
        ylab = "Number of Steps", 
        xlab = "",
        ylim = c(0,max(stepsTotalFix$steps, na.rm=T) * 1.2),
        main = "Total Number of Steps Taken each Day (NAs populated with average)", 
        col = "steelblue",
        cex.axis = .8,
        cex.names = .7,
        las = 2)
```

![](PA1_template_files/figure-html/new average number of steps-1.png) 

Mean of total number of steps taken each day is 10765.64 steps (change -0.55 steps).
The median is 10762 steps (change -3 steps).

## Are there differences in activity patterns between weekdays and weekends?

Following code fixes the data for weekdays.

```r
#Finnish day labels
weekdaynames <- c('maanantai', 'tiistai', 'keskiviikko', 'torstai', 'perjantai')
dtFix <- mutate(dtFix, 
               dayType = factor((weekdays(ymd(date)) %in% weekdaynames)+1L,
               levels=1:2,
               labels=c('weekend', 'weekday')))
stepsMeanWd <- summarise(group_by(dtFix, interval, dayType), steps = mean(steps))
```

Following code draws the chart.

```r
par(mfrow=c(1,2))
#Left-panel
plot((filter(stepsMeanWd, dayType == 'weekend'))$interval, 
     (filter(stepsMeanWd, dayType == 'weekend'))$steps, 
     type = "l", 
     col = "steelblue",
     xlab = "5-minutes Interval", 
     ylab = "Average Number of Steps",
     main = "Weekends")
#Right-panel
plot((filter(stepsMeanWd, dayType == 'weekday'))$interval, 
     (filter(stepsMeanWd, dayType == 'weekday'))$steps, 
     type = "l", 
     col = "steelblue",
     xlab = "5-minutes Interval", 
     ylab = "",
     main = "Weekdays")
```

![](PA1_template_files/figure-html/weekdays chart-1.png) 

