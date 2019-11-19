---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
## Values for processing
data.dir <- "C:/Users/berna/Documents/R/Reproducible_Research/Course_Project_1"         ## Data directory name
data.file <- "activity.csv"

## Load needed libraries
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.6.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

## Set direcory
setwd(data.dir)
## Retrieve Data
StepsPerInterval.df <- read.csv(data.file, header = TRUE)
## Transform
StepsPerInterval.tbl <- as_tibble(StepsPerInterval.df)                                  ## Create as tibble
StepsPerInterval.tbl <- mutate(StepsPerInterval.tbl, date = as.Date(date))
```

## What is mean total number of steps taken per day?

```r
Out.tbl <- StepsPerInterval.tbl[!is.na(StepsPerInterval.tbl[, 1]), ]                    ## 1st remove days with NA values
Out.tbl <- Out.tbl %>% group_by(date) %>% summarize(SumSteps = sum(steps))              ## Get total steps per date

## Plot bar diagram of total steps versus date
ggplot(data = Out.tbl) +
        aes(x = date, y = SumSteps) +
        geom_histogram(stat = "identity") +                                             ## For non-density histogram
        labs(title = "Total Steps per Day", x = "Date", y = "Total Steps")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_Template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
paste("Mean steps per day: ", format(round(mean(Out.tbl$SumSteps, na.rm = TRUE), 0), nsmall = 0), sep = "")
```

```
## [1] "Mean steps per day: 10766"
```

```r
paste("Median steps per day: ", format(round(median(Out.tbl$SumSteps, na.rm = TRUE), 0), nsmall = 0), sep = "")
```

```
## [1] "Median steps per day: 10765"
```

## What is the average daily activity pattern?

```r
Out.tbl <- StepsPerInterval.tbl[!is.na(StepsPerInterval.tbl[, 1]), ]                    ## 1st remove days with NA values

## Output graphs of mean and median steps
par(mfrow = c(1, 2))                                                                    ## Graphs for mean & median
Out.tbl2 <- Out.tbl %>% group_by(interval) %>% summarize(MeanSteps = mean(steps))
with(Out.tbl2, plot(x = interval, y = MeanSteps, type = "l",                             ## Mean steps
                main = "Mean Steps per Interval", xlab = "Time Interval",
                ylab = "Mean Steps"))

Out.tbl2 <- Out.tbl %>% group_by(interval) %>% summarize(MedianSteps = median(steps))
with(Out.tbl2, plot(x = interval, y = MedianSteps, type = "l",                           ## Median steps
                main = "Median Steps per Interval", xlab = "Time Interval",
                ylab = "Median Steps"))
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
Out.tbl2 <- Out.tbl2[order(-Out.tbl2$MedianSteps), ]                                         ## Order by decreasing steps

paste("Maximum steps interval: ", format(round(mean(Out.tbl$interval, na.rm = TRUE), 0), nsmall = 0), sep = "")
```

```
## [1] "Maximum steps interval: 1178"
```

## Imputing missing values

```r
## Calculate missing rows
Out.tbl <- StepsPerInterval.tbl
NA_Rows <- subset(Out.tbl, is.na(steps), select = steps)
paste("Number of missing values : ", nrow(NA_Rows), sep = "")
```

```
## [1] "Number of missing values : 2304"
```

```r
## Calculate number of rows with missing (i.e. NA) values for mean and median steps
## First mean steps
Out.tbl2 <- StepsPerInterval.tbl
Out.tbl <- subset(StepsPerInterval.tbl, !is.na(steps), select = steps:interval)         ## Table excluding NAs
Out.tbl <- Out.tbl %>% group_by(interval) %>% summarize(MeanSteps = mean(steps))        ## Get mean steps by interval
for(i in 1:nrow(Out.tbl2)) {
        if(is.na(Out.tbl2$steps[i])) {                                                  ## Where steps value is NA
                int_index <- match(Out.tbl2$interval[i], Out.tbl$interval)              ## Find matching interval in mean
                Out.tbl2$steps[i] <- Out.tbl$MeanSteps[int_index]                       ## Change NA to mean for interval
        }
}

## Next median steps
Out.tbl3 <- StepsPerInterval.tbl                                                        ## Table including NAs
Out.tbl <- subset(StepsPerInterval.tbl, !is.na(steps), select = steps:interval)         ## Table excluding NAs
Out.tbl <- Out.tbl %>% group_by(interval) %>% summarize(MedianSteps = median(steps))    ## Get median steps by interval
for(i in 1:nrow(Out.tbl3)) {
        if(is.na(Out.tbl3$steps[i])) {                                                  ## Where steps value is NA
                int_index <- match(Out.tbl3$interval[i], Out.tbl$interval)              ## Find matching interval in med.
                Out.tbl3$steps[i] <- Out.tbl$MedianSteps[int_index]                     ## Change NA to med. for interval
        }
}

## Output graphs of mean and median steps
par(mfrow = c(1, 2))                                                                    ## Graphs for mean & median
Out.tbl2 <- Out.tbl2 %>% group_by(interval) %>% summarize(MeanSteps = mean(steps))
with(Out.tbl2, plot(x = interval, y = MeanSteps, type = "l",                             ## Mean steps
                main = "Mean Steps - Imputed Values", xlab = "Time Interval",
                ylab = "Mean Steps"))

Out.tbl3 <- Out.tbl3 %>% group_by(interval) %>% summarize(MedianSteps = median(steps))
with(Out.tbl3, plot(x = interval, y = MedianSteps, type = "l",                           ## Median steps
                main = "Median Steps - Imputed Values", xlab = "Time Interval",
                ylab = "Median Steps"))
```

![](PA1_Template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?


```r
Out.tbl <- StepsPerInterval.tbl[!is.na(StepsPerInterval.tbl[, 1]), ]                    ## Eliminate all NA values

## Find day of week and whether weekday or weekend
Out.tbl <- mutate(Out.tbl, 
                  day = weekdays(Out.tbl$date, abbreviate = TRUE),
                  weekday.factor = factor(day,
                         levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                         labels = c("weekday", "weekday", "weekday", "weekday",
                                    "weekday", "weekend", "weekend")))                
                                                      
Out.tbl <- Out.tbl %>% group_by(weekday.factor, interval) %>%                                
        summarize(MeanSteps = mean(steps))                                              ## Get mean per interval by
                                                                                        ## weekday and weekend

## Split into 2 tables: weekday and weekend, respectively
Out.tbl2 <- subset(Out.tbl, weekday.factor == "weekday")
Out.tbl3 <- subset(Out.tbl, weekday.factor == "weekend")

## Output graphs for weekday and weekend, respectively
par(mfrow = c(1, 2))
with(Out.tbl2, plot(x = interval, y = MeanSteps, type = "l",                             ## Mean steps: weekdays
                main = "Weekdays: Mean Steps", xlab = "Time Interval",
                ylab = "Mean Steps"))

with(Out.tbl3, plot(x = interval, y = MeanSteps, type = "l",                             ## Mean steps: weekends
                main = "Weekends: Mean Steps", xlab = "Time Interval",
                ylab = "Mean Steps"))
```

![](PA1_Template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
