---
title: "assignment1"
output: html_document
---


```r
sessionInfo()
```

```
## R version 3.2.1 (2015-06-18)
## Platform: i386-w64-mingw32/i386 (32-bit)
## Running under: Windows 7 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=French_France.1252  LC_CTYPE=French_France.1252   
## [3] LC_MONETARY=French_France.1252 LC_NUMERIC=C                  
## [5] LC_TIME=French_France.1252    
## 
## attached base packages:
## [1] tcltk     stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] knitr_1.11      lattice_0.20-31 sqldf_0.4-10    RSQLite_1.0.0  
##  [5] DBI_0.3.1       gsubfn_0.6-6    proto_0.3-10    plyr_1.8.3     
##  [9] ggplot2_1.0.1   installr_0.17.0
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.0      magrittr_1.5     MASS_7.3-40      munsell_0.4.2   
##  [5] colorspace_1.2-6 highr_0.5        stringr_1.0.0    tools_3.2.1     
##  [9] grid_3.2.1       gtable_0.1.2     yaml_2.1.13      digest_0.6.8    
## [13] formatR_1.2      reshape2_1.4.1   evaluate_0.7.2   labeling_0.3    
## [17] stringi_0.5-5    scales_0.2.5     chron_2.3-47
```


```
## [1] "C:/Users/ephyto/Google Drive/coursera - Data Science Specialization/5 Reproducible Research/week 2 assignment/repdata-data-activity"
```

```r
activity <- read.csv("activity.csv")
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```
###Q1: what is mean total number of steps per day

create new dataset without na values

```r
nactivity <- na.omit(activity)
meanTotalSteps <- aggregate(nactivity$steps, list(nactivity$date), sum)
names(meanTotalSteps) <- c("date", "steps")
meanTotalSteps
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```
mean line in magenta, median line in blue, median=mean in our example

```r
lmts <- nrow(meanTotalSteps)
hist(meanTotalSteps$steps, breaks=lmts)
rug(meanTotalSteps$steps)
abline(v=mean(meanTotalSteps$steps), col="magenta", lwd=4)
abline(v=median(meanTotalSteps$steps), col="blue", lwd=2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean(meanTotalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(meanTotalSteps$steps)
```

```
## [1] 10765
```

###What is the average daily activity

```r
library(plyr)
#calculate average steps for each of 5-minute inteval during a 24-hour period
int.mean.steps <- ddply(nactivity, ~interval, summarise, mean=mean(steps))
```

```r
library(ggplot2)
qplot(x=interval, y=mean, data = int.mean.steps, geom = "line", xlab="5 minutes inteval", ylab="Number of Step Count", main="Average Number of Steps Across All Days"
)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Report 5-min interval, on average across all the days in the dataset, contains the maximum number of steps


```r
int.mean.steps[which.max(int.mean.steps$mean),]
```

```
##     interval     mean
## 104      835 206.1698
```

**Observations**

Based on steps taken pattern, the person's daily activity peaks around 8:35am

###Imputing missing values

Calculate and report the total number of missing values in the dataset


```r
library(sqldf)
```
```
tNA <- sqldf('SELECT d.* 
        FROM "activity" as d
        WHERE d.steps IS NULL 
        ORDER BY d.date, d.interval')
``


```r
#NROW(tNA)
```

Filling the missing values


```r
t1 <- sqldf('
        SELECT d.*, i.mean 
        FROM "int.mean.steps" as i 
        JOIN "activity" as d 
        ON d.interval = i.interval 
        ORDER BY d.date, d.interval ')

t1$steps[is.na(t1$steps)] <- t1$mean[is.na(t1$steps)]
```

Prepare data for plot histogram


```r
t1.total.steps <- as.integer( sqldf('
        SELECT sum(steps) 
        FROM t1') )
        
t1.total.steps.by.date <- sqldf('
        SELECT date, sum(steps) as "t1.total.steps.by.date" 
        FROM t1 GROUP BY date 
        ORDER BY date')
        
daily.61.steps <- sqldf('
        SELECT date, "t1.total.steps.by.date" as "steps" 
        FROM "t1.total.steps.by.date" 
        ORDER BY date')
```

Make an histogram of the total number of steps every day


```r
hist(daily.61.steps$steps,
        main=" ", 
        breaks=10, 
        xlab="After Imputate NA -Total Number Of Steps Taken Daily")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

Calculate and report the mean and median total number of steps taken per day


```r
t1.mean.steps.per.day <- as.integer(t1.total.steps / NROW(t1.total.steps.by.date))
t1.mean.steps.per.day
```

```
## [1] 10766
```


```r
t1.median.steps.per.day <- median(t1.total.steps.by.date$t1.total.steps.by.date)
t1.mean.steps.per.day
```

```
## [1] 10766
```
**Observation**

Do these values (mean and median) differ from the estimates from the first part of the assignment? Not really

What is the impact of imputing missing value on the estimates of the total daily number of steps? The shape of the histogram remains the same as the histogram from removed missing values. However, the frequency counts increased as expected.

###Are there differences in activity patterns between weekdays and weekends?

Use the dataset with the filled-in missing values for this part. Create a new factor variable with two levels -"weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make  a panel plot containing a time series plot (i.e. type="|") of the 5-minutes interval (x axis) and the average number of steps taken, averaged accross all weekday days or weekend days (y-axis)


```r
t1$date <- as.Date(t1$date)
t1$weektime <- as.factor(ifelse(weekdays(t1$date) %in%
        c("Saturday", "Sunday"), "weekend", "weekday"))
        
t5 <- sqldf('
        SELECT interval, avg(steps) as "mean.steps", weektime             FROM t1 
        GROUP BY weektime, interval 
        ORDER BY interval')
```


```r
library(lattice)
p <- xyplot(mean.steps ~ interval | factor(weektime), data=t5, 
        type = "|", 
        main = "Average Number of Steps Taken \n Averaged Across All Weekdays Days or Weekend Days", 
        xlab = "5-minutes Interval",
        ylab = "Average Number of steps taken"
        )
print(p)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 

