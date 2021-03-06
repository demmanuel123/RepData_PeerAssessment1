# Reproducible Research: Peer Assessment 1


### Loading and preprocessing the data
Loading the data and storing for future use.


```
## Warning: package 'dplyr' was built under R version 3.1.2
```

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

```
## Warning: package 'lubridate' was built under R version 3.1.2
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```
## Warning: package 'car' was built under R version 3.1.3
```

```r
activity_record <- read.csv("activity.csv",header=TRUE)
activity_record <- tbl_df(activity_record) 
```

### What is mean total number of steps taken per day?

Calculate the total number of steps taken per day. Depicting a histogram to show the total number of steps taken each day. 


```r
sum_daily_activity <- activity_record%>%
                      group_by(date) %>%
                      summarise(sum(steps,na.rm=TRUE))
names(sum_daily_activity)[2] <- "sumStepsPerDay"
```

Here is a histogram of the data

```r
hist(sum_daily_activity$sumStepsPerDay,xlab="Steps per day",breaks=30,col="lightblue",border="darkblue",main="")
```

![](PA1_template_files/figure-html/histogram-1.png) 
##Solution 
Mean of the total number of steps taken per day is 9354.23, Median of the total number of steps taken per day is , 1.0395\times 10^{4}

### What is the average daily activity pattern?

Making a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 
Make a plot by grouping based on intervals and then average number of steps per day. 
We find the interval where the average is maximum across all the days. 



```r
mean_interval <- activity_record%>%
                 group_by(interval) %>%
                 summarise(mean(steps,na.rm=TRUE))
names(mean_interval)[2] <- "averageStepsForInterval"
id = mean_interval[which(mean_interval$averageStepsForInterval== max(mean_interval$averageStepsForInterval)),]
```

```r
plot(mean_interval,type="l")
```

![](PA1_template_files/figure-html/IntervalPlot-1.png) 
##Solution 

The 5-minute Interval containing maximum number of steps on an average is ,835 , with a average value of,206.17. This is around the time 3 hours which could be a walk after lunch ! 

## Imputing missing values

Find the total number of values were steps are not recorded. We will replace the non recorded values with mean for the interval across all days. 
New dataset is created with replaced and old average of sums per day to compare the trend . 
A histogram is plotted with both the total number of steps,mean and median of steps taken each day before and after removing NA values . 



```r
numNAStepEntries <- nrow(activity_record[is.na(activity_record$steps),])
replicaActivityRecord <- activity_record
replacedNA <- merge(replicaActivityRecord,mean_interval)
replicaActivityRecord$steps[is.na(replicaActivityRecord$steps)] <- replacedNA$averageStepsForInterval[is.na(replacedNA$steps)]


new_sum_daily_activity <- replicaActivityRecord%>% group_by(date) %>% summarise(sum(steps,na.rm=TRUE))
names(new_sum_daily_activity)[2] <- "sumStepsPerDay"

new_sum_daily_activity <- mutate(new_sum_daily_activity,imputeNAtoMean=TRUE)
sum_daily_activity <- mutate(sum_daily_activity,imputeNAtoMean=FALSE)
oldandnew <- rbind(new_sum_daily_activity,sum_daily_activity)

newMeanStepsDailyActivity <- round(mean(new_sum_daily_activity$sumStepsPerDay,na.rm=TRUE),1)
newMedianStepsDailyActivity <- round(median(new_sum_daily_activity$sumStepsPerDay),1)
```
Here is a histogram of the comparison of the data


```r
ggplot(oldandnew, aes(sumStepsPerDay, fill = imputeNAtoMean,colour="red",alpha=.8)) + geom_histogram(alpha = 0.5, aes(y = ..density..,colour="lightblue"))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](PA1_template_files/figure-html/histogram2-1.png) 
##Solution

Total number of missing values in the dataset is,2304

After replacing the NA values with interval mean we get , Mean of the total number of steps taken per day is 1.07662\times 10^{4}, Median of the total number of steps taken per day is , 1.1015\times 10^{4}

The variations in the histogram is smoothened by replacing the NA values. The mean and median have shifted. The concentration is more to the center

### Are there differences in activity patterns between weekdays and weekends?

Weekday for teh given data is added as a column.
ggplot is plotted to find the patter for weekday and weekend. 

```r
tm_activity_record <- replacedNA %>% mutate(weekday=wday(as.POSIXlt(date),label=FALSE,abbr = TRUE))%>% arrange(date)
tm_activity_record$weekday <- recode(tm_activity_record$weekday,"2:6='weekday';c(1,7)='weekend'")
tm_activity_record <- tm_activity_record %>% group_by(weekday,interval) %>% summarise(mean(steps,na.rm=TRUE))
names(tm_activity_record)[3] <-"avgStepPerInterval"
```

Here is a panel plot of the data

```r
g <- ggplot(tm_activity_record,aes(x=tm_activity_record$interval,y=tm_activity_record$avgStepPerInterval))
g+ geom_line(aes(colour=tm_activity_record$steps)) + facet_grid(weekday~.) + labs(x=expression("Intervals"), y= expression("Average Steps")) + theme_grey() + labs(title ="Steps per Interval trend across weekday and weekend")
```

![](PA1_template_files/figure-html/WeekendWeekday-1.png) 


In weekdays we see stronger peaks, in weekend we see levelled activity across interval
