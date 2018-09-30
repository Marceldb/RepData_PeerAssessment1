Introduction
------------

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data \[52K\] The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

-   date: The date on which the measurement was taken in YYYY-MM-DD format

-   interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Commit containing full submission
---------------------------------

### 1 Dataset reading and pre-processing

``` r
library(readr)

activity<-read.csv("activity.csv",  header = TRUE)
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
summary(activity)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

NAs are in steps variable only. 2304 occurences, about 13% of total observations. As by specifications, we ignore NAs for the moment-

We need to transform (preprocess) the date variable from factor to date for later use

``` r
activity$date<-as.Date(activity$date)
```

2 What is the average number of steps taken per day?
----------------------------------------------------

### 2.1 calculating daily means

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# tot by date
activity_bydate <-activity%>%
group_by(date) %>%
summarize(tot_steps = sum(steps))
```

### 2.2 Histogram of daily steps taken

``` r
library(ggplot2)

p2<-activity_bydate %>% 
        ggplot(aes(tot_steps)) + 
        geom_histogram(binwidth = 2500, colour="black", fill="red") +
        ggtitle("Steps by day")+
        xlab("Steps")+
        theme_bw()
p2
```

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_github/Plotting%20daily%20steps-1.png)

### 2.3 Calculating total mean and median

``` r
# mean and median total
totmean <- mean(activity_bydate$tot_steps, na.rm = T)
totmedian <- median(activity_bydate$tot_steps, na.rm = T)

print(totmean)
```

    ## [1] 10766.19

``` r
print(totmedian)
```

    ## [1] 10765

3 Average number of steps taken by interval
-------------------------------------------

### 3.1 Time series

``` r
# tot by interval
library(dplyr)
activity_byinterval <-activity%>%
group_by(interval) %>%
summarize(avg_steps = mean(steps, na.rm=T))
```

``` r
activity_byinterval %>% 
        ggplot(aes(interval, avg_steps))+
        geom_line()+
        theme_bw()+
        ggtitle("Avg steps taken daily, by 5 minutes intervals")
```

![](PA1_template_files/figure-markdown_github/Time%20series-1.png)

### 3.2 Interval with the maximum number of steps

``` r
max_interval<-activity_byinterval %>% 
        filter(avg_steps==max(avg_steps))
print(max_interval$interval)
```

    ## [1] 835

4 Missing data strategy
-----------------------

We have seen from the summary() function at point 1, that there are 2304 NAs in the steps field. WE can now correct them, inputing an average value by interval

``` r
activity_no_na<-activity %>% inner_join(activity_byinterval, by = "interval") 
activity_no_na$steps_nona<-activity_no_na$steps

activity_no_na<-activity_no_na %>%     mutate(steps_nona = case_when(is.na(steps)  ~ avg_steps,TRUE ~  as.double(steps_nona)))


summary(activity_no_na)
```

    ##      steps             date               interval        avg_steps      
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :  0.000  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:  2.486  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5   Median : 34.113  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Mean   : 37.383  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.: 52.835  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :206.170  
    ##  NA's   :2304                                                            
    ##    steps_nona    
    ##  Min.   :  0.00  
    ##  1st Qu.:  0.00  
    ##  Median :  0.00  
    ##  Mean   : 37.38  
    ##  3rd Qu.: 27.00  
    ##  Max.   :806.00  
    ## 

``` r
# 
# tot by date
activity_no_na_bydate <-activity_no_na%>%
        group_by(date) %>%
        summarize(tot_steps = sum(steps_nona))




p4<-activity_no_na_bydate %>% 
        ggplot(aes(tot_steps)) + 
        geom_histogram(binwidth = 2500, colour="black", fill="yellow") +
        ggtitle("Steps by day, NAs filled in")+
        xlab("Steps")+
        theme_bw()
p4
```

![](PA1_template_files/figure-markdown_github/histogram%20with%20no%20more%20NAs-1.png)

``` r
# mean and median total
totmean_no_na <- mean(activity_no_na_bydate$tot_steps, na.rm = T)
totmedian_no_na <- median(activity_no_na_bydate$tot_steps, na.rm = T)

totmean_no_na 
```

    ## [1] 10766.19

``` r
totmedian_no_na
```

    ## [1] 10766.19

5 Comparing weekdays and weekends
---------------------------------

``` r
activity_no_na$day_type <- as.factor(ifelse(weekdays(activity_no_na$date) %in% c("sabato", "domenica"), "weekend", "weekday"))


activity_no_na_byinterval <-activity_no_na%>%
        select(interval,day_type, steps_nona)%>%               group_by(interval,day_type) %>%
        summarize( avg_steps = mean(steps_nona))

activity_no_na_byinterval %>% 
        ggplot(aes(interval, avg_steps))+
        geom_line()+
        theme_bw()+
        ggtitle("Avg steps taken daily, by 5 minutes intervals") +
        facet_grid(~ day_type)
```

![](PA1_template_files/figure-markdown_github/Comparing%20weekdays%20and%20weekends-1.png)

Interestingly, peak of activities are higher during weekdays than during the weekend.

Possible causes, worthing further investigation could be: lifestyle, not wearing the device in certain occasions, errors in raw data, differences in type of activities taken
