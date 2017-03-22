---
title: "Reproducible Research: Peer Assessment 1"
author: "Karlin"
date: "March 12, 2017"
output: html_document
keep_md: true
---


## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This report makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## R Packages 

The following R packages are used in this report for data processing and analysis:

- ggplot2
- dplyr



```r
#Checks if ggplot2 is installed and if not installs it
list.of.packages <- c("ggplot2")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

#Loads ggplot2
library(ggplot2)


#Checks if dplyr is installed and if not installs it
list.of.packages <- c("dplyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

#Loads dplyr
library(dplyr)


#Checks if sqldf is installed and if not installs it
list.of.packages <- c("sqldf")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

#Loads sqldf
library(sqldf)
```

## Loading and preprocessing the data


####Accessing the Data Set
The data used in this report can be downloaded here: [Data Set](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

####Variabels in the Data Set
The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- **date**: The date on which the measurement was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken  

####R Code for Downloading and Processing the Data Set
The following code will download, unzip, and load the data into a data frame titled DF:


```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","DF.zip")

unzip("DF.zip")

DF <- read.csv("activity.csv", header = TRUE, na.strings = "NA")
```


## What is mean total number of steps taken per day?

To get familiar with the dataset we will first plot the distribution of the total steps taken per day.  The mean and median total steps taken per day will be indicated on the histogram.



```r
# Sum the steps by day and store in data frame titled "aggStepsByDay"
aggStepsByDay <- DF %>%
        group_by(date) %>%
        summarize(stepsPerDay = sum(steps)) %>%
        select(date, stepsPerDay)

# Plot the distribution of total steps per day and mark the mean and median with a line

M <- as.numeric(format(round(mean(aggStepsByDay$stepsPerDay, na.rm = TRUE), 2), nsmall = 2))

m <- median(aggStepsByDay$stepsPerDay, na.rm = TRUE)

a <- ggplot(aggStepsByDay, aes(stepsPerDay))

a + geom_histogram() +
        xlab("Steps Per Day") +
        ylab("Count") +
        ggtitle("Distribution of Total Steps Per Day") +
        geom_vline(aes(xintercept = M, color = "Blue"), size = 2 , show.legend =TRUE) +
        geom_vline(aes(xintercept = m, color = "Red"), size = 1 , show.legend =TRUE) +
        scale_colour_manual(name='', labels=c(paste('Mean = ', M), paste('Median = ', m)), values = c("Blue", "Red")) +
        theme(legend.position="top")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk HistStepsPerDay](figure/HistStepsPerDay-1.png)



## What is the average daily activity pattern?

Next we will get an idea of what a typical daily activity pattern looks like through plotting a time series of the average number of steps per 5 minute interval.


```r
# Average the steps per interval and store in data frame titled "avStepsByInterval"
avStepsByInterval <- DF %>%
        group_by(interval) %>%
        summarize(avStepsPerInt = mean(steps, na.rm = TRUE)) %>%
        select(interval, avStepsPerInt)

# Plot the average number of steps per interval as a time series

MxStp <- max(avStepsByInterval$avStepsPerInt)
     
MxStpF <- as.numeric(format(round(MxStp, 2), nsmall = 2))

MxInt <- avStepsByInterval$interval[avStepsByInterval$avStepsPerInt == MxStp]

a <- ggplot(avStepsByInterval, aes(interval, avStepsPerInt))

a + geom_line() +
        xlab("Interval") +
        ylab("Average Steps") +
        ggtitle("Average Steps Per Interval") +
        geom_text(aes(label=ifelse(avStepsPerInt == MxStp, paste('The ', MxInt, 'th interval has the max average of ', MxStpF, ' steps', sep = ''),'')),hjust=0,vjust=0, color = "Red")
```

![plot of chunk TimeSeries](figure/TimeSeries-1.png)

## Imputing missing values

This data set contains may missing values. The following analysis will determine how those missing values when imputed will change the distribution, mean, and median of total number of steps per day.

#### How Many Missing Values (NA) Are There?

The following code will calculate the total number of observations with missing step count values:


```r
# Calculate number of NAs

NumberOfNAs <- sum(is.na(DF$steps))

NumberOfNAs
```

```
## [1] 2304
```

#### Impute the Missing Values

The missing values will be imputed via the mean value of steps per interval.

The code to complete this task is as follows:


```r
# Create data frame with mean steps per interval

MeStepsByInterval <- DF %>%
        group_by(interval) %>%
        summarize(MeStepsPerInt = mean(steps, na.rm = TRUE)) %>%
        select(interval, MeStepsPerInt)

DF$steps <- as.numeric(DF$steps)

# Replace NAs with mean steps for that interval

MDF <- sqldf("
                
                SELECT
                CASE 
                        WHEN D.steps is NULL THEN M.MeStepsPerInt
                        WHEN D.steps is not NULL THEN D.steps
                END as steps
                , D.date
                , D.interval
        
                FROM DF as D
                
                LEFT OUTER JOIN MeStepsByInterval as M
                on M.interval = D.interval
             
             ")
```

#### Plot Distribution With Imputed Values


```r
# Sum the steps by day and store in data frame titled "aggImStepsByDay"
aggImStepsByDay <- MDF %>%
        group_by(date) %>%
        summarize(stepsPerDay = sum(steps)) %>%
        select(date, stepsPerDay)

# Plot the distribution of total steps per day and mark the mean and median with a line

M <- as.numeric(format(round(mean(aggImStepsByDay$stepsPerDay, na.rm = TRUE), 2), nsmall = 2))

m <- median(aggImStepsByDay$stepsPerDay, na.rm = TRUE)

a <- ggplot(aggImStepsByDay, aes(stepsPerDay))

a + geom_histogram() +
        xlab("Steps Per Day") +
        ylab("Count") +
        ggtitle("Distribution of Total Steps Per Day") +
        geom_vline(aes(xintercept = M, color = "Blue"), size = 2 , show.legend =TRUE) +
        geom_vline(aes(xintercept = m, color = "Red"), size = 1 , show.legend =TRUE) +
        scale_colour_manual(name='', labels=c(paste('Mean = ', M), paste('Median = ', m)), values = c("Blue", "Red")) +
        theme(legend.position="top")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk ImputeHistogram](figure/ImputeHistogram-1.png)

#### How Has Imputing Values Affected The Distrubution, Mean, and Median?

It isn't surprising that after imputing the mean values for each missing interval that the mean steps per day didn't change since the previous distribution ignored the days with missing values.

The median however has moved closer to the mean since the additional days were added with the mean total steps per day.

## Are there differences in activity patterns between weekdays and weekends?

The following code uses the imputed value data set and adds a factor variable indicating whether the day of the observation is a weekend or weekday.  After the factor variable has been added the average steps per interval is calculated accross all day for each factor (weekend or weekday).  Once the average steps per interval is calculated the results are plotted comparing the average weekday to weekend day.


```r
# Update date variable to date data type 
MDF$date <- as.Date(MDF$date)

# Add factor variable for weekend vs weekday

WeekMDF <- MDF %>%
        mutate(Day = weekdays(date))

WMDF <- sqldf("
                
                SELECT
                
                D.steps
                , D.date
                , D.interval
                , CASE 
                        WHEN D.Day in (\"Saturday\", \"Sunday\") THEN \"Weekend\"
                        Else \"Weekday\"
                END as WDay
        
                FROM WeekMDF as D
             
             ")

WMDF$WDay <- as.factor(WMDF$WDay)

# Average the steps per interval accross weekend/weekday grouping
avWStepsByInterval <- WMDF %>%
        group_by(interval, WDay) %>%
        summarize(avStepsPerInt = mean(steps)) %>%
        select(interval, WDay, avStepsPerInt)

# Create panel plot comparing average steps per interval accross weekend vs weekday

a <- ggplot(avWStepsByInterval, aes(interval, avStepsPerInt))

a + geom_line() +
        xlab("Interval") +
        ylab("Average Steps") +
        ggtitle("Average Steps Per Interval") +
        facet_grid(WDay ~ .)
```

![plot of chunk AddFactorVariable](figure/AddFactorVariable-1.png)

It looks like weekends have a shallower morning peak and then more activity throughout the day.  This would make sense since many people have morning commutes on weekdays and are probably less active while working (assuming they have an office job).



