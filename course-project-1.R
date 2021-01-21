##-----------------------Week 2 Peer graded Assignment------------------------##
##                             Course Project 1                               ##
##                                                                            ##
##    This script is part of the Reproducible Research Module of the Data     ##
##    Science Specialization, offered by Johns Hopkins University through     ##
##    Coursera platform.                                                      ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)

## Set language
Sys.setlocale("LC_ALL","English")

## Set the work directory
setwd("C:/Data-Science-Foundations-using-R-Specialization/Reproducible-Research/filehash/")

## Set link
lk <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

## Set file
zipFile <- "./data/activity.zip"

## Download
if(!dir.exists("./data")){  ## Checks if the data directory exists
        dir.create("data")  ## Creates the folder if it doesn't exist
}

## Checks if the zip file exists, otherwise it starts downloading it
if(!file.exists("./data/activity.zip")){
        download.file(url = lk,
                      destfile = zipFile,
                      mode = "wb")
        
        ## Unzip the file
        unzip(zipfile = zipFile,
              exdir = "./data")
}

## List files and directories
list.files(full.names = TRUE, 
           recursive = TRUE, 
           include.dirs = TRUE)

### Loading and preprocessing the data  ###
### 1. Load data
activity <- read.csv(file = "./data/activity.csv",
                     header = TRUE,
                     dec = ".",
                     sep = ",")

## Get more information about the data
str(activity)

## Read the data
as_tibble(activity)

### What is mean total number of steps taken per day? ###
### 1. Calculate the total number of steps taken per day
activity %>%
        filter(!is.na(steps)) %>%
        group_by(day) %>%
        summarize(avg_steps = mean(steps), Total_steps = n())

### 2. histogram of the total number of steps taken each day
activity %>%
        filter(!is.na(steps)) %>%
        group_by(day, steps) %>%
        ggplot(aes(x = day, y = steps)) +
        geom_bar(stat = "identity", fill = "steelblue")

### 3. Calculate and report the mean and median of the total number of steps
### taken per day
activity %>%
        filter(!is.na(steps)) %>%
        group_by(day, steps) %>%
        summarize(avg_steps = mean(steps), median = median(steps))

### What is the average daily activity pattern? ###
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
### and the average number of steps taken, averaged across all days (y-axis)
activity %>%
        filter(!is.na(steps), !is.na(date)) %>%
        group_by(date) %>%
        summarize(avg = mean(steps)) %>%
        ggplot(aes(x = date, y = avg)) +
        geom_line(color="steelblue") +
        scale_x_date(date_labels = "%b %d")

### 2. Which 5-minute interval, on average across all the days in the dataset, 
### contains the maximum number of steps?
activity %>%
        mutate(avg = mean(interval, na.rm = TRUE)) %>%
        summarize(max = max(avg, na.rm = TRUE)) %>%
        print()

### Imputing missing values ###
### 1. Calculate and report the total number of missing values in the dataset
activity %>%
        filter_all(any_vars(is.na(.))) %>%
        group_by(steps) %>%
        summarize(n = n())

### 2. Devise a strategy for filling in all of the missing values in the dataset.
### The strategy does not need to be sophisticated. For example, you could use
### the mean/median for that day, or the mean for that 5-minute interval, etc.
activity %>%
        replace_na(list(steps = 37.38)) %>%
        as_tibble()

### 3. Create a new dataset that is equal to the original dataset but with the 
### missing data filled in.
no_NA <- activity %>%
        replace_na(list(steps = 37.38)) %>%
        as_tibble()
### 4. Make a histogram of the total number of steps taken each day and 
### Calculate and report the mean and median total number of steps taken per day. 
### Do these values differ from the estimates from the first part of the 
### assignment? What is the impact of imputing missing data on the estimates of 
### the total daily number of steps?
no_NA %>%
        group_by(day) %>%
        summarize(mean = mean(steps),
                  median = median(steps)) %>%
        ggplot(aes(x = day)) +
        geom_histogram(fill="white", color = "steelblue", position="dodge")

### Are there differences in activity patterns between weekdays and weekends? ###
### 1. Create a new factor variable in the dataset with two levels – “weekday”
### and “weekend” indicating whether a given date is a weekday or weekend day.
activity_week <- activity %>%
        filter(!is.na(steps) | steps != 0, !is.na(interval)) %>%
        mutate(day = wday(date, label = TRUE, abbr = TRUE),
               Wday = ifelse(day == "Sun" | day == "Sat", "weekend", "weekday")) %>%
        group_by(interval, Wday) %>%
        summarize(avg = mean(steps, na.rm = TRUE))

### 2. Make a panel plot containing a time series plot (i.e. type = "l")
### of the 5-minute interval (x-axis) and the average number of steps taken, 
### averaged across all weekday days or weekend days (y-axis).
ggplot(na.omit(activity_week), aes(x = interval, y = avg)) +
        geom_line(stat = "identity", color = "steelblue") +
        facet_wrap(Wday ~ .) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "Interval (sec.)",
             y = "Number of Steps",
             title = "Mean steps over each 5min interval split by weekday/weekend")
