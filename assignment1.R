library(dplyr)
library(ggplot2)
library(lubridate)

unzip("./activity.zip")

activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

str(activity)

## Config locale
Sys.setlocale(locale = "C")

## Transform date
activity$date <- ymd(activity$date, tz = "UTC")
summary(activity$date)

year <- year(activity$date)
month <- month(activity$date)
day <- day(activity$date)


minsec <- formatC(activity$interval, width = 4, flag = "0")
hours <- substr(minsec, 1, 2)
minutes <- substr(minsec, 3, 4)

activity$datetime <- make_datetime(year, month, day, hours, minutes)


# What is mean total number of steps taken per day?

## Step sum for days
# sumsteps <- activity %>%
#                 mutate(days = wday(date, label = TRUE)) %>%
#                 group_by(days) %>%
#                 summarise(sum = sum(steps, na.rm= TRUE))
# 
# g <- ggplot(sumsteps, aes(days, sum))
# g + geom_col()

## step means for day
# sumsteps <- activity %>%
#     mutate(days = wday(date, label = TRUE)) %>%
#     group_by(date) %>%
#     summarise(sum = sum(steps, na.rm= TRUE)) %>% 
#     mutate(days = wday(date, label = TRUE)) %>% 
#     group_by(days) %>%
#     summarise(mean = mean(sum, na.rm = TRUE))
# 
# g <- ggplot(sumsteps, aes(days, mean))
# g + geom_col()


# sumxday <- activity %>%
#                 group_by(date) %>%
#                 summarise(sum = sum(steps, na.rm = TRUE))

# Histogram of total of total number of steps taken each days, by wday
sumsteps <- activity %>%
    group_by(date) %>%
    summarise(sum = sum(steps, na.rm= TRUE)) %>% 
    mutate(days = wday(date, label = TRUE))

g <- ggplot(sumsteps, aes(sum, fill = days))
g + geom_histogram(color = "black", bins = 15) +
    xlab("Total number of steps taken each day")

# 
# 
# activity <- activity %>%
#                 mutate(days = wday(date, label = TRUE))
#                 
# 
# g <- ggplot(activity, aes(steps, fill = days))
# g + geom_histogram(color = "black")
# 
# g <- ggplot(activity, aes(log10(steps), fill = days))
# g + facet_grid(days ~ .) +
#     geom_histogram()
# 
# barplot(c(sumsteps$days, sumsteps$sum))


## 2. Calculate and report the mean and median total number of steps taken

# meansteps <- activity %>%
#         group_by(date) %>%
#         summarise(mean = mean(steps, na.rm = TRUE))
# 
# meansteps <- activity %>%
#     mutate(days = wday(date, label = TRUE)) %>%
#     group_by(date) %>%
#     summarise(sum = sum(steps, na.rm= TRUE)) %>%
#     mutate(days = wday(date, label = TRUE)) %>%
#     group_by(days) %>%
#     summarise(mean = mean(sum, na.rm = TRUE))

meansteps <- activity %>%
    mutate(days = wday(date, label = TRUE)) %>%
    group_by(date) %>%
    summarise(sum = sum(steps, na.rm= TRUE)) %>% 
    summarise(mean = mean(sum), median = median(sum))
    

# mediansteps <- activity %>%
#     mutate(days = wday(date, label = TRUE)) %>%
#     group_by(date) %>%
#     summarise(sum = sum(steps, na.rm= TRUE)) %>%
#     mutate(days = wday(date, label = TRUE)) %>%
#     group_by(days) %>%
#     summarise(median = median(sum, na.rm = TRUE))


## What is the average activity pattern?

# averagedaily <- activity %>%
#                     group_by(interval) %>%
#                     summarise(mean = mean(steps, na.rm = TRUE))
# 
# g <- ggplot(averagedaily, aes(seq_along(interval), mean))
# g + geom_line() +
#     labs(x = "5-minute interval", y = "Number of steps taken across all days")
# 
averagedaily <- activity %>%
    mutate(time = strftime(datetime, format = "%H:%M", tz = "UTC")) %>%
    group_by(time) %>%
    summarise(mean = mean(steps, na.rm = TRUE)) %>%
    mutate(time = as.POSIXct(origin + hm(time)))

g <- ggplot(averagedaily, aes(time, mean))
g + geom_line() +
    labs(x = "5-minute interval", y = "Average number of steps taken across all days") +
    scale_x_datetime(date_labels = "%H:%M")
    
#    as.POSIXct(0, tz = "UTC", origin = origin)


averagedaily <- activity %>%
    mutate(time = origin + hm(time)) %>%
    group_by(time) %>%
    summarise(mean = mean(steps, na.rm = TRUE))



##2.

maxaverage <- with(averagedaily, time[which.max(mean)])
strftime(maxaverage, format = "%H:%M", tz = "UTC")


## Imputing missing values

## 1. 

summary(activity)[10]
with(activity, sum(is.na(steps)))


## 2

 # mean(activity$steps[activity$interval == activity$interval[1]], na.rm = TRUE)
# vector <- activity %>% filter(interval == 0) %>% select(steps)
# mean(vector$steps, na.rm = TRUE)
## Copy the dataframe to a new variable

fillactivity <- activity

## Define function total mean per interval
totalmeanperint <- function(x){
    mean(activity$steps[activity$interval == activity$interval[x]], na.rm = TRUE)
}
## Fill NAs with total mean per interval
for(i in seq_along(fillactivity$steps)){
        if(is.na(fillactivity$steps[i])) {
            fillactivity$steps[i] <- totalmeanperint(i)
        }
}

## 4. Histogram and calculate the mean

sumstepsfill <- fillactivity %>%
    group_by(date) %>%
    summarise(sum = sum(steps, na.rm= TRUE)) %>% 
    mutate(days = wday(date, label = TRUE))

g <- ggplot(sumstepsfill, aes(sum, fill = days))
g + geom_histogram(color = "black", bins = 15) +
    xlab("Total number of steps taken each day")


meanstepsfill <- fillactivity %>%
    mutate(days = wday(date, label = TRUE)) %>%
    group_by(date) %>%
    summarise(sum = sum(steps)) %>% 
    summarise(mean = mean(sum), median = median(sum))


## Are there differences in activity patterns between weekdays and week-ends?
# isweekend <- function(x){
#     y <- FALSE
#     for(i in 1:5){
#         y <- y || x == i
#     }
#     !y
# }
# 
# weekrecode <- function(vector){
#                 x <- vector(mode = "character")
#                 for(i in seq_along(vector)){
#                     ifelse(isweekend(vector[i]), x[i] <- "Weekend", x[i] <- "Weekday")
#                 }
#                 x
# }
# 
# 
# fillactivity <- fillactivity %>% 
#                 mutate(wday = wday(date),
#                        wday = recode(wday, `1` = "Weekday",
#                                            `2` = "Weekday",
#                                            `3` = "Weekday",
#                                            `4` = "Weekday",
#                                            `5` = "Weekday",
#                                            `6` = "Weekend",
#                                            `7` = "Weekend",
#                                            ))

fillactivity$wday[wday(fillactivity$date) <= 5] <- "Weekday"
fillactivity$wday[wday(fillactivity$date) > 5] <- "Weekend"

fillmeansteps <- fillactivity %>%
                    group_by(date, wday) %>% 
                    summarise(mean = mean(steps)) 
    
averagedailyfill <- fillactivity %>%
    mutate(time = strftime(datetime, format = "%H:%M", tz = "UTC")) %>%
    group_by(time, wday) %>%
    summarise(mean = mean(steps) %>% 
    group_by() %>% 
    mutate(time = as.POSIXct(origin + hm(time)))

    
g <- ggplot(averagedailyfill, aes(time, mean))
g + facet_grid(wday ~ . ) +
    geom_line() +
    labs(x = "5-minute interval", y = "Average number of steps taken across all days") +
    scale_x_datetime(date_labels = "%H:%M")
    