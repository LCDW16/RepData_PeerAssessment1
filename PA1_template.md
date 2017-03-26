Reproducible Research: Peer Assessment 1
Loading and preprocessing the data
# read data
setwd("D:/Course5_wk2_assignment")
activity_data <- read.csv("./activity.csv")
str(activity_data)
## 'data.frame':    17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
# Transform date from Factor to Date class 
activity_data$date <- as.Date(activity_data$date, format = "%Y-%m-%d")
What is mean total number of steps taken per day?
steps_per_day  <- tapply(activity_data$steps, activity_data$date, sum, na.rm= TRUE)

# make histogram
hist(steps_per_day, col = "red", breaks=20, xlab = "total steps per day", main ="Histogram total number of steps per day" )
 
# calculate and report mean and median of the total number of steps taken per day
mean_per_day  <- round(mean(steps_per_day, na.rm = TRUE))
median_per_day  <- round(median(steps_per_day, na.rm = TRUE))

# mean of total number of steps per day
print(mean_per_day)
## [1] 9354
# median of total number of steps per day
print(median_per_day)
## [1] 10395
What is the average daily activity pattern?
# calculate average steps
average_steps <- aggregate(activity_data$steps, list(interval=activity_data$interval), mean, na.rm = TRUE)
     
# make plot
plot(average_steps, type = "l", col = "red", main = "average number of steps of the 5-minute interval", xlab = "5-minute interval", ylab = "average number of steps")
 
# the 5-minute interval with the maximum number of steps
max_steps <- max(average_steps$x)
max_interval <- average_steps[which(average_steps$x == max_steps),1]

# interval with maximum number of steps
print(max_interval)
## [1] 835
Imputing missing values
# Calculate and report the total number of missing values in the dataset 
total_NA <- sum(is.na(activity_data))

# number of missing values in the dataset
print(total_NA)
## [1] 2304
# create a new dataset which is a copy of the original dataset
activity_data_nw <- activity_data

# replace NA with the average of the interval.
activity_data_nw$steps[which(is.na(activity_data_nw$steps))] = rep(average_steps$x, 288)[which(is.na(activity_data_nw$steps))]

# create a new dataset which is a copy of the original dataset
activity_data_nw <- activity_data

# replace NA with the average of the interval.
activity_data_nw$steps[which(is.na(activity_data_nw$steps))] = rep(average_steps$x, 288)[which(is.na(activity_data_nw$steps))]

# calculate total number of steps per day fot the new data
steps_per_day_nw  <- tapply(activity_data_nw$steps, activity_data_nw$date, sum, na.rm= TRUE)

# make a histogram of the new data
hist(steps_per_day_nw, col = "red", breaks=20, xlab = "total steps per day", main ="Histogram of total number of steps per day (NA replaced by mean)" )
 
# calculate the mean and median of the new data
mean_per_day_nw  <- mean(steps_per_day_nw, na.rm = TRUE)
median_per_day_nw  <- median(steps_per_day_nw, na.rm = TRUE)

# mean of total number of steps per day of the data NA replaced by mean
print(mean_per_day_nw)
## [1] 10766.19
# median of total number of steps per day of the data NA replaced by mean
print(median_per_day_nw)
## [1] 10766.19
# calculate the differences
diff_mean <- mean_per_day_nw - mean_per_day
diff_median <- median_per_day_nw - median_per_day

# difference between the both means
print(diff_mean)
## [1] 1412.189
# difference between the both medians
print(diff_median)
## [1] 371.1887
The mean and median based on the data, where NAs are replaced by mean are larger than the mean and median based on original data.
Are there differences in activity patterns between weekdays and weekends?
## setlocale
Sys.setlocale("LC_TIME", "English")
## [1] "English_United States.1252"
days <- weekdays(activity_data_nw$date)
activity_data_days <- cbind(activity_data_nw, days)

# define weekdays    
week_day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
activity_data_days$dayType = ifelse(days %in% week_day, "weekday", "weekend")

# calculate average steps for weekdays and weekends
average_steps <- aggregate(activity_data_days$steps, list(interval=activity_data_days$interval, dayType = activity_data_days$dayType), mean, na.rm = TRUE)

# make plot
library(ggplot2)
g <- ggplot(average_steps, aes(interval, x))
g <- g + facet_grid(dayType~.)
p <- g + geom_line(stat="identity", color = "red")+labs(x="interval",y="number of steps")+labs(title="activity patterns between weekdays and weekends")
print(p)
 

There are differences between activity pattern weekdays and weekends.
