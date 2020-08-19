#Loading and processing data
activity <- read.csv("activity.csv")
library(dplyr)
steps <- group_by(activity, date) %>% 
        summarise(total = sum(steps), mean = mean(steps, na.rm = T), 
                  median = median(steps))



#What is the mean total number of steps per day
hist(steps$total, breaks = 20, xlab = "Total Steps in a Day",
     col = "wheat", border = "red",
     main = "Histogram of the Total Number of Steps Taken per Day")
View(steps)

#What is the average daily activity pattern?
library(ggplot2)
interval <- group_by(activity, interval) %>%
              summarise(mean = mean(steps, na.rm = T))
g <- ggplot(interval, aes(interval, mean))
g + geom_line(col = "red") + 
    labs(title = "Mean Steps per Interval", y = "Mean Steps", x = "Interval")
interval$interval[which(interval$mean == max(interval$mean))]

#Imputing missing values
#number of missing values
sum(is.na(activity$steps))

#replacing missing values with mean for interval
new.activity <- activity
for (i in 1:length(new.activity$steps)) {
    if (is.na(new.activity$steps[i])) {
        inter <- new.activity$interval[i]
        x <- interval$mean[which(interval$interval == inter)]
        new.activity$steps[i] <- x
    }
}
View(new.activity)

#New total number of steps with histogram
new.steps <- group_by(new.activity, date) %>% 
  summarise(total = sum(steps), mean = mean(steps, na.rm = T), 
            median = median(steps))
hist(new.steps$total, breaks = 20, xlab = "Total Steps in a Day",
     col = "wheat", border = "red",
     main = "Histogram of the Total Number of Steps Taken per Day")
View(new.steps)


#Weekday vs weekend analysis
new.activity$date <- as.Date(new.activity$date)
for (i in 1:length(new.activity$date)) {
    if (weekdays(new.activity$date[i]) %in% c("Saturday", "Sunday")){
        new.activity$weekday[i] <- "weekend"
    } else new.activity$weekday[i] <- "weekday"
}
new.activity$weekday <- as.factor(new.activity$weekday)
new.interval <- group_by(new.activity, interval, weekday) %>%
    summarise(mean = mean(steps, na.rm = T))
gnew <- ggplot(new.interval, aes(interval, mean))
gnew + geom_line(col = "red") + facet_grid(weekday ~ .)
  labs(title = "Mean Steps per Interval", y = "Mean Steps", x = "Interval")
