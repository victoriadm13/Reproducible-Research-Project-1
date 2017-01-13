
## Loading the data

data <- read.csv("activity.csv")

### First look at the data

head(data)
summary(data)

# What is mean total number of steps taken per day?

#Total number of steps taken per day

steps_per_day <- aggregate(steps ~ date, data, sum)

# Histogram of the total number of steps taken each day

hist(steps_per_day$steps, main = "Number of steps per day", xlab = "Number of steps", col = "gray")


#Mean and median of the total number of steps


mean <- mean(steps_per_day$steps)
median <- median(steps_per_day$steps)


# What is the average daily activity pattern?

#Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days (y-axis)

steps_by_interval <- aggregate(steps ~ interval, data = data, FUN = "mean")

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")


#Maximum number of steps  

max <- steps_by_interval[which.max(steps_by_interval$steps),1]



# Imputing missing values

# Total number of missing values in the dataset

nas <- sum(!complete.cases(data))

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

# To filled the missing data I used the mean number of steps of the interval

mean_int <- mean(steps_by_interval$steps)
data1 <- data
nas_index <- is.na(data[,1])
data1[nas_index,1] <- mean_int
head(data1)

#Histogram of the total number of steps taken each day


steps_by_day1 <- aggregate(steps ~ date, data1, sum)
hist(steps_by_day1$steps, xlab = "Total number of steps by day", main = "Number of steps by day with imputed missing values")

#Mean and median total number of steps per day


mean1 <- mean(steps_by_day1$steps)
median1 <- median(steps_by_day1$steps)

# Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


data1$date <- as.Date(data1$date)

weekday <- function(d) {
  weekdays <- weekdays(d)
  ifelse (weekdays == "Saturday" | weekdays == "Sunday", "weekend", "weekday")
}

week <- sapply(data1$date, weekday)
data1$week <- as.factor(week)
head(data1)


# Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


data3 <- aggregate(steps ~ week+interval, data = data1, FUN = "mean")
library(lattice)
xyplot(steps ~ interval | factor(week),
       layout = c(1,2),
       xlab = "Interval",
       ylab = "Number of steps",
       type = "l",
       lty = 1,
       data = data3)








