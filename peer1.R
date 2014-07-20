## load data
mydata <- read.csv("activity.csv")

## process date
mydata$date <- as.Date(mydata$date)

## omit missing data
newdata <- na.omit(mydata)

## count the total number of steps taken each day, aggregate() is very handy.
totalSteps <- aggregate(steps ~ date, newdata, sum)

## total steps on the x-axis, count on the y-axis
ggplot(totalSteps, aes(x=steps)) + geom_histogram()

## calculate the mean and median of the total number of steps taken per day. 
## that means one mean and one median for the steps column. 
steps.mean <- mean(totalSteps$steps)
steps.median <- median(totalSteps$steps)

## sum the steps in each interval (e.g., sum the steps in interval 0 of all dates), 
## and then calculate the mean of each interval (e.g., the mean of interval 0 of all dates)
## plot the mean (x-axis is the interval, and y-axis is the mean)
## use aggregate() function!
meanSteps <- aggregate(newdata$steps, list(interval=newdata$interval), mean)
with(meanSteps, plot(interval, x, type="l", xlab="5 Minute Interval", ylab="Mean Steps"))

## find the interval with the max number of mean steps of all interval
maxInterval <- meanSteps[which.max(meanSteps$x), ]

## calculate the total number of missing values
nmissing <- nrow(mydata[!complete.cases(mydata), ])

## replace NA with mean of interval
## merge mydata and meanSteps
mergeMean <- merge(mydata, meanSteps, by="interval")
mergeMean$steps[is.na(mergeMean$steps)] <- mergeMean$x[is.na(mergeMean$steps)]

## total number of steps taken each day
totalStepsna <- aggregate(steps ~ date, mergeMean, sum)
## histogram 
ggplot(totalStepsna, aes(x=steps)) + geom_histogram()
## calculate the mean and median of the total number of steps taken per day. 
## that means one mean and one median for the steps column. 
stepsna.mean <- mean(totalStepsna$steps)
stepsna.median <- median(totalStepsna$steps)

## add day column to mergeMean dataset
mergeMean$day <- weekdays(mergeMean$date)
## revalue day column to weekday and weekend
mergeMean$day <- factor(mergeMean$day)
library(plyr)
mergeMean$day <- revalue(mergeMean$day, c("Monday"="weekday", "Tuesday"="weekday", 
                                          "Wednesday"="weekday", "Thursday"="weekday", 
                                          "Friday"="weekday", "Saturday"="weekend", "Sunday"="weekend"))

## aggregate on interval and day type (weekday or weekend)
day.mean <- aggregate(mergeMean$steps, list(interval=mergeMean$interval, day=mergeMean$day), mean)
## make a panel plot 
xyplot(x ~ interval|day, data=day.mean, type="l", layout=c(1, 2))