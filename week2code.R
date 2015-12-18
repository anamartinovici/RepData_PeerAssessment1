
### Loading and preprocessing the data ###

steps<-read.csv("activity.csv")             #load the data

byDate<-split(steps$steps, steps$date)          #split steps by date
stepsPerDay<-sapply(byInterval, sum)            #add steps in intervals

byInterval<-split(steps$steps, steps$interval)      #split steps by interval

stepsPerInterval<-sapply(byInterval, mean, na.rm=TRUE)               #average steps in intervals

### What is mean total number of steps taken per day?###

hist(stepsPerDay, xlab="Steps",               #draw histogram
   main="Steps per day", ylab="Frequency")

mean<-mean(stepsPerDay, na.rm=TRUE)         #Calculate the mean steps
print(mean)

median<-median(stepsPerDay, na.rm=TRUE)     #Calculate the median steps
print(median)

### What is the average daily activity pattern? ###

#Make a time series plot

plot(stepsPerInterval, type='l', xlab="5-minute Interval", ylab="steps", 
     main="Steps per 5-minute interval across days")

#Which 5 minute interval across days contains max steps
maxInterval<-subset(stepsPerInterval, stepsPerInterval==max(stepsPerInterval))
print(names(maxInterval))

#Total number of missing values

### Impute Missing Values ###

#Fill in missing values

#Create new dataset with values filled

#Make a histogram of the total number of steps taken each day and 
  #Calculate and report the mean and median total number of steps taken per day. 
  #Do these values differ from the estimates from the first part of the assignment? 
  #What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.

#Make a panel plot containing time series of 5 min interval and average number of steps taken on weekends and weekdays