#load the data
steps<-read.csv("activity.csv")

### Make a histogram of total steps per day ###

byDate<-split(steps$steps, steps$date)      #split by date
stepsPerDay<-lapply(byDate, sum)            #add steps in intervals
hist(stepsPerDay, xlab="Day",               #draw histogram
     main="Steps per day", ylab="Steps")

#Calculate the mean steps
mean<-mean(steps$steps, na.rm=TRUE)

#Calculate the median steps
median<-median(steps$steps, na.rm=TRUE)

#Make a time series plot

#Which 5 minute interval across days contains max steps

#Total number of missing values

#Fill in missing values

#Create new dataset with values filled

#Make a histogram of the total number of steps taken each day and 
  #Calculate and report the mean and median total number of steps taken per day. 
  #Do these values differ from the estimates from the first part of the assignment? 
  #What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.

#Make a panel plot containing time series of 5 min interval and average number of steps taken on weekends and weekdays