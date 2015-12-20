
### Loading and preprocessing the data ###

steps<-read.csv("activity.csv")             #load the data

byDate<-split(steps$steps, steps$date)          #split steps by date
stepsPerDay<-sapply(byDate, sum, na.rm=TRUE)            #add steps in intervals

byInterval<-split(steps$steps, steps$interval)      #split steps by interval
stepsPerInterval<-sapply(byInterval, mean, na.rm=TRUE) #average steps in intervals

steps$date<-as.Date(steps$date)


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

### Impute Missing Values ###

nrow(subset(steps, is.na(steps))) #Calculate total number of NAs

#Impute missing values

impute<-function(dfr){
     yn<-NULL
     x<NULL
     
     for (i in dfr$steps){
          if(is.na(i)) x<-as.integer(mean(dfr$steps, na.rm=TRUE))
          else x<-i
          yn<-c(yn,x)
     }
     return (yn)
}


{
     int<-split(dfr$steps, dfr$interval)
     int[dfr]
     x<-mean(, na.rm=TRUE)
}

#Create new dataset with values filled
imp<- impute(steps) 
imputedSteps<-steps
imputedSteps$steps<-imp

#Make a histogram of the total number of steps taken each day 

  #Calculate and report the mean and median total number of steps taken per day. 
  #Do these values differ from the estimates from the first part of the assignment? 
  #What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.

imputedSteps$date<-as.Date(imputedSteps$date)
wkday<-sapply(imputedSteps$date, weekdays)

wkdaywkConv <- function(wkday) {
  yn<-NULL
  for (i in wkday){
    if(i=="Saturday") i<-"weekend"
    else if (i=="Sunday") i<-"weekend"
    else i<-"weekday"
    yn<-c(yn,i)
  }
  return (yn)
}

wkday<-factor(wkConv(wkday))
wkSteps<-cbind(imputedSteps,wkday)

#Make a panel plot containing time series of 5 min interval and average number 
#of steps taken on weekends and weekdays

byWeek<-split(wkSteps,wkSteps$wkday)

wday<-byWeek$weekday
wdaybyInterval<-split(wday$steps, wday$interval)      #split steps by interval
wdayPerInterval<-sapply(wdaybyInterval, mean, na.rm=TRUE) #average steps in intervals

wend<-byWeek$weekend
wendbyInterval<-split(wend$steps, wend$interval)      #split steps by interval
wendPerInterval<-sapply(wendbyInterval, mean, na.rm=TRUE) #average steps in intervals

par(mfrow=c(1,2))
plot(wdayPerInterval, main="Weekday", type='l')
plot(wendPerInterval, main="Weekend", type='l')