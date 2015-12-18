data = read.csv("activity.csv")
summary(data)

totalSteps <- aggregate(data$steps~data$date, FUN=sum)

#hist(totalSteps[,2], breaks=10, plot=TRUE, xlab="Number of steps", main="Total steps per day")

mean(totalSteps[,2])
median(totalSteps[,2])

dailyAverage <- aggregate(data$steps~data$interval, FUN=mean)

#plot(dailyAverage[,1], dailyAverage[,2], type="l", xlab="time (min)", 
#     ylab="average steps per 5 min.", main="Average daily steps pattern")

sum(is.na(data$steps))

imputedData <- rep(data)
for(i in 1:length(data$steps))
{
  if(is.na(data$steps[i]))
  {
    idx <- match(data$interval[i],dailyAverage[,1])
    imputedData$steps[i] <- dailyAverage[idx,2]
  }  
}

totalSteps <- aggregate(imputedData$steps~imputedData$date, FUN=sum)

#hist(totalSteps[,2], breaks=10, plot=TRUE, xlab="Number of steps", main="Total steps per day")

mean(totalSteps[,2])
median(totalSteps[,2])

weekend <- c("Saturday","Sunday")
weekendDays <- weekdays(as.Date(imputedData$date)) %in% weekend
imputedData$dayTime <- factor(weekendDays, levels=c(TRUE, FALSE), labels=c('weekend', 'weekday')) 


averages <- aggregate(imputedData$steps, by=list(imputedData$interval,imputedData$dayTime), FUN=mean)
names(averages) <- c("intervals","dayType","steps")

library(ggplot2)
g <- ggplot(averages, aes(x=intervals,y=steps)) #initial call: aestethics
g + geom_line() +
  facet_grid(. ~ dayType) +
  labs(title="Average daily step pattern") +
  labs(x="interval") +
  labs(y="Number of steps")

