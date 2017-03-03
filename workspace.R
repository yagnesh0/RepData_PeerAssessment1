totalSteps<-tapply(X = dfActivity$steps,INDEX = dfActivity$date,sum,na.rm=TRUE)

str(totalSteps)

mean(totalSteps)
median(totalSteps)

averageSteps<-tapply(X = dfActivity$steps,INDEX = dfActivity$interval, mean, na.rm=TRUE)
plot(averageSteps,type="l",col="purple",xlab="Interval",ylab="Average Number of Steps")
averageSteps[which.max(averageSteps)]


sum(!complete.cases(dfActivity))

dfActivity[(!complete.cases(dfActivity)), ]

dfActivity2<-dfActivity

for(i in 1:nrow(dfActivity2)){
    if(is.na(dfActivity2$steps[i])){
        for(j in 1:nrow(averageSteps)){
            if(as.character(dfActivity2$interval[i])==names(averageSteps[j])){
                dfActivity2$steps[i] = averageSteps[[j]]
            }
        }
    }
}

dfActivity2$interval[1]

names(averageSteps[2])
averageSteps[[2]]

nrow(averageSteps)

for(i in 1:nrow(averageSteps)){
    if(as.character(dfActivity2$interval[1])==names(averageSteps[i])){
        dfActivity2$steps[1] = averageSteps[[i]]
    }
}

dfActivity<-read.csv("activity.csv")
dfActivity$days <-if(weekdays(as.POSIXlt.date(dfActivity$date))=="Sunday" |  weekdays(as.POSIXlt.date(dfActivity$date))=="Saturday"){
                    "Weekend"
                } else {
                    "Weekday"
                }



dfActivity$days <- weekdays(as.POSIXlt.date(dfActivity$date))
dfActivity$daytype <- dfActivity$days

for(k in 1:nrow(dfActivity)){
    if(dfActivity$days[k] == "Saturday" || dfActivity$days[k] == "Sunday"){
        dfActivity$daytype[k] = "weekend"
    } else {
        dfActivity$daytype[k] = "weekday"
    }
}

dfActivity$daytype<-as.factor(dfActivity$daytype)


dfActivityWeekend<-subset(dfActivity,dfActivity$daytype == "weekend")
dfActivityWeekday<-subset(dfActivity,dfActivity$daytype == "weekday")

averageStepsWeekend<-tapply(X = dfActivityWeekend$steps,INDEX = dfActivityWeekend$interval, mean, na.rm=TRUE)
averageStepsWeekday<-tapply(X = dfActivityWeekday$steps,INDEX = dfActivityWeekday$interval, mean, na.rm=TRUE)

par(mfrow = c(2,1))
plot(averageStepsWeekday,type="l",col="red",xlab="Interval",ylab="Steps",main="Average Number of Steps for Weekdays")
plot(averageStepsWeekend,type="l",col="blue",xlab="Interval",ylab="Steps",main="Average Number of Steps for Weekends")

