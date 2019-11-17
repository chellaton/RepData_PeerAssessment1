
library(tidyverse)
library(dplyr)
library(gridExtra)

PAdata <- read.csv("activity.csv", stringsAsFactors = FALSE)
PAdata$date <- as.Date(PAdata$date, "%Y-%m-%d")
summary(PAdata)

PAdata_bg <- group_by(PAdata, date)
PAdata_summary <- summarize(PAdata_bg, totalSteps=sum(steps, na.rm=TRUE))

hist(PAdata_summary$totalSteps, 
     xlab="Number of Steps", 
     main="Histogram for Number of Steps per Day",
     breaks=15, col="lightblue", ylim=c(0,25))

totalNA <- sum(complete.cases(PAdata))

averageSteps <- aggregate(steps ~ interval, PAdata, FUN=mean)
newSet <- numeric()
for (i in 1:nrow(PAdata)) {
  if(is.na(PAdata[i,]$steps)) {
    steps <- subset(averageSteps, interval == PAdata[i,]$interval)$steps
    steps <- round(steps,0)
  }
  else
  {
    steps <- PAdata[i,]$steps
  }
  newSet <- c(newSet, steps)
}

PAdata_new <- PAdata
PAdata_new$steps <- newSet

PAdata_new_bg <- group_by(PAdata_new, date)
PAdata_new_summary <- summarize(PAdata_new_bg, totalSteps=sum(steps, na.rm=TRUE))

hist(PAdata_new_summary$totalSteps, 
     xlab="Number of Steps", main="Histogram for Number of Steps per Day: Imputed Data",
     breaks=15, col="lightblue", ylim=c(0,25))


