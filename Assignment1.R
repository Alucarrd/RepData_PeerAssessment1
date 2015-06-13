
# set the working directory
setwd("./DataScience/ReproducibleResearch/Assignment1")

# Always make code visible
echo = TRUE  

# load the necessary library
library(plyr)
library(reshape)
library(lattice)

# read off the initial data
activity <- read.csv(file="data/activity.csv", na.strings = "NA", stringsAsFactors = FALSE)

# get the total of steps by date, remove na
total_step.df <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)

# plot the histogram
hist(total_step.df, main = "Total Step Per Day", xlab ="Number Of Steps")

# pivot the data set
total_step_list <- melt(total_step.df, id=c("date"))

# reset the column name
colnames(total_step_list) <- c('date', 'step')

# get the average of steps
mean(total_step_list[,2])

# get the median of steps
median(total_step_list[,2])

# get the average interval and step
avgInterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)

# plot the average result set
plot(x = avgInterval$interval, y = avgInterval$steps, type="l", main ="Average Daily Activity Pattern", xlab="Interval", ylab="Steps" )

# find the steps for max interval
avgInterval[which(avgInterval[, 2] == max(avgInterval[, 2])),]

# find out how many NA there are
sum(!complete.cases(activity))

# dup original data set
filled_activity <- activity

# use for loop to fill in the NA with average dataset
for (i in 1:nrow(filled_activity)){
  if(is.na(filled_activity$steps[i])){
    filled_activity$steps[i] <- avgInterval[which(filled_activity$interval[i] == avgInterval$interval), ]$steps
  }
  
}

# find out how many NA there are to verify the result
sum(!complete.cases(filled_activity))

# calculate the mean of steps from the filled dataset
total_step_filled.df <- tapply(filled_activity$steps, filled_activity$date, FUN = sum, na.rm = TRUE)

# Plot the histogram of the filled dataset.
hist(total_step_filled.df, main = "Total Step Pers Day", xlab ="Number Of Steps")

# pivot the result set
total_step_filled <- melt(total_step_filled.df, id=c("date"))

# Rename columns
colnames(total_step_filled) <- c('date', 'step')

# Find out the mean of filled dataset
mean(total_step_filled[,2])

# Find out the median of filled dataset
median(total_step_filled[,2])

# put original mean/median and new mean/median into variables.  Then compare the result.
original_mean <- mean(total_step_list[,2])

original_median <- median(total_step_list[,2])

new_mean <- mean(total_step_filled[,2])

new_median <- median(total_step_filled[,2])

original_mean - new_mean

original_median - new_median

# Add a new column to indicate whether the date is weekday or weekend 
filled_activity$day = ifelse(as.POSIXlt(as.Date(filled_activity$date))$wday%%6 == 0, "weekend", "weekday")

# For Sunday and Saturday : weekend, Other days : weekday
filled_activity$day = factor(filled_activity$day, levels = c("weekday", "weekend"))

# Calculate the means
filled_activity_mean = aggregate(steps ~ interval + day, filled_activity, mean)

# Plot the result using xyplot
xyplot(steps ~ interval | factor(day), data = filled_activity_mean, aspect = 1/2, 
       type = "l")