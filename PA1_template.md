# R project for Coursera Reproducible research peer assessment 1
repdata-005 
Raymond Nelson - August 2005
Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Show any code that is needed to


```r
# 1. Load the data (i.e. `read.csv()`)

# first set the working directory
# then download the data from the URL
dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if(!exists("activity.zip")) download.file(dataURL, destfile = "activity.zip", method = "curl")
if(!exists("activity.csv")) unzip("./activity.zip")
# activityData <- read.csv("activity.csv")
activityData <- read.table("activity.csv", 
                           header = TRUE, 
                           sep = ",", 
                           quote = "\"", 
                           dec = ".", 
                           fill = TRUE, 
                           comment.char = "", 
                           colClasses = c("integer", "Date", "integer"))
activityData$interval <- as.ordered(activityData$interval)

# 2. Process/transform the data (if necessary) into a format suitable for your analysis

# newData <- split(activityData, activityData$date)
# newData2 <- as.array(newData)
daySum <- as.vector(tapply(activityData$steps, activityData$date, sum))
```

## What is mean total number of steps taken per day?

Ignore the missing values in the dataset.


```r
dailyMean <- mean(daySum, na.rm = TRUE)

# 1. Make a histogram of the total number of steps taken each day

hist(daySum, breaks = 20) 
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
# 2. Calculate and report the **mean** and **median** total number of steps taken per day

summary(daySum) #easiest way - and gives the most information
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8840   10800   10800   13300   21200       8
```

## What is the average daily activity pattern?


```r
# 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

intervalMean <- as.vector(tapply(activityData$steps, 
                                 activityData$interval, 
                                 mean, na.rm = TRUE))
plot(intervalMean, type = "l", main = "Mean steps per 5 min interval")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

dayMax <- which.max(intervalMean)
maxVal <- intervalMean[dayMax]
maxTime <- paste((dayMax * 5) %/% 60, ":", (dayMax * 5) %% 60, sep = "")
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

Missing values are imputed from the interval mean.


```r
# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with `NA`s)

missingVal <- which(is.na(activityData$steps))
numberMissing <- length(missingVal)

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.

# I will fill missing values from the interval mean

# 3. Create a new dataset that is equal to the original dataset 
# but with the missing data filled in.

activityDataImputed <- activityData
# get the factor level for the time interval of the missing values
missingValFactors <- activityDataImputed[missingVal,3]
# make a vector of the factor levels
intervals <- as.integer(levels(activityDataImputed$interval))
# replace the NA values with the interval mean
activityDataImputed$steps[missingVal] <- intervalMean[match(missingValFactors, intervals)]

# 4. Make a histogram of the total number of steps taken each day 
# and Calculate and report the **mean** and **median** total number of steps taken per day. 
# Do these values differ from the estimates from the 
# first part of the assignment? 
# What is the impact of imputing missing data on the 
# estimates of the total daily number of steps?

daySumImputed <- as.vector(tapply(activityDataImputed$steps, activityDataImputed$date, sum))
dailyMeanImputed <- mean(daySumImputed, na.rm = TRUE)
hist(daySumImputed, breaks = 20, main = "Mean steps per day")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

```r
summary(daySumImputed)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9820   10800   10800   12800   21200
```

```r
# The mean and median are not equivalent, 
# and the distribution has been more leptokurtotic as a result of 
# loading the mean values
```

## Are there differences in activity patterns between weekdays and weekends?


```r
# For this part the `weekdays()` function may be of some help here. Use
# the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with 
# two levels -- "weekday" and "weekend" 
# indicating whether a given date is a weekday or weekend day.

activityDataImputed$day <- as.factor(weekdays(activityDataImputed$date))
activityDataImputed$wDay <- as.factor(with(activityDataImputed, 
                                 ifelse(day == "Saturday" | day == "Sunday", 
                                        "weekend", 
                                        "weekday")))

# 1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using **simulated data**:

wDayMeans <- aggregate(steps ~ wDay + interval, data = activityDataImputed, mean)

par(mfcol = c(2, 1))
with(wDayMeans, {
        plot(wDayMeans$steps[which(wDayMeans$wDay == "weekend")], type = "l", main = "Weekend", ylab = "Mean Steps")
        plot(wDayMeans$steps[which(wDayMeans$wDay == "weekday")], type = "l", main = "Weekday", ylab = "Mean Steps") 
        }
     )
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

