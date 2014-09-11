
### Loading and preprocessing the data

# Show any code that is needed to
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

### What is mean total number of steps taken per day?

dailyMean <- mean(daySum, na.rm = TRUE)

# For this part of the assignment, you can ignore the missing values in
# the dataset.

# 1. Make a histogram of the total number of steps taken each day

hist(daySum, breaks = 20) 

# 2. Calculate and report the **mean** and **median** total number of steps taken per day

summary(daySum) #easiest way - and gives the most information

### What is the average daily activity pattern?

# 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

intervalMean <- as.vector(tapply(activityData$steps, 
                                 activityData$interval, 
                                 mean, na.rm = TRUE))
plot(intervalMean, type = "l", main = "Mean steps per 5 min interval")

# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

dayMax <- which.max(intervalMean)
maxVal <- intervalMean[dayMax]
maxTime <- paste((dayMax * 5) %/% 60, ":", (dayMax * 5) %% 60, sep = "")

### Imputing missing values

# Note that there are a number of days/intervals where there are missing
# values (coded as `NA`). The presence of missing days may introduce
# bias into some calculations or summaries of the data.

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
summary(daySumImputed)

# The mean and median are not equivalent, 
# and the distribution has been more leptokurtotic as a result of 
# loading the mean values

### Are there differences in activity patterns between weekdays and weekends?

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





## Submitting the Assignment

To submit the assignment:
        
        1. Commit the your completed `PA1_template.Rmd` file to the `master` branch of your git repository (you should already be on the `master` branch unless you created new ones)

2. Commit your `PA1_template.md` and `PA1_template.html` files produced by processing your R markdown file with `knit2html()` function in R (from the **knitr** package)

3. If your document has figures included (it should) then they should have been placed in the `figures/` directory by default (unless you overrided the default). Add and commit the `figures/` directory to yoru git repository.

4. Push your `master` branch to GitHub.

5. Submit the URL to your GitHub repository for this assignment on the course web site.

In addition to submitting the URL for your GitHub repository, you will
need to submit the 40 character SHA-1 hash (as string of numbers from
                                            0-9 and letters from a-f) that identifies the repository commit that
contains the version of the files you want to submit. You can do this
in GitHub by doing the following

1. Going to your GitHub repository web page for this assignment

2. Click on the "?? commits" link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say "10 commits".

3. You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the "copy to clipboard" button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.

A valid submission will look something like (this is just an **example**!)

```r
https://github.com/rdpeng/RepData_PeerAssessment1

7c376cc5447f11537f8740af8e07d6facc3d9645
```

