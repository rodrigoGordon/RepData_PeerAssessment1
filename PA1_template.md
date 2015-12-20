# Reproducible Research: Peer Assessment 1





## Loading and preprocessing the data



```r
activityDataSet <- read.csv("activity.csv", header = TRUE)

#convert the date column to a more handy format
activityDataSet$date <- as.Date(activityDataSet$date, "%Y-%m-%d")

sapply(activityDataSet, class)
```

```
##     steps      date  interval 
## "integer"    "Date" "integer"
```

```r
head(activityDataSet)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
Looks like we're good to go, besides the NA values on the columns STEPS, but more on that later in the assigment.

## What is mean total number of steps taken per day?

This part uses very simple functions such as: sum, mean and median; However, since we didn't fix the NA values on the data set, we need to use ( for now ), na.rm = TRUE, so we can calculate what we want, without being bothered by NA.

- Calculate the total number of steps taken per day, and to do this we use:

```r
totalNumberStepsPerDayBeforeNA <- as.data.frame.table(tapply(activityDataSet$steps,activityDataSet[,2],sum,na.rm = TRUE))
```

- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(totalNumberStepsPerDayBeforeNA$Freq)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

- Calculate and report the mean and median of the total number of steps taken per day
 - Mean 
    
    ```r
      mean(totalNumberStepsPerDayBeforeNA$Freq, na.rm = TRUE)
    ```
    
    ```
    ## [1] 9354.23
    ```
 - Median
    
    ```r
      median(totalNumberStepsPerDayBeforeNA$Freq, na.rm = TRUE)   
    ```
    
    ```
    ## [1] 10395
    ```

## What is the average daily activity pattern?
- Time series plot for # of Steps taken ~ Avg across all days

```r
#credits to http://stackoverflow.com/questions/4683242/calculating-a-daily-mean-in-r
#credits to http://r.789695.n4.nabble.com/How-to-quot-flatten-quot-a-multidimensional-array-into-a-dataframe-td4572108.html
part2 <- as.data.frame.table(tapply(activityDataSet$steps,activityDataSet[,3],mean,na.rm = TRUE))

plot(part2$Var1 , 
     part2$Freq, 
     type = "l",
     lwd = 3,
     col = "red"
     )
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 


## Imputing missing values

###Total number of missing values

```r
colSums(is.na(activityDataSet))
```

```
##    steps     date interval 
##     2304        0        0
```
###Strategy for filling the missing values


We take a mean of the steps taken and group it by day, then add the values to the rows that contain NA(through that ugly and slow FOR nested loop, I know).

```r
fixNa <- as.data.frame.table(tapply(activityDataSet$steps,activityDataSet[, 2],mean,na.rm = TRUE))
fixNa[is.na(fixNa)] <- 0
fixNa$Var1 <- as.Date(fixNa$Var1, "%Y-%m-%d")
```

###New dataset
Now we create a new dataset without the NA values

```r
copyActivityDataSet <- activityDataSet

#partial credits to http://stackoverflow.com/questions/19379081/how-to-replace-na-values-in-a-table-for-selected-columns-data-frame-data-tab
# the other half goes to http://stackoverflow.com/questions/5965698/r-merge-unequal-dataframes-and-replace-missing-rows-with-0

for(i in 1:nrow(copyActivityDataSet))
{
    for(j in 1:nrow(fixNa)){
        if(copyActivityDataSet$date[i] == fixNa$Var1[j]){
            copyActivityDataSet[i,1][is.na(copyActivityDataSet[i, 1])] <- fixNa$Freq[j]
        }
    }
}
```
Let's check how many NA's now

```r
colSums(is.na(copyActivityDataSet))
```

```
##    steps     date interval 
##        0        0        0
```
Looks good, let's move on.

###Histogram , Mean and Median with the new Dataset

```r
totalNumberStepsPerDayAfterNA <- as.data.frame.table(tapply(copyActivityDataSet$steps,activityDataSet[,2],sum,na.rm = TRUE))

hist(totalNumberStepsPerDayAfterNA$Freq)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

```r
mean(totalNumberStepsPerDayAfterNA$Freq)
```

```
## [1] 9354.23
```

```r
median(totalNumberStepsPerDayAfterNA$Freq)
```

```
## [1] 10395
```

###Do the values differ from part 1?
Apparently not if you simply look the values above, but let's take a look at the sum of the columns before and after the 
insertion of mean steps.


```r
sum(totalNumberStepsPerDayBeforeNA$Freq)
```

```
## [1] 570608
```

```r
sum(totalNumberStepsPerDayAfterNA$Freq)
```

```
## [1] 570608
```
And the values are the same, magic? Maybe not; If you saw any mistake, please leave a feedback, I'll appreciate very much.


## Are there differences in activity patterns between weekdays and weekends?
