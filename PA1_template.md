# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
df.data <-read.csv("activity.csv")
# create a Datetime class Date column 
df.data["Date"] <- NA
df.data$Date <- as.POSIXct(as.character(df.data$date), format = "%Y-%m-%d")

## What is mean total number of steps taken per day?
df.totalSteps.day.rmNA <-with(df.data, aggregate(steps, by=list(Date = Date), FUN=sum, na.rm=TRUE))
names(df.totalSteps.day.rmNA)[2] <- "TotalSteps"
##To produce a histogram that illustrates the total number of steps taken per day.
library("ggplot2")
# produce the histogram
ggplot(data=df.totalSteps.day.rmNA) + geom_histogram(aes(x=Date, y=TotalSteps), fill="blue", stat="identity")+
  xlab("Date") +
  ylab("Total Steps") +
  ggtitle(expression("Total Steps per Day")) 

## What is the average daily activity pattern?
df.meanSteps.interval.rmNA <-with(df.data, aggregate(steps, by=list(Interval = interval), 
                                                     FUN=mean, na.rm=TRUE))

names(df.meanSteps.interval.rmNA)[2] <- "MeanSteps"

# create a interval column of Datetime class

df.meanSteps.interval.rmNA["TimeInterval"] <- NA

df.meanSteps.interval.rmNA$TimeInterval <- strptime(
  sprintf("%04d", df.meanSteps.interval.rmNA$Interval),"%H%M")

# produce the line grapah
with(df.meanSteps.interval.rmNA, plot(TimeInterval, MeanSteps, type="l", ylab=expression("Average steps across all days"),xlab="Interval", main = expression("Average Steps by Interval")))
  

## Imputing missing values
# Below R code calculates the total number of rows with NA values in the dataset

sum(is.na(df.data$steps))# shows count of NA

## [1] 2304

## The below R code implements the strategy of filling missing values with mean number of steps for the interval.

# merge in the mean steps per interval from previous section, identify position of NAs and replace NAs with means steps
df.data.naFilled <- merge(df.data, df.meanSteps.interval.rmNA, by.x = "interval", by.y="Interval" )

v_na.Steps <- which(is.na(df.data.naFilled$steps)== TRUE)

df.data.naFilled[v_na.Steps,2] <- df.data.naFilled[v_na.Steps,5]


# sum up steps by Date and name the sum column 'TotalSteps'
df.data.naFilled.sum <-with(df.data.naFilled, aggregate(steps, by=list(Date = Date), 
                                                        FUN=sum))

names(df.data.naFilled.sum)[2] <- "TotalSteps"

# produce the histogram
ggplot(df.data.naFilled.sum) + geom_histogram(aes(x=Date, y=TotalSteps), fill="green", stat="identity")+
  xlab("Date") +
  ylab("Total Steps") +
  ggtitle(expression("Total Steps per Day")) 


## Are there differences in activity patterns between weekdays and weekends?
##To create the 'weekday' or 'weekend' factor column (Cat) and the line graphs 
##which illustrate the difference in mean steps taken between weekday and weekend.

# create a new factor column Cat for weekend or weekday factors
df.data.naFilled["Cat"] <- NA

df.data.naFilled$Cat <- ifelse (weekdays(df.data.naFilled$Date) %in% c("Saturday", "Sunday"), 
                                "weekend", "weekday")

df.data.naFilled$Cat <- as.factor(df.data.naFilled$Cat)

# average steps by interval and name the column 'MeanSteps'
df.data.naFilled.mean <-with(df.data.naFilled, aggregate(steps, by=list(TimeInterval = interval, Cat = Cat), 
                                                         FUN=mean))

names(df.data.naFilled.mean)[3] <- "MeanSteps"

df.data.naFilled.mean$TimeInterval <- strptime(sprintf("%04d", df.data.naFilled.mean$TimeInterval), "%H%M")

# load scales library to manipulate x scales
library("scales")

# plot the 2 line graphs
ggplot(df.data.naFilled.mean, aes(x=TimeInterval, y=MeanSteps)) +
  geom_line() +
  xlab("Interval") +
  ylab("Average Steps") +
  ggtitle(expression("Average Step by Intervals across Weekday and Weekend")) + 
  scale_x_datetime(labels = date_format("%H:%M"),breaks = "2 hour") +
  facet_grid(Cat~ .)
