# PeerAssignment 1
James Primrose  
11/08/2014  
### What is mean total number of steps taken per day?
*  For this part of the assignment, you can ignore the missing values in the dataset.
*  Make a histogram of the total number of steps taken each day
*  Calculate and report the mean and median total number of steps taken per day
* aggregate to get total steps per day

Load the data

```r
old.par <- par(mfrow=c(1,1))
act  <- read.csv("activity.csv",head=T,sep=",")
```

Take a peek at the data

```r
head(act)
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

Aggregate steps per day

```r
per.day <- aggregate(steps ~ date, data=act,FUN=sum,na.rm=TRUE)
s <- per.day$steps
```

Grab the mean & median

```r
smean <- mean(s)
smed <- as.numeric(median(s))
```

Make Histogram and format

```r
b <- as.numeric(nrow(per.day))/2
h <- hist(s,breaks=b,main="Total Steps Per Day",xlab="Steps",col="green")
sfit<-seq(min(s),max(s),length=b)
yfit<-dnorm(sfit,mean=smean,sd=sd(s))
yfit <- yfit*diff(h$mids[1:2])*length(s)
lines(sfit, yfit, col="blue", lwd=2) 
tout <- paste("Mean: ",round(smean,d=2),"\nMedian :",smed,sep="")
mtext(tout, 3,-.25,cex=.65)
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Cleanup

```r
rm(per.day,b,h,s,sfit,smean,smed,tout,yfit)
```






### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval  (x-axis) and the average number of steps taken, averaged across all days (y-axis). 
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Take a look again at the same data

```r
head(act)
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

Aggregate for mean steps per interval

```r
avg.day <- aggregate(steps ~ interval, data=act,FUN=mean,na.rm=TRUE)
```

Pull in the mean of aggregated means

```r
smean <- mean(avg.day$steps)
```

Get the max interval value

```r
smax <- subset(avg.day,avg.day$steps==max(avg.day$steps))
```

Plot away w/abline & mtext

```r
plot(avg.day, type="l",xlab="interval",lty=1,main="5 min interval avg steps taken",cex=.5)
abline(v=smax$interval,col="red",lty=3,cex=1)
mtext(paste("Max Avg Steps: \n",smax$interval,": ",round(smax$steps,d=2),sep=""), 3,-.15,cex=.65)
```

![](./PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

Cleanup

```r
rm(avg.day,smax,smean)
```


### Imputing missing values
* Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3 - Create a new dataset that is equal to the original dataset but with the missing data filled in.

I still dont remember the data <sigh>

```r
head(act)
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

get a number of blank (NA) rows using nrow 


```r
nan <- as.numeric(nrow(subset(act,is.na(act$steps))))
```

Print

```r
print(paste("The total number of missing values in the dataset is ",nan,sep=""))
```

```
## [1] "The total number of missing values in the dataset is 2304"
```

Duplicate the data for safety

```r
act2 <- act
```

Replace NA with zero (0)

```r
act2[is.na(act2)] <- 0
```

Aggregate the steps per interval using the copy pasta strategy above

```r
avg.interval <- aggregate(steps ~ interval, data=act2,FUN=mean)
```

Brute force subset to split off non-zero

```r
zer <- subset(act2,act2$steps==0)
nzer <- subset(act2,act2$steps>0)
```

Merge the avg per interval

```r
zer <- merge(zer,avg.interval, by="interval")
zer <- zer[c(1,3,4)]
```

Reorder

```r
zer <- zer[c(3,2,1)]
names(zer) <- c("steps", "date",  "interval")
```

Rbind the final set

```r
final.act <- rbind(nzer,zer)
head(final.act)
```

```
##     steps       date interval
## 555   117 2012-10-02     2210
## 556     9 2012-10-02     2215
## 627     4 2012-10-03      410
## 631    36 2012-10-03      430
## 644    25 2012-10-03      535
## 647    90 2012-10-03      550
```

Cleanup

```r
rm(act2,avg.interval,zer,nzer,nan)
```

Aggregate to get total steps per day

```r
per.day <- aggregate(steps ~ date, data=final.act,FUN=sum,na.rm=TRUE)
s <- per.day$steps
```

Grab the mean & median

```r
smean <- mean(s)
smed <- as.numeric(median(s))
```

Make a histogram

```r
b <- as.numeric(nrow(per.day))/2
h <- hist(s,breaks=b,main="Adj. Total Steps Per Day",xlab="Steps",col="blue")
sfit<-seq(min(s),max(s),length=b)
yfit<-dnorm(sfit,mean=smean,sd=sd(s))
yfit <- yfit*diff(h$mids[1:2])*length(s)
lines(sfit, yfit, col="green", lwd=2) 
tout <- paste("Mean: ",round(smean,d=2),"\nMedian :",smed,sep="")
mtext(tout, 3,-.25,cex=.65)
```

![](./PA1_template_files/figure-html/unnamed-chunk-26-1.png) 

Q. Do these values differ from the estimates from the first part of the assignment?

A. Yes, we see another 5k steps based on the updated averages. The change is rather considerable.

Q. What is the impact of imputing missing data on the estimates of the total daily number of steps?

A. Data becomes more right skewed. Since it's clear people walk, the data must be missing values 
it makes sense that since people are sedentary that few steps would be taken per time interval?
I dunno. It's probably because these devices like iWatch and FitBit seem awesome at first, but people
really don't like being spied on. So they get wind that these devices are watching their every more
and pretty soon, people view the device with contempt. They hide from it. They don't want to pay more
for health care and it's clear this device is squealing on them. They don't want it to know they got 2 double cheese Monday morning at 10am from McDonalds so they lie to it. Starve it of electricity. Anything to avoid being seen by that electronic accelorometer. Anything to avoid being judged.

Cleanup

```r
rm(per.day,b,h,old.par,s,sfit,smean,smed,tout,yfit)
```


### Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

One more look at the data

```r
head(final.act)
```

```
##     steps       date interval
## 555   117 2012-10-02     2210
## 556     9 2012-10-02     2215
## 627     4 2012-10-03      410
## 631    36 2012-10-03      430
## 644    25 2012-10-03      535
## 647    90 2012-10-03      550
```

1 - Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
final.act$wd <- factor(weekdays(as.Date(final.act$date,'%Y-%m-%d')))
levels(final.act$wd) <- c("Weekday","Weekday","Weekend","Weekend","Weekday","Weekday","Weekday")
```

2 - Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

Whack up the data into weekend and weekday

```r
final.act.end <- subset(final.act,final.act$wd=="Weekend")
final.act.end <- final.act.end[with(final.act.end, order(date,interval)),]
per.end <- aggregate(steps ~ interval, data=final.act.end,FUN=mean,na.rm=TRUE)

final.act.day <- subset(final.act,final.act$wd=="Weekday")
final.act.day <- final.act.day[with(final.act.day, order(date,interval)),]
per.day <- aggregate(steps ~ interval, data=final.act.day,FUN=mean,na.rm=TRUE)
```

Set our graphing parms to be tiny / narrow margin, give a bit of space right for our top yaxis and maybe some mtext blurbs & Plot

```r
par(mfrow = c(1,1),
          oma = c(5,4,0,0) + 0.05,
          mar = c(0,0,1,2) + 0.05)
par(fig=c(0,1,0,0.5))
plot(per.end$steps ~ per.end$interval,type="l",ylim=c(0,250))
mtext("Weekend",3,-1)
par(fig=c(0,1,0.5,1), new=TRUE)
plot(per.day$steps ~ per.day$interval,xaxt="n",yaxt="n",type="l",ylim=c(0,250))
axis(side=4)
mtext("Weekday",3,-1)
title(xlab = "Interval",
      ylab = "Steps",
      outer = TRUE, line = 3)
```

![](./PA1_template_files/figure-html/unnamed-chunk-31-1.png) 

Cleanup

```r
old.par <- par(mfrow=c(1,1))
rm(act, final.act, final.act.day, final.act.end, per.day, per.end, old.par)
```



