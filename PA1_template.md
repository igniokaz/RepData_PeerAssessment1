---
title: "Activity Monitoring"
author: "Ignas M"
date: "27 September 2016"
output: html_document
---

```{r setup}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the ???quantified self??? movement ??? a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as ????????)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## 1) Loading and Pre-Processing the Data
### Loading the data
```{r dataload, echo=TRUE}
pam<-read.csv("activity.csv")
head(pam)
summary(pam)
```

###Loading packages
```{r packages, echo=TRUE, warning=FALSE, message=FALSE}
library(dplyr)
library(ggplot2)
library(chron)
```

## 2) What is the mean total number of steps per day?
###Calculate Total Steps per Day
```{r pam, echo=TRUE}
grppam<-group_by(pam, date)
sumpam<-summarize(grppam, sum(steps))
colnames(sumpam)[2]<-"total"
head(sumpam)
summary(sumpam)
```

###Histogram of Total Steps per Day
```{r hist, echo=TRUE}
hist(sumpam$total, breaks=11)
```

###Calculating Mean & Median Daily Steps
```{r pam2, echo=TRUE, include=TRUE}
meansteps<-mean(sumpam$total, na.rm=TRUE)
mediansteps<-median(sumpam$total, na.rm=TRUE)
meansteps
mediansteps
```

## 3) What is the average daily activity pattern?
###Timeseries plot
```{r pattern, echo=TRUE}
grpint<-group_by(pam, interval)
sumint<-summarise(grpint, mean(steps, na.rm = TRUE))
colnames(sumint)[2]<-"avg_steps"

q<-ggplot(sumint, aes(interval, avg_steps))
q+geom_line(color="red", size=0.5)
```

### Which 5 min interval on average across all days contains a maximum number of steps?
```{r max, echo=TRUE}
max<-subset(sumint, avg_steps==max(sumint$avg_steps))
max
```

## 4) Imputing missing values

###Calculate number of lines with NA for steps
```{r nasteps, echo=TRUE}
sum(is.na(pam$steps))
```

### Devise a strategy to fill in the missing values

NA values could be replaced by interval average per observed day

### Create a new dataset with the missing values filled
```{r dataset2, echo=TRUE}
pam2<-pam
pam2[is.na(pam$steps),1]<-subset(sumint$avg_steps, sumint$interval%in%pam2$interval)
head(pam2)
```

### Histogram of total number of steps per day
```{r hist2, echo=TRUE}
grppam2<-group_by(pam2, date)
sumpam2<-summarize(grppam2, sum(steps))
colnames(sumpam2)[2]<-"total"
head(sumpam2)
summary(sumpam2)
hist(sumpam2$total, breaks=11)
meansteps2<-mean(sumpam2$total, na.rm=TRUE)
mediansteps2<-median(sumpam2$total, na.rm=TRUE)
meansteps2
mediansteps2
```

The impact of imputing the missing values is that the median increases, and becomes equal to the mean. There is no change to the mean between original and imputed values, as average calculations were used.

## 5) Are there differences in activity patterns between weekdays and weekends?
```{r hist3, echo=TRUE}
pam3<-mutate(pam2, weekend=is.weekend(date))
head(pam3)
grppam3<-group_by(pam3, interval, weekend)
sumint3<-summarise(grppam3, mean(steps))
colnames(sumint3)[3]<-"avg"
head(sumint3)
g<-ggplot(sumint3, aes(interval, avg), color=weekend)
g+facet_grid(.~weekend)+geom_line(color="blue")+geom_smooth(method="lm", se=F, size=0.5, color="black")
```

In the graph above, FALSE indicates it is a weekday, and TRUE indicates it is a weekend.

