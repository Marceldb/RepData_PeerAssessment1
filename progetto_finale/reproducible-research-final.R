# Reproducible Research - Project 2 Analysis of NOAA Storm Datase

# Synopsis This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.



# A) DATA PROCESSING
# Data are in a csv compress file, for 48MB total in the raw format. They are obtained from the course website at
# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","D://datasets/reproducible-research/NOAA.csv.bz2 

# Data are automatically downloaded from the course website with download.file() function while read_csv() unzips and read the data


#loading raw file

setwd("D:/datasets/reproducible-research")

# downloading raw file
# download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","D://datasets/reproducible-research/NOAA.csv.bz2")

# reading file (no need to unzip, read_csv does it automatically)
library(readr)
myData <- read_csv("NOAA.csv.bz2")
names(myData)

# the NOAA dataset contains 37 fields. Most relevant for our analysis are the fatalities, injuries, propdmg, propdmgexp, cropdmg, cropdmgexp, evtype fields

summary(myData)

# there is an inconsistency in lower/uppercase use that could be problematic. We select relevant fields and convert all the character variable to uppercase values.

library(dplyr)
NOAA <- myData %>% select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% mutate_if(is.character, toupper) 



# Q.1)Across the United States, which types of events (as indicated in the \color{red}{\verb|EVTYPE|}EVTYPE variable) are most harmful with respect to population health?


library(ggplot2)
library(gridExtra)

NOAA_by_EVTYPE<-NOAA %>% 
        select(EVTYPE, FATALITIES, INJURIES) %>% 
        group_by(EVTYPE) %>% 
        summarise_all(sum) %>% 
        arrange(desc(FATALITIES))

tmp <- head(NOAA_by_EVTYPE, n=5)

p1<-tmp %>% ggplot(aes(x=EVTYPE, y=FATALITIES))+ geom_bar(stat = "identity") + theme_bw()

p2<-tmp %>% ggplot(aes(x=EVTYPE, y=INJURIES))+ geom_bar(stat = "identity")+ theme_bw()
       
grid.arrange(p1,p2, ncol=2)

# Tornados are by far the most harmful events, followed by excessive heat

#2)       Across the United States, which types of events have the greatest economic consequences?

# B) RESULTS