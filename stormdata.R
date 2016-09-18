# |*****************************************************************************
# | Dwayne Macadangdang 9/17/2016
# | Coursera: Exploratory Data Analysis
# | Week 4 Programming Assignment 2

stormdata <- function()
{
  
  # working directory
  setwd("/Users/mistermaxx/Documents/work/personal/Coursera/Repro_Res/")
  
  # reference depedencies
  library(dplyr)
  library(ggplot2)
  library(lattice)
  library(lubridate)
  
  # getExponent function definition: return numeric value based on string value passed in
  # uses switch() and tolower() functions
  getExponent <- function(value)
  {
    switch(tolower(value), "h" = {2}, "k" = {3}, "m" = {6}, "b" = {9}, {0})
  }
  
  # decompress bzip archive, read data from file
  storm.file.data <- read.csv(bzfile("repdata-data-StormData.csv.bz2"))
  
  # subset data: required columns
  storm.data <- select(storm.file.data, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
  
  # get exponents for property and crop damage
  propertyExponent <- sapply(storm.data$PROPDMGEXP, FUN = getExponent)
  cropExponent <- sapply(storm.data$CROPDMGEXP, FUN = getExponent)
  
  #storm.data <- group_by(select(storm.file.data, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP), EVTYPE)
  
  # calculate values for property and crop damage
  storm.data$propertyDamage <- storm.data$PROPDMG * (10 ** propertyExponent)
  storm.data$cropDamage <- storm.data$CROPDMG * (10 ** cropExponent)
  
  # subset, sum injuries by event type
  injury.subset.data <- select(storm.data, EVTYPE, INJURIES) # subset down to just event type, injuries
  injury.data <- summarize(injury.subset.data, INJURIES = sum(INJURIES)) # sum the injuries by event type
  injury.data <- arrange(injury.data, desc(INJURIES)) # arrange injury totals in descending order
  
  # graph injury plot
  p1ot.injury.data <- ggplot(data = head(injury.data,10), aes(x = reorder(EVTYPE, INJURIES), y = INJURIES))
  p1ot.injury.data + geom_bar(fill = "steelblue1", stat = "identity") + coord_flip() + ylab("Total number of injuries") + xlab("Event type") + ggtitle("Health impact of weather events in the US - Top 10") + theme(legend.position = "none")
  
  # subset, sum fatalities by event type
  fatality.subset.data <- select(storm.data, EVTYPE, FATALITIES)
  fatality.data <- summarize(fatality.subset.data, FATALITIES = sum(FATALITIES))
  fatality.data <- arrange(fatality.data, desc(FATALITIES))
  
  # graph fatality plot
  plot.fatality.data <- ggplot(data = head(fatality.data,10), aes(x = reorder(EVTYPE, FATALITIES), y = FATALITIES))
  plot.fatality.data + geom_bar(fill = "darkcyan", stat = "identity") + coord_flip() + ylab("Total Fatalities") + xlab("Event Type") + theme(legend.position = "none")
  
  #property.damage.data <- summarize(property.subset.data)
  
  #econDamage <- ddply(stormDataRed, .(EVTYPE), summarize,propDamage = sum(propDamage), cropDamage = sum(cropDamage))
  
  }
