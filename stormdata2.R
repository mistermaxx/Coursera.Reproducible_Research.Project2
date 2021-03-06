# |*****************************************************************************
# | Dwayne Macadangdang 9/19/2016
# | Coursera: Reproducible Research
# | Week 4 Programming Assignment 2

stormdata <- function()
{
  
  # reference depedencies
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  
  # working directory
  setwd("/Users/mistermaxx/Documents/work/personal/Coursera/Repro_Res/Project2/")
  
  # getExponent function definition: return numeric value based on string value passed in
  # uses switch() and tolower() functions
  getExponent <- function(value)
  {
    switch(tolower(value), "h" = {2}, "k" = {3}, "m" = {6}, "b" = {9}, {0})
  }
  
  options("scipen" = 10)
  
  # decompress bzip archive, read data from file
  storm.file.data <- read.csv("StormData.csv")
  #storm.file.data <- read.csv(bzfile("StormData.csv.bz2"))

  
  # subset data: required columns
  storm.data <- select(storm.file.data, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
  
  # get exponents for property and crop damage
  propertyExponent <- sapply(storm.data$PROPDMGEXP, FUN = getExponent)
  cropExponent <- sapply(storm.data$CROPDMGEXP, FUN = getExponent)
  
  #storm.data <- group_by(storm.data, EVTYPE)
  
  # calculate values for property and crop damage
  storm.data$propertyDamage <- storm.data$PROPDMG * (10 ** propertyExponent)
  storm.data$cropDamage <- storm.data$CROPDMG * (10 ** cropExponent)
  
  # subset, sum injuries by event type
  injury.subset.data <- select(storm.data, EVTYPE, INJURIES) 
  injury.subset.data <- group_by(injury.subset.data, EVTYPE, INJURIES)
  injury.data <- aggregate(INJURIES ~ EVTYPE, data = injury.subset.data, sum) 
  injury.data <- arrange(injury.data, desc(INJURIES)) 
  
  # graph injury plot
  p1ot.injury.data <- ggplot(data = head(injury.data,10), aes(x = reorder(EVTYPE, INJURIES), y = INJURIES))
  injury.graph <- p1ot.injury.data + geom_bar(fill = "powderblue", stat = "identity") + coord_flip() + ylab("Total number of injuries") + xlab("Event type") + ggtitle("Health impact of weather events in the US - Top 10") + theme(legend.position = "none")
  
  # subset, sum fatalities by event type
  fatality.subset.data <- select(storm.data, EVTYPE, FATALITIES)
  fatality.subset.data <- group_by(fatality.subset.data, EVTYPE, FATALITIES)
  fatality.data <- aggregate(FATALITIES ~ EVTYPE, data = fatality.subset.data, sum)
  fatality.data <- arrange(fatality.data, desc(FATALITIES))
  
  # graph fatality plot
  plot.fatality.data <- ggplot(data = head(fatality.data,10), aes(x = reorder(EVTYPE, FATALITIES), y = FATALITIES))
  fatality.graph <- plot.fatality.data + geom_bar(fill = "darkcyan", stat = "identity") + coord_flip() + ylab("Total Fatalities") + xlab("Event Type") + theme(legend.position = "none")
  
  grid.arrange(injury.graph, fatality.graph)
  
  # subset, sum property damage data
  property.subset.data <- select(storm.data, EVTYPE, propertyDamage)
  property.subset.data <- group_by(property.subset.data, EVTYPE, propertyDamage)
  property.damage.data <- aggregate(propertyDamage ~ EVTYPE, data = property.subset.data, sum)
  property.damage.data <- arrange(property.damage.data, desc(propertyDamage))
  
  # graph property damage plot
  plot.property.data <- ggplot(data = head(property.damage.data, 10), aes(x = reorder(EVTYPE, propertyDamage), y = propertyDamage, fill = propertyDamage )) 
  #plot.property.data <- ggplot(data = head(property.damage.data, 10), aes(x = reorder(EVTYPE, propertyDamage), y = log10(propertyDamage), fill = propertyDamage )) 
  #plot.property.data <- ggplot(data = head(property.damage.data, 10), aes(x = reorder(EVTYPE, propertyDamage), y = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 250), fill = propertyDamage )) 
  property.graph <- plot.property.data + geom_bar(fill = "mediumturquoise", stat = "identity") + coord_flip() + xlab("Event Type") + ylab("Property Damage in Dollars (Hundred Billions)") + ggtitle("Economic Impact of Extreme Weather Events in the US: Top 10") + theme(plot.title = element_text(hjust = 0)) 
  
  # subset, sum crop damage data
  crop.subset.data <- select(storm.data, EVTYPE, cropDamage)
  crop.subset.data <- group_by(crop.subset.data, EVTYPE, cropDamage)
  crop.damage.data <- aggregate(cropDamage ~ EVTYPE, data = crop.subset.data, sum)
  crop.damage.data <- arrange(crop.damage.data, desc(cropDamage))
  
  # graph crop damage plot
  plot.crop.data <- ggplot(data = head(crop.damage.data, 10), aes(x = reorder(EVTYPE, cropDamage), y = cropDamage, fill = cropDamage)) 
  #plot.crop.data <- ggplot(data = head(crop.damage.data, 10), aes(x = reorder(EVTYPE, cropDamage), y = log10(cropDamage), fill = cropDamage))
  crop.graph <- plot.crop.data + geom_bar(fill = "darksalmon", stat = "identity") + coord_flip() + xlab("Event Type") + ylab("Crop Damage in Dollars (Billions)") + theme(legend.position="none") 
  
  grid.arrange(property.graph, crop.graph, ncol = 1, nrow = 2)
}