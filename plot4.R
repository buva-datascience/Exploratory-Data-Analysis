#  ------------------------------------------------------------------------------------------------------------------------
#
# Course:  Exploratory Data Analysis
# Project: Course Assignment II
# Author:  Buva Ramamoorthy
# 
# Program Name: plot4.R
# Date Written: Jul 25 2014
#
# Program prepare_data.R uses function dwld_file to download the data from 
# Data download url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip and then find the following:
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#
#  ------------------------------------------------------------------------------------------------------------------------ 
#
# Prepare working directory
wd <- "/Users/sbuva/Documents/Buva- Data Science/Exploratory Data Analysis/Course Project/"
setwd(wd)

library(plyr)
library(ggplot2)
rm(list=ls())

# function to Download the file from the website location to the local directory
dwld_file <- function (fileurl) {
        
        if (!file.exists("data")) dir.create("data")                            # create a folder if it doesnt exist        
        
        if (!file.exists("./data/NEI-Dataset.zip")) {                           # download the file if its not already downloaded
                download.file(fileurl, destfile = "./data/NEI-Dataset.zip", method = "curl")
        }
        
        # Unzip all files
        unzip("./data/NEI-Dataset.zip", overwrite=T)
        
        #List all files in zip folder
        files <- unzip("./data/NEI-Dataset.zip", list=T)
}

# Call funtion to download from the url
dwld_file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip")

# read RDS files
SCC             <- readRDS("Source_Classification_Code.rds")
SummarySCC.PM25 <- readRDS("summarySCC_PM25.rds")
#
################################################################################
#
# Question 4:
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#
################################################################################
#
# Convert year into a factor
SummarySCC.PM25$year <- factor(SummarySCC.PM25$year) 

# Check for the coal combustion emission source classification
Coal.Data <- as.data.frame(SCC[ grep("coal", SCC$EI.Sector, ignore.case=T),1 ])
names(Coal.Data) <- "SCC"

# Merge the Coal combustion with the PM25 data
Merged <- merge(Coal.Data, SummarySCC.PM25, by="SCC", na.rm=TRUE)

# prepare the data for plotting by calculating the mean
plot_data <- ddply(Merged, .(year), summarize, Avg.Emissions=mean(Emissions))

# GG plot 
par(bg="white", mfrow=c(1,1))

# Use may also use the qplot() for graph
# qplot(data=plot_data, x=year, y=Avg.Emissions, xlab="Year", ylab="Average Emissions each year", 
#      main="United States - PM2.5 Emission from coal combustion-related sources")

# Use ggplot() for graph
gplot<-ggplot(plot_data, aes(year,Avg.Emissions))
gplot + geom_point(size=4) + labs( title="United States - PM2.5 Emission from coal combustion-related sources",
                                   y="Average PM2.5 Emission each year")

# copy the plot from the screen device to .png file
dev.copy(png, file="plot4.png", width = 480, height = 480)

# turn device off
dev.off()