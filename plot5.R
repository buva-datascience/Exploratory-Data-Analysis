#  ------------------------------------------------------------------------------------------------------------------------
#
# Course:  Exploratory Data Analysis
# Project: Course Assignment II
# Author:  Buva Ramamoorthy
# 
# Program Name: plot5.R
# Date Written: Jul 25 2014
#
# Program prepare_data.R uses function dwld_file to download the data from 
# Data download url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip and then find the following:
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#
#  ------------------------------------------------------------------------------------------------------------------------ 
#
# Prepare working directory
wd <- "/Users/sangeethabuvanendiran/Documents/Buva- Data Science/Exploratory Data Analysis/Course Project/"
setwd(wd)

library(plyr)
library(ggplot2)
rm(list=ls())

# function to Download the file from the website location to the local directory
dwld_file <- function(fileurl){
        
        if (!file.exists("data")) dir.create("data")                            # create a folder if it doesnt exist        
        
        if (!file.exists("./data/NEI-Dataset.zip")) {           # download the file if its not already downloaded
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

################################################################################
#
# Assignment
#
# The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say  
# about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any   
# R package you want to support your analysis.
#
# You must address the following questions and tasks in your exploratory analysis. For each question/task you will  
# need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.
#
################################################################################
#
# Question 5:
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#
################################################################################
#

# Convert year into a factor
SummarySCC.PM25$year <- factor(SummarySCC.PM25$year) 

# Filter Baltimore Data
Baltimore.Data <- SummarySCC.PM25[ SummarySCC.PM25$fips =="24510",]

# Create a subset of the SCC dataset with fewer columns
SCC_Subset <- SCC[1:4]

# Check for the Motor Vehicle Source classification
Vehicle.Data <- as.data.frame(SCC[ grep("vehicle", SCC_Subset$EI.Sector, ignore.case=T), 1])
names(Vehicle.Data) <- "SCC"

# Merge the Motor Vehicle with the PM25 data
Merged <- merge(Vehicle.Data, Baltimore.Data, by="SCC", na.rm=TRUE)

# prepare the data for plotting by calculating the mean
plot_data <- ddply(Merged, .(year), summarize, Avg.Emissions=mean(Emissions))

# GG plot 
par(bg="white", mfrow=c(1,1))

# Use ggplot() for graph
gplot<-ggplot(plot_data, aes(year, Avg.Emissions))
gplot + geom_point(size=4) + labs( title="PM2.5 Emission from Motor Vehicles - Baltimore City",
                                   y="Average PM2.5 Emission each year")

# copy the plot from the screen device to .png file
dev.copy(png, file="plot5.png", width = 480, height = 480)

# turn device off
dev.off()