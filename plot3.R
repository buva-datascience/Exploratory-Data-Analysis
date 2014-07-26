#  ------------------------------------------------------------------------------------------------------------------------
#
# Course:  Exploratory Data Analysis
# Project: Course Assignment II
# Author:  Buva Ramamoorthy
# 
# Program Name: plot3.R
# Date Written: Jul 24 2014
#
# Program prepare_data.R uses function dwld_file to download the data from 
# Data download url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip and then find the following:
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which   
# of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen   
# increases in emissions from 1999–2008?
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
# Question 3:
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which   
# of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen   
# increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.  
#
################################################################################

# Convert year into a factor
SummarySCC.PM25$year <- factor(SummarySCC.PM25$year) 

Baltimore.Data <- SummarySCC.PM25[ SummarySCC.PM25$fips =="24510",]

plot_data <- ddply(Baltimore.Data, .(year, type), summarize, Avg.Emissions=mean(Emissions))

# GG plot 
par(bg="white", mfrow=c(1,1))

qplot(data=plot_data, x=year, y=Avg.Emissions, facets= .~type, xlab="Year", ylab="Average Emissions", 
      main="Emission Types - Baltimore City (Maryland)")

# copy the plot from the screen device to .png file
dev.copy(png, file="plot3.png", width = 480, height = 480)

# turn device off
dev.off()