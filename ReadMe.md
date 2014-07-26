<!------------------------------------------------------
title: "ReadMe.md"
author: "Buva Ramamoorthy"
date: "July 26, 2014"
output: MarkDown Document
------------------------------------------------------->
        
# **Exploratory Data Analysis - Course Project 2**
        
*by Buva Ramamoorthy*
        
This is a repository which is created for the project submission for the course Exploratory Data Analysis 
by John Hopkins University

## Overview
The purpose of this project is to demonstrate working/exploring with a data set to prepare analytical  
reasoning to answer few questions. The goal is to prepare graphs which assist in performing an exploratory 
analysis with the tidy data set for better analysis.

For this course we will be considering the the National Emissions Inventory (NEI) data collected by the  
US Government. Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong   
evidence that it is harmful to human health. In the United States, the Environmental Protection Agency  
(EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions  
of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions  
of PM2.5. For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted   
from that source over the course of the entire year. The data that you will use for this assignment are for   
1999, 2002, 2005, and 2008.

The data for this assignment is available as a single zip file: 

[Click here to find the source data for the project:](https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip) 

## Course Project Summary

The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about   
fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package  
you want to support your analysis. 
You must address the following questions and tasks in your exploratory analysis.   
For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to  
make your plot. Each question is answered via a single program with name **Plot#.R** where # is 1-6 to answer all questions 1-6.

**Questions:**
- 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system,  
     make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
- 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?   
     Use the base plotting system to make a plot answering this question.
- 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these   
     four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in   
     emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
- 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
- 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
- 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in  
     Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

**Making and Submitting Plots:**

For each plot you should
  - Construct the plot and save it to a PNG file.
  - Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R  
    constructs the plot1.png plot. Your code file should include code for reading the data so that the plot can be fully   
    reproduced. You should also include the code that creates the PNG file. Only include the code for a single plot   
    (i.e. plot1.R should only include code for producing plot1.png)
  - Upload the PNG file on the Assignment submission page
  - Copy and paste the R code from the corresponding R file into the text box at the appropriate point in the peer assessment.

## Additional Information and Important points for Github R/DataScience community 

## About Datasets:
PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame with all of the PM2.5 emissions data   
for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific  
type of source for the entire year. Here are the first few rows.

<pre><code>
##     fips      SCC Pollutant Emissions  type year
## 4  09001 10100401  PM25-PRI    15.714 POINT 1999
## 8  09001 10100404  PM25-PRI   234.178 POINT 1999
## 12 09001 10100501  PM25-PRI     0.128 POINT 1999
## 16 09001 10200401  PM25-PRI     2.036 POINT 1999
## 20 09001 10200504  PM25-PRI     0.388 POINT 1999
## 24 09001 10200602  PM25-PRI     1.490 POINT 1999
</code></pre>

-**fips:**      A five-digit number (represented as a string) indicating the U.S. county
-**SCC:**       The name of the source as indicated by a digit string (see source code classification table)
-**Pollutant:** A string indicating the pollutant
-**Emissions:** Amount of PM2.5 emitted, in tons
-**type:**      The type of source (point, non-point, on-road, or non-road)
-**year:**      The year of emissions recorded

Source Classification Code Table (Source_Classification_Code.rds): This table provides a mapping from the SCC  
digit strings int he Emissions table to the actual name of the PM2.5 source. The sources are categorized in a           
few different ways from more general to more specific and you may choose to explore whatever categories you   
think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

**Important** : The author has hard coded the working directory assertions in each **plot#.R** program at   
line#17. Might need modifications to reset the working directory for code compatibility for other mac users.
