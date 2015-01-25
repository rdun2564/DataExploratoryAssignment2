library(dplyr)
setwd("~/RData3/DEAssignment2")
## This first line will likely take a few seconds. Be patient!

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# sink("DEAssign2.out", append = TRUE)

str(SCC)

# Question 1 : Have Total PM25 Emissions decreased in US

# First Generate quick crude plot of year by Total PM25
NEIGroupByYearCrude <- group_by(NEI, year)
NEIPM25ByYearCrude <- summarise(NEIGroupByYearCrude, MeanPM25 = mean(Emissions))

png(filename = "plot4.png", width = 480, height = 480, units = "px")
par(mfcol = c(1,1), mar = c(5,4,3,1))
with(NEIPM25ByYearCrude, plot(year,MeanPM25, xlab = "", 
        ylab ="Avg Particulate matter emitted per source(tons)", ylim = c(0,8), type = "n"))
with(NEIPM25ByYearCrude, points(year,MeanPM25, pch = 20, type = "b", col="blue"))
title (main = "Average Fine Particulates Emitted (PM25) per Source\n in US between 1999 and 2008",
       sub = "This graph was generated from uncorrected data.") 
dev.off()

#
# Step 1: Examines the unbalanced nature of the data
dist <- table(NEI$year, NEI$type)  
# Results
#  YEAR   NON-ROAD NONPOINT ON-ROAD  POINT
#  1999   472362   102544  477883  55680
#  2002   618343   127240  890592  62502
#  2005   612129   127083  903983  70655
#  2008   621428   116892  911141 327194
# 
class(dist)

# Step 2: determine number of min yearly number of observations for each type of emmission
distdf <- as.data.frame(dist) #as dist is a Table, create a data.frame
distgrp <- group_by(distdf, Var2)
distgrpmin <- summarise(distgrp, min = min(Freq))
distgrpmin
# Result
#    Var2    min
# 1 NON-ROAD 472362
# 2 NONPOINT 102544
# 3  ON-ROAD 477883
# 4    POINT  55680

#3 Create datasets for each year and type combination
nonroad.1999 <-filter(NEI, year == 1999 & type == "NON-ROAD")
nonroad.2002 <-filter(NEI, year == 2002 & type == "NON-ROAD")
nonroad.2005 <-filter(NEI, year == 2005 & type == "NON-ROAD")
nonroad.2008 <-filter(NEI, year == 2008 & type == "NON-ROAD")
#
onroad.1999 <-filter(NEI, year == 1999 & type == "ON-ROAD")
onroad.2002 <-filter(NEI, year == 2002 & type == "ON-ROAD")
onroad.2005 <-filter(NEI, year == 2005 & type == "ON-ROAD")
onroad.2008 <-filter(NEI, year == 2008 & type == "ON-ROAD")
#
nonpoint.1999 <-filter(NEI, year == 1999 & type == "NONPOINT")
nonpoint.2002 <-filter(NEI, year == 2002 & type == "NONPOINT")
nonpoint.2005 <-filter(NEI, year == 2005 & type == "NONPOINT")
nonpoint.2008 <-filter(NEI, year == 2008 & type == "NONPOINT")
#
point.1999 <-filter(NEI, year == 1999 & type == "POINT")
point.2002 <-filter(NEI, year == 2002 & type == "POINT")
point.2005 <-filter(NEI, year == 2005 & type == "POINT")
point.2008 <-filter(NEI, year == 2008 & type == "POINT")

# Step 4: Create unique list of counties for each emmission type for 1999
# for nonroad emmissions
fipsnr99 <- unique(nonroad.1999$fips)
fipsnr99df <- as.data.frame(fipsnr99)
names(fipsnr99df) = c("fips")
fipsnr99df$fips <- as.character(fipsnr99df$fips)

# for road emmissions
fipsor99 <- unique(onroad.1999$fips)
fipsor99df <- as.data.frame(fipsor99)
names(fipsor99df) = c("fips")
fipsor99df$fips <- as.character(fipsor99df$fips)

# for non-point emmissions
fipsnp99 <- unique(nonpoint.1999$fips)
fipsnp99df <- as.data.frame(fipsnp99)
names(fipsnp99df) = c("fips")
fipsnp99df$fips <- as.character(fipsnp99df$fips)

# for point emmissions
fipsp99 <- unique(point.1999$fips)
fipsp99df <- as.data.frame(fipsp99)
names(fipsp99df) = c("fips")
fipsp99df$fips <- as.character(fipsp99df$fips)


# Step 5: create 1999 county corrected sets for each emmission type
nonroad1999 <- inner_join(fipsnr99df, nonroad.1999, by ="fips")
nonroad2002 <- inner_join(fipsnr99df, nonroad.2002, by ="fips")
nonroad2005 <- inner_join(fipsnr99df, nonroad.2005, by ="fips")
nonroad2008 <- inner_join(fipsnr99df, nonroad.2008, by ="fips")

onroad1999 <- inner_join(fipsor99df, onroad.1999, by ="fips")
onroad2002 <- inner_join(fipsor99df, onroad.2002, by ="fips")
onroad2005 <- inner_join(fipsor99df, onroad.2005, by ="fips")
onroad2008 <- inner_join(fipsor99df, onroad.2008, by ="fips")

nonpoint1999 <- inner_join(fipsnp99df, nonpoint.1999, by ="fips")
nonpoint2002 <- inner_join(fipsnp99df, nonpoint.2002, by ="fips")
nonpoint2005 <- inner_join(fipsnp99df, nonpoint.2005, by ="fips")
nonpoint2008 <- inner_join(fipsnp99df, nonpoint.2008, by ="fips")

point1999 <- inner_join(fipsp99df, point.1999, by ="fips")
point2002 <- inner_join(fipsp99df, point.2002, by ="fips")
point2005 <- inner_join(fipsp99df, point.2005, by ="fips")
point2008 <- inner_join(fipsp99df, point.2008, by ="fips")

# Step 6: create datasets corrected for number of observations for each emmission
# type based on the number of observations made for each emmission type in 1999

nonroad1999s <-nonroad1999[sample(nrow(nonroad1999), distgrpmin$min[1], replace=FALSE), ]
nonroad2002s <-nonroad2002[sample(nrow(nonroad2002), distgrpmin$min[1], replace=FALSE), ]
nonroad2005s <-nonroad2005[sample(nrow(nonroad2005), distgrpmin$min[1], replace=FALSE), ]
nonroad2008s <-nonroad2008[sample(nrow(nonroad2008), distgrpmin$min[1], replace=FALSE), ]

nonpoint1999s <-nonpoint1999[sample(nrow(nonpoint1999), distgrpmin$min[2], replace=FALSE), ]
nonpoint2002s <-nonpoint2002[sample(nrow(nonpoint2002), distgrpmin$min[2], replace=FALSE), ]
nonpoint2005s <-nonpoint2005[sample(nrow(nonpoint2005), distgrpmin$min[2], replace=FALSE), ]
nonpoint2008s <-nonpoint2008[sample(nrow(nonpoint2008), distgrpmin$min[2], replace=FALSE), ]

onroad1999s <-onroad1999[sample(nrow(onroad1999), distgrpmin$min[3], replace=FALSE), ]
onroad2002s <-onroad2002[sample(nrow(onroad2002), distgrpmin$min[3], replace=FALSE), ]
onroad2005s <-onroad2005[sample(nrow(onroad2005), distgrpmin$min[3], replace=FALSE), ]
onroad2008s <-onroad2008[sample(nrow(onroad2008), distgrpmin$min[3], replace=FALSE), ]

point1999s <-point1999[sample(nrow(point1999), distgrpmin$min[4], replace=FALSE), ]
point2002s <-point2002[sample(nrow(point2002), distgrpmin$min[4], replace=FALSE), ]
point2005s <-point2005[sample(nrow(point2005), distgrpmin$min[4], replace=FALSE), ]
point2008s <-point2008[sample(nrow(point2008), distgrpmin$min[4], replace=FALSE), ]

# Step 7: Use rbind to append datasets created in Step 6 
NEIAdj <- rbind(nonroad1999s,nonroad2002s,nonroad2005s,nonroad2008s,onroad1999s,onroad2002s,onroad2005s,onroad2008s,nonpoint1999s,nonpoint2002s,nonpoint2005s,nonpoint2008s,point1999s,point2002s,point2005s,point2008s)
str(NEIAdj)

# Step 8: check numbers of each emmission type is the same for each year
table(NEIAdj$year,NEIAdj$type)
# Results - number of observations balanced over the four years
# YEAR  NON-ROAD NONPOINT ON-ROAD  POINT
# 1999   472362   102544  477883  55680
# 2002   472362   102544  477883  55680
# 2005   472362   102544  477883  55680
# 2008   472362   102544  477883  55680

# Step 8: Generate average PM25 for each year
NEIAdjByYear <- group_by(NEIAdj, year)
NEIPM25Adj <- summarise(NEIAdjByYear, avgPM25 = mean(Emissions))
# Print out NEIPM25Adj dataset
NEIPM25Adj

# Step 9 Generate plot of year by the Average PM25 based on data
# corrected for counties that were measured in all years and 
# corrected for number of measurements per emission source type in each 
# of these years

png(filename = "plot1.png", width = 480, height = 480, units = "px")
par(mfcol = c(1,1), mar = c(5,4,3,1))
with(NEIPM25Adj, plot(year,avgPM25, xlab = "", 
    ylab ="Avg Particulate matter emitted per source(tons/yr)", ylim = c(0,8), type = "n"))
with(NEIPM25Adj, points(year,avgPM25, pch = 20, type = "b", col="blue"))
title (main = "Average Fine Particulates Emitted (PM25) per Source\n in US between 1999 and 2008",
       sub = "This graph was generated after correcting for County\n and Number of observations per source") 
dev.off()



