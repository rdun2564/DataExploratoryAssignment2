library(dplyr)
library(ggplot2)
setwd("~/RData3/DEAssignment2")

# Question 6 : Compare emissions from motor vehicles in Baltimore City with those in LA over time
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Step 1: Generate generate SCC dataset with Vehicles SCC codes for merging with NEI
SCCv2 <-SCC[grep("[Vv]ehicle", SCC$SCC.Level.Two),]
View(SCCv2) # demonstrates that SCC$SCC.Level.Two captures all motor vehicle sources within Level.Three and Level.Four

SCCv2$SCC <- as.character(SCCv2$SCC)
NEIv <- inner_join(NEI,SCCv2, by = "SCC")

# Step 2: Filter out data for Baltimore City and LA County
NEIvBCLA <- filter(NEIv, fips == "24510" | fips == "06037")

# Step 3: Create dataset with average PM25 emissions from vehicles by Year and fips
NEIvGrpYearFips <- group_by(NEIvBCLA, year, fips)
NEIvPM25 <- summarise(NEIvGrpYearFips, avgPM25 = mean(Emissions))

# Step 4: Subset data = fips = "24510"
NEIvPM25BC <- filter(NEIvPM25, fips == "24510")
NEIvPM25BC

# Step 5: Plot PM25 by year for Baltimore and LA County
png(filename = "plot6.png", width = 480, height = 480, units = "px")
par(mfcol = c(1,1), mar = c(5,4,3,1))
with(NEIvPM25, plot(year,avgPM25, xlab = "", 
                      ylab ="Avg PM25 emitted by vehicles(tons/yr)", ylim = c(0,45), type = "n"))
with(subset(NEIvPM25, NEIvPM25$fips == "06037"), points(year,avgPM25, pch = 20, type = "b", col="blue"))
with(subset(NEIvPM25, NEIvPM25$fips == "24510"), points(year,avgPM25, pch = 20, type = "b", col="red"))
title (main = "Average Fine Particulates Emitted (PM25) by Motor Vehicles \n in Baltimore City and LA County between 1999 and 2008")
legend("topright", pch = 20, col=c("blue","red"), legend = c("LA County", "Baltimore City"))
dev.off()


