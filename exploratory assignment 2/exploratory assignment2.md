Peer Assessment 2: Exploratory Data Analysis
============================================
 Introduction
=============
Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the EPA National Emissions Inventory web site.

For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year. The data that will be use for this assignment are for 1999, 2002, 2005, and 2008.


```r
library(plyr)
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
```


The goal
=======

The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999-2008. You may use any R package you want to support your analysis.


```r
set.seed(12345)
NEI.reduced <- NEI[sample(nrow(NEI), 500), ]
```


As a summary,all the data for plots below is also represented in single plot.r for the six plots separately

 plot 1.
=======

```r
library(plyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

aggregate.data <- with(NEI, aggregate(Emissions, by = list(year), sum))


plot(aggregate.data, type = "o", ylab = expression("Total Emissions, PM"[2.5]), 
    xlab = "Year", main = "Total Emissions in the United States")
polygon(aggregate.data, col = "green", border = "blue")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


It is clear that  total emissions from PM2.5 have decreased in the United States from 1999 to 2008

plot 2
======

```r
library(plyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI.24510 <- NEI[which(NEI$fips == "24510"), ]
aggregate.24510 <- with(NEI.24510, aggregate(Emissions, by = list(year), sum))
colnames(aggregate.24510) <- c("year", "Emissions")


plot(aggregate.24510, type = "o", ylab = expression("Total Emissions, PM"[2.5]), 
    xlab = "Year", main = "Total Emissions for Baltimore County", xlim = c(1999, 
        2008))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Total emissions from PM2.5 have decreased in the Baltimore City, Maryland from 1999 to 2008 however there was an increase of PM2.5 between the year 2002 and 2006. 

PLOT3
=====

Sources have seen decreases in emissions from 1999-2008 for Baltimore City Which have seen increases in emissions from 1999-2000


```r
library(ggplot2)
library(plyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI.24510 <- NEI[which(NEI$fips == "24510"), ]
aggregate.24510 <- with(NEI.24510, aggregate(Emissions, by = list(year), sum))
colnames(aggregate.24510) <- c("year", "Emissions")

NEI.24510.type <- ddply(NEI.24510, .(type, year), summarize, Emissions = sum(Emissions))
NEI.24510.type$Pollutant_Type <- NEI.24510.type$type

qplot(year, Emissions, data = NEI.24510.type, group = Pollutant_Type, color = Pollutant_Type, 
    geom = c("point", "line"), ylab = expression("Total Emissions, PM"[2.5]), 
    xlab = "Year", main = "Total Emissions in U.S. by Type of Pollutant")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


plot4
====
How emissions from coal combustion-related sources changed from 1999-2008


```r
library(lattice)
library(plyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC.coal <- grep("coal", SCC$Short.Name, ignore.case = TRUE)
SCC.coal <- SCC[SCC.coal, ]
SCC.identifiers <- as.character(SCC.coal$SCC)

NEI$SCC <- as.character(NEI$SCC)
NEI.coal <- NEI[NEI$SCC %in% SCC.identifiers, ]

aggregate.coal <- with(NEI.coal, aggregate(Emissions, by = list(year), sum))
colnames(aggregate.coal) <- c("year", "Emissions")

plot(aggregate.coal, type = "o", ylab = expression("Total Emissions, PM"[2.5]), 
    xlab = "Year", main = "Emissions and Total Coal Combustion for the United States", 
    xlim = c(1999, 2008))
polygon(aggregate.coal, col = "red", border = "red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


plot5
====
How emissions from motor vehicle sources changed from 1999-2008 in Baltimore City


```r
library(ggplot2)
library(plyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC.motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
SCC.motor <- SCC[SCC.motor, ]
SCC.identifiers <- as.character(SCC.motor$SCC)
NEI$SCC <- as.character(NEI$SCC)
NEI.motor <- NEI[NEI$SCC %in% SCC.identifiers, ]

NEI.motor.24510 <- NEI.motor[which(NEI.motor$fips == "24510"), ]

aggregate.motor.24510 <- with(NEI.motor.24510, aggregate(Emissions, by = list(year), 
    sum))

plot(aggregate.motor.24510, type = "o", ylab = expression("Total Emissions, PM"[2.5]), 
    xlab = "Year", main = "Total Emissions from Motor Vehicle Sources")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


plot6
=====

motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California have decreased.The city which has seen greater changes over time in motor vehicle emissions is los angeles county


```r
library(plyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


SCC.motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
SCC.motor <- SCC[SCC.motor, ]
SCC.identifiers <- as.character(SCC.motor$SCC)
NEI$SCC <- as.character(NEI$SCC)
NEI.motor <- NEI[NEI$SCC %in% SCC.identifiers, ]
NEI.motor.24510 <- NEI.motor[which(NEI.motor$fips == "24510"), ]
aggregate.motor.24510 <- with(NEI.motor.24510, aggregate(Emissions, by = list(year), 
    sum))


SCC.motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
SCC.motor <- SCC[SCC.motor, ]
SCC.identifiers <- as.character(SCC.motor$SCC)


NEI$SCC <- as.character(NEI$SCC)
NEI.motor <- NEI[NEI$SCC %in% SCC.identifiers, ]

NEI.motor.24510 <- NEI.motor[which(NEI.motor$fips == "24510"), ]
NEI.motor.06037 <- NEI.motor[which(NEI.motor$fips == "06037"), ]

aggregate.motor.24510 <- with(NEI.motor.24510, aggregate(Emissions, by = list(year), 
    sum))
aggregate.motor.24510$group <- rep("Baltimore County", length(aggregate.motor.24510[, 
    1]))


aggregate.motor.06037 <- with(NEI.motor.06037, aggregate(Emissions, by = list(year), 
    sum))
aggregate.motor.06037$group <- rep("Los Angeles County", length(aggregate.motor.06037[, 
    1]))

aggregated.motor.zips <- rbind(aggregate.motor.06037, aggregate.motor.24510)
aggregated.motor.zips$group <- as.factor(aggregated.motor.zips$group)

colnames(aggregated.motor.zips) <- c("Year", "Emissions", "Group")

qplot(Year, Emissions, data = aggregated.motor.zips, group = Group, color = Group, 
    geom = c("point", "line"), ylab = expression("Total Emissions, PM"[2.5]), 
    xlab = "Year", main = "Comparison of Total Emissions by County")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


