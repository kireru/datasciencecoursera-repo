library(plyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI.24510 <- NEI[ which(NEI$fips == "24510"),]
datasum <- with(NEI.24510, aggregate(Emissions, by = list(year), sum))
colnames(datasum) <- c("year", "Emissions")


plot(aggregate.24510, 
     type="o", 
     ylab = expression('Total Emissions, PM'[2.5]), 
     xlab = 'Year', 
     main = 'TOTAL EMISSIONS FOR  BALTMORE',
     xlim = c(1999, 2008))

dev.copy(png, 'Plot2.png')
dev.off()

