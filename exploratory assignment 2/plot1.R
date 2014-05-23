library(plyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

set.seed(1234)
NEI.reduced <- NEI[sample(nrow(NEI), 500), ]
aggregate.data <- with(NEI, aggregate(Emissions, by = list(year), sum))


plot(aggregate.data, 
     type="o", 
     ylab = expression('Total Emissions, PM'[2.5]), 
     xlab = 'Year', 
     main = 'Total Emissions in the United States', border="blue")

dev.copy(png, 'Plot1.png')
dev.off()
