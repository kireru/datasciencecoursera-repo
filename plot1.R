url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
date <- as.character(as.Date(subset$Date, "%d/%m/%Y"))
x <- paste(date, subset$Time)
dateTime <- strptime(x, "%Y-%m-%d %H:%M:%S")

png("plot1.png", height=480, width=480)

hist(as.numeric(subset$Global_active_power), col="red",
main = "Global Active Power",xlab="Global Active Power (kilowatts)")
head(as.Date(subset$Date))
dev.off()
