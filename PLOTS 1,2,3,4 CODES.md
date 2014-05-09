for all the codes below make sure that the working directory has ("household_power_consumption.txt") data unzipped

# plot1.R - Histogram of Global Active Power


# Read data, convert dates and subset on two days in February 2007
url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]
date <- as.character(as.Date(subset$Date, "%d/%m/%Y"))
x <- paste(date, subset$Time)
dateTime <- strptime(x, "%Y-%m-%d %H:%M:%S")
# PNG dimensions

png("plot1.png", height=480, width=480)
# plotting histogram

hist(as.numeric(subset$Global_active_power), col="red",
main = "Global Active Power",xlab="Global Active Power (kilowatts)")
head(as.Date(subset$Date))
# Close PNG file
dev.off()


Plot 2


# Read data, convert dates and subset on two days in February 2007
url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

date <- as.character(as.Date(subset$Date, "%d/%m/%Y"))
x <- paste(date, subset$Time)
dateTime <- strptime(x, "%Y-%m-%d %H:%M:%S")
# PNG dimensions
png("plot2.png", width = 480, height = 480)
# plotting 
plot(dateTime, as.numeric(subset$Global_active_power), type="l",
     ylab="Global Active Power (kilowatts)", xlab="")
# Close PNG file

dev.off()
 

Plot 3


# plot3.R - Time Series of Global Active Power by submeter

# Read data, convert dates and subset on two days in February 2007
url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

date <- as.character(as.Date(subset$Date, "%d/%m/%Y"))
x <- paste(date, subset$Time)
dateTime <- strptime(x, "%Y-%m-%d %H:%M:%S")
# PNG dimensions
png("plot3.png", height=480, width=480)
# plotting 
plot(dateTime, as.numeric(subset$Sub_metering_1), type="l",
ylab="Energy Submetering", xlab="")
lines(dateTime, as.numeric(subset$Sub_metering_2), type="l", col="red")
lines(dateTime, as.numeric(subset$Sub_metering_3), type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd =2.5,
col=c("black", "red", "blue"), cex=0.75)
# Close PNG file
dev.off()

 
Plot 4

# plot4.R 
# Read data, convert dates and subset on two days in February 2007
url = "household_power_consumption.txt"
data <- read.table(url, header=TRUE, nrows=2100000, sep=";", stringsAsFactors =FALSE)
subset <- data[data$Date %in% c("1/2/2007","2/2/2007") ,]

date <- as.character(as.Date(subset$Date, "%d/%m/%Y"))
x <- paste(date, subset$Time)
dateTime <- strptime(x, "%Y-%m-%d %H:%M:%S")
# PNG dimensions
png("plot4.png", width = 480, height = 480)
# plotting 
par(mfrow = c(2, 2)) 
plot(dateTime, as.numeric(subset$Global_active_power), type="l",
     ylab="Global Active Power", xlab="", cex=0.2)
plot(dateTime, as.numeric(subset$Voltage), type="l",
     ylab="Voltage", xlab="datetime")
plot(dateTime, as.numeric(subset$Sub_metering_1), type="l",
     ylab="Energy Submetering", xlab="")
lines(dateTime, as.numeric(subset$Sub_metering_2), type="l", col="red")
lines(dateTime, as.numeric(subset$Sub_metering_3), type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd =2.5,
       col=c("black", "red", "blue"), bty = "n")
plot(dateTime, as.numeric(subset$Global_reactive_power), type="l", xlab="datetime",
     ylab="Global_reactive_power")
# Close PNG file
dev.off()
