plot4 <- function() {
  
  library(lubridate)
  
  # Download data from internet and unzip
  download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                destfile = "power.zip", method = "curl")
  unzip(zipfile = "power.zip")
  
  # Make sure your computer has enough memory
  computer_memory <- 8*1E9
  df_size <- object.size(read.csv(file = "household_power_consumption.txt", header = TRUE, sep = ";"))
  if (computer_memory - df_size < 0) {
    stop("Insufficient memory")
  }
  
  # Read data into power data frame
  power <- read.csv(file = "household_power_consumption.txt", 
                    header = TRUE, sep = ";")
  power$Date <- dmy(power$Date)
  
  # Only use data from the dates 2007-02-01 and 2007-02-02
  power_subset <- subset(power, 
                         Date == ymd("2007-02-01") | Date == ymd("2007-02-02"))
  
  # Convert dates and times into one variable DateTime
  power_subset$Date <- ymd_hms(paste(power_subset$Date, power_subset$Time))
  names(power_subset)[1] <- "DateTime"
  power_clean <- subset(power_subset, select = -Time)
  
  # Plot 2
  plot_2 <- function(name) {
    with(power_clean, plot(DateTime, as.numeric(as.character(Global_active_power)), 
                           type = "l", xlab = "", 
                           ylab = name))
  }

  # Plot 3
  plot_3 <- function(bty_selection) {
    with(power_clean, plot(DateTime, as.numeric(as.character(Sub_metering_1)), 
                           type = "l", xlab = "", 
                           ylab = "Energy sub metering"))
    with(power_clean, lines(DateTime, as.numeric(as.character(Sub_metering_2)), 
                            col = "red"))
    with(power_clean, lines(DateTime, as.numeric(as.character(Sub_metering_3)), 
                            col = "blue"))
    legend("topright", lty = 1, seg.len = 1, legend = c("Sub_metering_1", 
                                                        "Sub_metering_2", 
                                                        "Sub_metering_3"),
           col = c("black", "red", "blue"), bty = bty_selection)
  }
  
  # Make plot
  png(filename = "plot4.png", width = 480, height = 480)
  plot_4 <- function() {
    par(mfrow = c(2,2))
    plot_2(name = "Global Active Power")
    with(power_clean, plot(DateTime, as.numeric(as.character(Voltage)), 
                           type = "l", xlab = "datetime", ylab = "Voltage"))
    plot_3("n")
    with(power_clean, plot(DateTime, as.numeric(as.character(Global_reactive_power)), 
                           type = "l", xlab = "datetime", 
                           ylab = "Global_reactive_power"))
  }
  plot_4()
  dev.off()
  
}