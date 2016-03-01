plot2 <- function() {
  
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
  
  # Make plot
  png(filename = "plot2.png", width = 480, height = 480)
  plot_2 <- function(name) {
    with(power_clean, plot(DateTime, as.numeric(as.character(Global_active_power)), 
                           type = "l", xlab = "", 
                           ylab = name))
  }
  plot_2("Global Active Power (kilowatts)")
  dev.off()
  
}