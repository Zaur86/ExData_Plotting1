plot4 <- function(file_name) {
  
  # Checking existing of the file
  
  if (!file.exists(file_name)) return(FALSE)
  
  # Set libraries
  
  library(dplyr)
  library(stringr)
  
  # Loading and cleaning the data
  
  tab <- read.delim(file_name, sep = ";")
  
  tab <- subset(tab,(Date == "1/2/2007") | (Date == "2/2/2007"))
  
  tab$Date <- paste0(substr(tab$Date,5,8),"-0",
                     substr(tab$Date,3,3), "-0", 
                     substr(tab$Date,1,1), " ", 
                     as.character(tab$Time))
  
  tab$Date <- strptime(tab$Date, format = "%Y-%m-%d %H:%M:%S")
  
  tab$Global_active_power <- tab$Global_active_power %>% 
                                as.character() %>% as.numeric()
  
  tab$Sub_metering_1 <- tab$Sub_metering_1 %>% 
                                as.character() %>% as.numeric()
  
  tab$Sub_metering_2 <- tab$Sub_metering_2 %>% 
                                as.character() %>% as.numeric()
  
  tab$Sub_metering_3 <- tab$Sub_metering_3 %>% 
                                as.character() %>% as.numeric()
  
  tab$Voltage <- tab$Voltage %>% 
                                as.character() %>% as.numeric()
  
  tab$Global_reactive_power <- tab$Global_reactive_power %>% 
                                as.character() %>% as.numeric()
  
  
  # Making plot
  
  par(mfrow=c(2,2))
  
  plot(tab$Date, tab$Global_active_power, type = "l",
       xlab = "",
       ylab = "Global Active Power")
  
  plot(tab$Date, tab$Voltage, type = "l",
       xlab = "datetime",
       ylab = "Voltage")
  
  plot(tab$Date,tab$Sub_metering_1, type = "l",
                    xlab = "",
                    ylab = "Energy sub metering")
  
  points(tab$Date, tab$Sub_metering_2, col = "red", type = "l")
  
  points(tab$Date, tab$Sub_metering_3, col = "blue", type = "l")
  
  legend("topright", lty = c(1,1,1), 
              col = c("black","red","blue"), 
                legend = c("Sub_metering_1","Sub_metering_2",
                         "Sub_metering_3"))
  
  plot(tab$Date, tab$Global_reactive_power, type = "l",
       xlab = "datetime",
       ylab = "Global_reactive_power")
  
  dev.copy(png, file = "plot4.png")
  
  dev.off()
  
  return(TRUE)
  
}