plot3 <- function(file_name) {
  
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
  
  tab$Sub_metering_1 <- tab$Sub_metering_1 %>% 
                                as.character() %>% as.numeric()
  
  tab$Sub_metering_2 <- tab$Sub_metering_2 %>% 
                                as.character() %>% as.numeric()
  
  tab$Sub_metering_3 <- tab$Sub_metering_3 %>% 
                                as.character() %>% as.numeric()
  
  # Making plot
  
  plot(tab$Date,tab$Sub_metering_1, type = "l",
                    xlab = "",
                    ylab = "Energy sub metering")
  
  points(tab$Date, tab$Sub_metering_2, col = "red", type = "l")
  
  points(tab$Date, tab$Sub_metering_3, col = "blue", type = "l")
  
  legend("topright", lty = c(1,1,1), 
              col = c("black","red","blue"), 
                legend = c("Sub_metering_1","Sub_metering_2",
                         "Sub_metering_3"))
  
  dev.copy(png, file = "plot3.png")
  
  dev.off()
  
  return(TRUE)
  
}