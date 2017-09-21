plot2 <- function(file_name) {
  
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
  
  # Making plot
  
  plot(tab$Date,tab$Global_active_power, type = "l",
                    xlab = "",
                    ylab = "Global Active Power (kilowatts)")
  
  dev.copy(png, file = "plot2.png")
  
  dev.off()
  
  return(TRUE)
  
}