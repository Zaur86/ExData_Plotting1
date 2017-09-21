plot1 <- function(file_name) {
  
  # Checking existing of the file
  
  if (!file.exists(file_name)) return(FALSE)
  
  # Set libraries
  
  library(dplyr)
  
  # Loading and cleaning the data
  
  tab <- read.delim(file_name, sep = ";")
  
  tab <- subset(tab,(Date == "1/2/2007") | (Date == "2/2/2007"))
  
  tab$Date <- tab$Date %>% as.character() %>% as.factor()
  
  tab$Global_active_power <- tab$Global_active_power %>% 
                                as.character() %>% as.numeric()
  
  # Making plot
  
  hist(tab$Global_active_power, col = "red",
                    main = "Clobal Active Power",
                    xlab = "Global Active Power (kilowatts)")
  
  dev.copy(png, file = "plot1.png")
  
  dev.off()
  
  return(TRUE)
  
}