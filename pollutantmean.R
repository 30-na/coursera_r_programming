#Part 1
#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of 
#monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID 
#numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument 
#and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. 

pollutantmean <- function(directory, pollutant, id = 1:332){
  add_zero <- function(n) {
    if (n < 10) {
      n <- paste('00', n, sep = '')
    }
    else if (n < 100) {
      n <- paste('0', n, sep = '')
    }
    else{
      n <- paste(n, sep = '')
    }
  }
  
  sum_data <- c()
  for (i in id) {
    #check for valid id
    if (i < 1) {
      print ('Invalid id number')
    }
    
    #open CVS file
    working_directory <- getwd()
    file_name <- paste(add_zero(i), '.csv', sep = '')
    path <- file.path(working_directory, directory, file_name)
    df <-  read.table(path, header = TRUE, sep = ',', skipNul=TRUE)
    
    #ignore missing values
    missing_data <- is.na(df[pollutant])
    available_data <- df[pollutant][!missing_data]
    
    #add all data
    sum_data <- append(sum_data, available_data)
  }
  return(mean(sum_data))
}



