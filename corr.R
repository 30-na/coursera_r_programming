#part03
#Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between
#sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the
#threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no
#monitors meet the threshold requirement, then the function should return a numeric vector of length 0.

source("complete.R")

corr <- function(directory, threshold = 0){
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
  
  corr_vector <- c()
  for (i in 1:332) {
    #open CVS file
    working_directory <- getwd()
    file_name <- paste(add_zero(i), '.csv', sep = '')
    path <- file.path(working_directory, directory, file_name)
    df <-  read.table(path, header = TRUE, sep = ',', skipNul=TRUE)
    
    #check the threshold
    if (complete(directory, id = i)['nobs'] > threshold){
      good <- complete.cases(df)
      #print(df[good,'sulfate'])
      #print(df[good, 'nitrate'])
      good_sulfate <- df[good,'sulfate']
      good_nitrate <- df[good, 'nitrate']
      
      corr <- cor(df[good,'sulfate'], df[good, 'nitrate'])
      corr_vector <- append(corr_vector, corr)
      }
    }
   return(corr_vector) 
  }


