#part02
#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The
#function should return a data frame where the first column is the name of the file and the second column is the number of 
#complete cases.

complete <- function(directory, id = 1:332){
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
  
  nobs <- c()
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
    
    #number of complete cases
    complete_cases <- complete.cases(df)
    nobs <- append(nobs, sum(complete_cases))
  }
  return(data.frame(id=id, nobs=nobs))
}

