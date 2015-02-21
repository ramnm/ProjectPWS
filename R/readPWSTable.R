##
## Author - Maruthi Ram Nadakuduru
## Description - This function reads a saved PWS table created by user from his 
## working directory
##
readPWSTable <- function(fileName){
  pwsTable <- readRDS(fileName)        
}