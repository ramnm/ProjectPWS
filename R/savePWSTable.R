##
## Author - Maruthi Ram Nadakuduru
## Description - This function saves the PWS table created by user in his 
## working directory
##
savePWSTable <- function(pwsTable,fileName){
  fileName <- paste0("~/",fileName)
  saveRDS(pwsTable, file=fileName)        
}