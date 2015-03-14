#' Load PWS Table
#' @author Maruthi Ram Nadakuduru
#' @description This function reads a saved PWS table created by user from
#'              the specified location.
#' @export
readPWSTable <- function(fileName){
  readRDS(fileName)
}