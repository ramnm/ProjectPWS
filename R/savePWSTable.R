#' Save PWS Table
#' @author Maruthi Ram Nadakuduru
#' @description This function saves the PWS table created by user in the specified location
#' @export
savePWSTable <- function(pwsTable, fileName) {
  saveRDS(pwsTable, file = fileName)
}