#' Save PWS Table
#' @author Maruthi Ram Nadakuduru
#' @description This function saves the PWS table created by user in the specified location.
#' @param pwsTable The PWStation object to save.
#' @param fileName The location where it is to be saved.
#' @export
savePWSTable <- function(pwsTable, fileName) {
  saveRDS(pwsTable, file = fileName)
}