#' Launch PWS Shiny app
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description This is a test hook to start the shiny app
#' @export
pwsWebapp <- function() {
  data(sysdata,envir=environment())
  shiny::runApp(system.file("shiny", package="ProjectPWS"))
}