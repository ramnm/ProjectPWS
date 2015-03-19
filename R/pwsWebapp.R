##
##  Begin jcasale code
##
#' Launch PWS Shiny app
#' @author Jared Casale
#' @description This is a test hook to start the shiny app
#' @export
pwsWebapp <- function() {
 shiny::runApp(system.file("shiny", package="ProjectPWS"))
}
##
##  End jcasale code
##