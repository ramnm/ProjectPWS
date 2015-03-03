#' This is a test hook to start the shiny app
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description This is a test hook to start the shiny app
#' @export
startShiny <- function() {
 data(sysdata,envir=environment())
 shiny::runApp(system.file("webapp", package="ProjectPWS"))
}