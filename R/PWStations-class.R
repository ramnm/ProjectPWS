#' Function defining the PWStations R6 class
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description This function includes all the definitions of the PWStations
#'              R6 class.
#' @import methods data.table R6
#' @exportClass PWStations
## @examples
## \dontrun{plotQuakes("red")}
##
PWStations <- R6::R6Class("PWStations",
  public = list(
    center = NA,
    range = NA,
    state = NA,
    country = NA,
    stations = NA,
    initialize = function(center,range,state,country,stations) {
      if (is.na(center) &
          is.na(state) &
          is.na(country)) {
        return("Please enter either zip, state or country code")
      }
      if (!is.na(center)) {
        if (!any(zipcodes[zipcodes$zip==center,1])) {
          return("Please enter a valid zip code")
        } else {
          self$center <- center
          self$range <- range
        }
      }
#
      if (!is.na(state)) {self$state <- state}
#
      if (!is.na(country)) {self$country <- country}
    }
  )
)