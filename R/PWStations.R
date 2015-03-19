##
##  Begin nmram code
##
#' PWS Storage
#' @docType class
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description This class holds the result of a WUnderground API query.
#'              The parameters imply the query used (i.e. if zip is set, a zip
#'              lookup was performed) and the resulting stations are stored in
#'              a stations data table.
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords PWS, Wunderground
#' @export PWStations
#' @seealso \code{\link{getStations}} for a method that will generate an
#'          object. See \code{\link{loadWeatherData}} to load weather
#'          data for the selected PWS stations.
PWStations <- R6::R6Class("PWStations",
  public = list(
    latlong = NA,
    zip = NA,
    state = NA,
    country = NA,
    city = NA,
    radius = NA,
    queryArg = NA,
    stations = NULL,
    weatherData = NULL,
    startDate = NA,
    endDate = NA,
    startHour = NA,
    endHour = NA,
    weatherVars = NA,
##
##  End nmram code
##
##
##  Begin jcasale code
##
    initialize = function(latlong = NA, zip = NA, state = NA,
                          country = NA, city = NA, radius = NA) {
      if (all(is.na(latlong),
              is.na(zip),
              is.na(state),
              is.na(country),
              is.na(city))) {
        stop("A valid location must be specified.")
      }

      # Radius must be numeric if specified
      if (!is.na(radius) && !is.numeric(radius)) {
        stop("Radius must be numeric.")
      }

      # Check the bounds and default to just outside the max (25)
      if (is.na(radius) || radius < 0 || radius > 24) {
        radius <- 25
      }

      # Build the query argument as we check
      queryArg <- NA

      # Check latlong
      if (!any(is.na(latlong))) {
        # Lat/long specified
        if (!is.numeric(latlong) || length(latlong) != 2) {
          stop("Latlong must be two numeric values.")
        }

        queryArg <- paste0(latlong[1], ",", latlong[2])
      }

      # Check zipcode
      if (!is.na(zip)) {
        if (!is.na(queryArg)) {
          stop(paste0("Please specify a single location parameter (latlong, ",
                      "zipcode, state, country, state and city or country and",
                      "city"))
        }

        if (!is.character(zip) || nchar(zip) != 5) {
          stop("Please specify a valid zipcode string.")
        }

        queryArg <- zip
      }

      # Check for a state
      if (!is.na(state)) {
        if (!is.na(queryArg)) {
          stop(paste0("Please specify a single location parameter (latlong, ",
                      "zipcode, state, country, state and city or country and",
                      " city"))
        }

        if (!is.character(state) || nchar(state) != 2) {
          stop("Please specify state as a length 2 character string.")
        }

        queryArg <- state
      }

      # Check for a country
      if (!is.na(country)) {
        if (!is.na(queryArg)) {
          stop(paste0("Please specify a single location parameter (latlong,",
                      " zipcode, state, country, state and city or country and",
                      "city"))
        }

        if (!is.character(country)) {
          stop("Please specify country as a chracter string.")
        }

        queryArg <- country
      }

      # Check for a city
      if (!is.na(city)) {
        if (!is.character(city)) {
          stop("Please specify city as a chracter string.")
        }

        if (is.na(state) && is.na(country)) {
          stop("Please specify a state or country along with the city.")
        }

        region <- ifelse(is.na(state), country, state)
        queryArg <- paste0(region, "/", city)
      }

      self$latlong <- latlong
      self$zip <- zip
      self$state <- state
      self$country <- country
      self$city <- city
      self$radius <- radius
      self$queryArg <- queryArg
    },
##
##  End jcasale code
##
##
##  Begin nmram code
##
    plotStations = function () {
      stn <- self$stations
      hist(stn$distance_mi,
           main = "Histogram of Stations",
           xlab = "Distance in Miles",
           ylab = "No. of Stations",
           col = "dark green")
    }
  )
)
##
##  End nmram code
##