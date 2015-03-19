##
##  Begin jcasale code
##
#' Create subtable of PWS
#' @author Jared Casale
#' @description Returns a PWStations object with the specified filter applied
#'              to the original PWStations. If the original object contained
#'              weather data, this will also be filtered to the appropriate
#'              stations.
#' Filter options are one of the following:
#' \itemize{
#' \item Reducing the radius of the original query, so that only stations
#' \item within the new radius are kept.
#' \item Choosing a number of closest or farthest stations from the original
#' \item query location.
#' \item Choosing a set of stations by name.
#' }
#' @param pwStations A PWStations object with the data to be filtered.
#' @param newRadius A positive numeric value representing the max distance_mi.
#'        Anything greater or equal to the original radius will result in no
#'        changes.
#' @param numberToKeep An integer value specifying the number of stations to
#'        keep, sorted by distance.
#' @param nearest Boolean value indicating if stations should be kept
#'        according to the nearest(sorted by increasing distance_mi) or
#'        farthest (sorted by decreasing distance_mi). Default is TRUE.
#' @param stationNames A vector of the names/IDs of specific stations in the
#'        table.
#' @return A PWStations object representing the result of applying the
#'         specified criteria to the original object.
#' @seealso \code{\link{getStations}} for a method that will generate an object.
#'          See \code{\link{loadWeatherData}} to load weather data for the
#'          selected PWS stations.
#' @export makeStationSubtable
#' @examples
#' data(charlotteStations)
#' # Decrease radius to 5 miles
#' newRadTable <- makeStationSubtable(charlotteStations,
#'                                    newRadius = 5)
#' newRadTable$stations
#'
#' # Keep 3 closest
#' closestTable <- makeStationSubtable(charlotteStations,
#'                                     numberToKeep = 3)
#' closestTable$stations
#'
#' # Keep 3 farthest
#' farthestTable <- makeStationSubtable(charlotteStations,
#'                                      numberToKeep = 3,
#'                                      nearest = FALSE)
#' farthestTable$stations
#'
#' # Keep specific stations
#' specificTable <- makeStationSubtable(charlotteStations,
#'                                      stationNames = c("KNCCHARL71",
#'                                                       "KNCCHARL83"))
#' specificTable$stations
makeStationSubtable <- function(pwStations, newRadius = NA, numberToKeep = NA,
                                nearest = TRUE, stationNames = NA) {
  if (all(is.na(newRadius),
       is.na(numberToKeep),
       is.na(stationNames))) {
    stop("Please specify a filter criteria.")
  }

  if (is.null(pwStations$stations) || nrow(pwStations$stations) < 1) {
    stop("Please specify a PWStations object with at least 1 station.")
  }

  returnRadius <- pwStations$radius
  returnStations <- NA

  if (!is.na(newRadius)) {
    if (!is.numeric(newRadius) || newRadius < 0) {
      stop("Please specify a positive numeric value for newRadius.")
    }

    returnStations <-
       pwStations$stations[pwStations$stations$distance_mi <= newRadius,]
    returnRadius <- newRadius
  } else if (!is.na(numberToKeep)) {
      if (!is.numeric(numberToKeep) || as.integer(numberToKeep) < 1 ||
           as.integer(numberToKeep) >= nrow(pwStations$stations)) {
        stop(paste0("Please specify a numberToKeep of at least 1 and less than",
                  "the number of stations provided."))
      }

      returnStations <- head(pwStations$stations[order(
                                     pwStations$stations$distance_mi,
                                     decreasing = !nearest), ],
                             n = as.integer(numberToKeep))
  } else {
    if (!is.character(stationNames)) {
      stop("Please specify a character vector of station names.")
    }

    returnStations <-
           pwStations$stations[pwStations$stations$id %in% stationNames, ]
  }

  # Copy the original
  returnObj <- PWStations$new(latlong = pwStations$latlong,
                              zip = pwStations$zip,
                              state = pwStations$state,
                              country = pwStations$country,
                              city = pwStations$city,
                              radius = returnRadius)

  returnObj$stations <- returnStations
  returnObj$queryArg <- pwStations$queryArg
  returnObj$weatherData <- NULL

  if (!is.null(pwStations$weatherData)) {
    returnObj$weatherData <- pwStations$weatherData[returnStations$id]
  }

  returnObj
}
##
##  End jcasale code
##