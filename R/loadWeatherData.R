#' Load PWS Weather Data
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description This function will query the wunderground site and retrieve the weather data for each PW Station, over the specified time period.
#' @param pwStations PWStation object containing the stations that require data.
#' @param startDate Beginning of time period of interest. Must be a string with format mm/dd/YYYY.
#' @param endDate End of time period of interest. If specified, must be a string with format mm/dd/YYYY.
#' @param weatherVars Variables to retrieve, must be one of "tempi" (temperature imperial i.e. Fahrenheit), "hum" (humidity)
#' @param stationLimit This restricts to the first stationLimit stations. -1 signals to retrieve all in the table.
#' @return List of data frames, one for each weather station in the original PWStations object. The name of each element is the station's ID.
#' Note that this is also accessible from the supplied PWStations object.
#' @export
#' @examples
#' \dontrun{
#'  stations <- getStations(zip = "98107", radius = 2)
#'  weatherData <- loadWeatherData(stations, startDate = "3/3/2015", stationLimit = 1)
#'  weatherData2 <- loadWeatherData(stations, startDate = "3/1/2015", endDate = "3/3/2015", stationLimit = 3)
#' }
loadWeatherData <- function(pwStations, startDate, endDate = NA, weatherVars = c("tempi", "hum"), stationLimit = -1L) {
  # Check pwStations object
  if (!all(class(pwStations) == c("PWStations", "R6"))) {
    stop("pwStations must be a PWStations object.")
  }

  # Get the stations - is it empty?
  stationsTable <- stations$stations

  if (is.na(stationsTable) || nrow(stationsTable) == 0) {
    stop("Need at least one PW Station.")
  }

  if (stationLimit > 0) {
    stationsTable <- stationsTable[1:stationLimit, ]
  }

  stationIds <- stationsTable$id

  # Check startDate, is it a date?
  if (is.na(startDate) || !is.character(startDate)) {
    stop("startDate must be specified as a character string.")
  }

  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  if (is.na(startDate)) {
    stop("invalid startDate specified.")
  }

  # Check endDate, is it specified, is it a date?
  if (is.na(endDate)) {
    endDate <- Sys.Date()
  } else if (!is.character(endDate)) {
    stop("endDate must be a character string if specified.")
  } else {
    endDate <- as.Date(endDate, format = "%m/%d/%Y")
  }

  if (startDate > endDate) {
    stop("endDate must be after startDate.")
  }

  days <- seq(startDate, endDate, by="days")

  # Match weatherVars
  weatherVars <- match.arg(weatherVars, several.ok = TRUE)

  # Go and retrieve!

  # For the Wunderground API, we are limited to a call per station, per day
  # We create a SAX handler that will parse a single day of history data for a single station.
  # It saves the first value at each hour for that day and provides a hook to retrieve a data table.

  # SAX Handler that will deal with each observation
  observationHandler = function(day) {
    tableHours <- numeric()
    tableVars <- list()
    # Create an empty vector for each var we are saving
    lapply(weatherVars, function(var) tableVars[[var]] <- numeric())

    # Keep the first observation for each hour, also check if we skip an hour
    nextHour <- 0

    observation <- function(context, node, attrs, ...) {
      hour <- as.numeric(XML::xmlValue(node[["date"]][["hour"]]))

      if (hour >= nextHour) {
        if (hour > nextHour) {
          # This means we skipped an hour/s
          # Create a sequence of missing hours
          hoursMissing <- seq(nextHour, hour - 1)

          # Fill these in
          tableHours <<- c(tableHours, hoursMissing)

          # Add NA values for each missing datapoint
          lapply(weatherVars, function(var) tableVars[[var]] <<- c(tableVars[[var]],
                                                                   rep(NA, length(hoursMissing))))
        }

        # We are at the first point of the next hour
        nextHour <<- hour + 1
        lapply(weatherVars, function(var) tableVars[[var]] <<- c(tableVars[[var]],
                                                                         as.numeric(XML::xmlValue(node[[var]]))))
        tableHours <<- c(tableHours, hour)
      }
    }

    getWeatherDT <- function() {
      if (length(tableHours) > 0) {
        if (length(tableHours) < 24) {
          # Looks like we missed some data at the end
          missingHours <- seq(max(tableHours) + 1, 23)
          tableHours <- c(tableHours, missingHours)
          lapply(weatherVars, function(var) tableVars[[var]] <<- c(tableVars[[var]],
                                                                   rep(NA, length(missingHours))))
        }
        weatherDT <- data.table::data.table(day = day,
                                            hour = tableHours)
        weatherDT <- cbind(weatherDT, as.data.frame(tableVars))
        setNames(weatherDT, c("day", "hour", weatherVars))
      } else {
        data.table::data.table()
      }
    }

    c(observation = XML::xmlParserContextFunction(observation),
      getWeatherDT = getWeatherDT)
  }

  getDayHistory <- function(stationId, day) {
    usertoken <- Sys.getenv("WUNDERGROUND_TOKEN")
    baseurl <- "http://api.wunderground.com/api/"
    historyUrl <- paste0(baseurl,
                         usertoken,
                         "/history_",
                         format(day, "%Y%m%d"),
                         "/q/PWS:",
                         stationId,
                         ".xml")

    # Generate a table for a single day
    obsHandler <- observationHandler(day)
    XML::xmlEventParse(file = historyUrl,
                              handlers = NULL,
                              branches = obsHandler,
                              isURL = TRUE)

    obsHandler$getWeatherDT()
  }

  # This call is a for each station id, for each day, get the weather data then combine into a single
  # data table for each station and into a list of data tables, one for each station.
  print(sprintf("There are %d stations to query over %d days. This may take a while.", length(stationIds), length(days)))
  stationCount <- 1
  stationsTotal <- length(stationIds)
  allStations <- lapply(stationIds, function(station) {
    print(sprintf("Processing station %d of %d...", stationCount, stationsTotal))
    stationCount <<- stationCount + 1
    data.table::rbindlist(lapply(days, function(day) getDayHistory(station, day)))
  })
  names(allStations) <- stationIds

  pwStations$weatherData <- allStations

  pwStations$weatherData
}