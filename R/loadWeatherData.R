#' Queries WUnderground and retrieves the specified weather variables over the requested time range, for each station in the PWStations object.
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description This function will query the wunderground site and retrieve the weather data for each PW Station, over the specified time period.
#' @param pwStations PWStation object containing the stations that require data.
#' @param startDate Beginning of time period of interest. Must be a string with format mm/dd/YYYY.
#' @param endDate End of time period of interest. If specified, must be a string with format mm/dd/YYYY.
#' @param weatherVars Variables to retrieve, must be one of "tempi" (temperature imperial i.e. Fahrenheit), "hum" (humidity)
#' @param stationLimit This restricts to the first stationLimit stations. -1 signals to retrieve all in the table.
#' @return List of data frames, one for each weather station in the original PWStations object. The name of each element is the station's ID.
#' @export
#' @examples
#' \dontrun{
#'  stations <- getStations(zip = "98107", radius = 2)
#'  weatherData <- loadWeatherData(stations, startDate = "3/3/2015", stationLimit = 1)
#'  weatherData2 <- loadWeatherData(stations, startDate = "3/1/2015", endDate = "3/3/2015", stationLimit = 1)
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
  # This function takes a station ID and day and grabs the appropriate weather variables
  # It then takes the first value at each hour for that day and returns a data table.
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

    historyXml <- XML::xmlTreeParse(historyUrl, useInternalNodes = TRUE)
    historyRoot <- XML::xmlRoot(historyXml)
    historyNamespace <- XML::getDefaultNamespace(historyRoot)
    obsPath <- "/response/history/observations/observation"
    observations <- XML::getNodeSet(historyRoot, obsPath, historyNamespace)

    obsList <- lapply(observations, XML::xmlToList)

    # Grab the dates
    dateNodes <- lapply(obsList, function(x) x$date)

    # Save the hour of each observation for sorting, we will just keep the first for each hour
    hour <- as.numeric(sapply(dateNodes, function(x) x$hour))

    # Now get each variable we are interested in
    getWeatherData <- function(varName) {
      as.numeric(sapply(obsList, function(x) eval(parse(text = sprintf("x$%s", varName)))))
    }

    weatherData <- lapply(weatherVars, getWeatherData)
    names(weatherData) <- weatherVars

    # Combine into a data frame
    weatherDf <- as.data.frame(weatherData)
    weatherDf <- cbind(hour, weatherDf)

    # Create a data table with hours as the key. This lets us call unique which will leave the
    # first entry for each hour.
    # TODO: What if an hour is missing?
    weatherDT <- data.table::data.table(weatherDf, key="hour")
    weatherDT <- unique(weatherDT)
    weatherDT$day <- day # For when we combine later

    weatherDT
  }

  # TODO: Break this up and add progress e.g. Retrieving station 1 of 5...
  # This call is a for each station id, for each day, get the weather data then combine into a single
  # data table for each station and into a list of data tables, one for each station.
  allStations <- lapply(stationIds, function(station) data.table::rbindlist(lapply(days, function(day) getDayHistory(station, day))))
  names(allStations) <- stationIds

  allStations
}