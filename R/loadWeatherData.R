##
##  Begin jcasale code
##
#' Load PWS Weather Data
#' @author Jared Casale
#' @description This function will query the wunderground site and retrieve the
#'              weather data for each PW Station, over the specified time
#'              period.
#' @param pwStations PWStation object containing the stations that require data.
#' @param startDate Beginning of time period of interest. Must be a string with
#'        format mm/dd/YYYY.
#' @param endDate End of time period of interest. If specified, must be a
#'        string with format mm/dd/YYYY.
#' @param startHour First hour of period of interest.
#'        Must be numeric between 0 and 23.
#' @param endHour Last hour of period of interest.
#'        Must be numeric between 0 and 23.
#' @param weatherVars Variables to retrieve, must be one of "tempi"
#'        (temperature imperial i.e. Fahrenheit), "hum" (humidity),
#'        "wspid" (Weather speed imperial), "pressure". Plain-text
#'        weather conditions ("conds") will also be extracted.
#' @param stationLimit This restricts to the first stationLimit stations.
#'        -1 signals to retrieve all in the table.
#' @return List of data frames, one for each weather station in the original
#'         PWStations object. The name of each element is the station's ID.
#' Note that this is also accessible from the supplied PWStations object.
#' @export
#' @examples
#' \dontrun{
#' stations <- getStations(zip = "98107",
#'                         radius = 2)
#' loadWeatherData(stations,
#'                 startDate = "3/3/2015",
#'                 startHour = 9,
#'                 endHour = 5,
#'                 stationLimit = 1)
#'
#' length(stations$weatherData)
#' nrow(stations$weatherData[[1]])
#'
#' loadWeatherData(stations,
#'                 startDate = "3/1/2015",
#'                 endDate = "3/3/2015",
#'                 stationLimit = 3)
#'
#' length(stations$weatherData)
#' nrow(stations$weatherData[[1]])
#' }
loadWeatherData <- function(pwStations, startDate, endDate = NA,
                            startHour = NA, endHour = NA,
                            weatherVars = c("tempi", "hum", "wspdi",
                                            "pressure"),
                            stationLimit = -1L) {
  # Check pwStations object
  if (!all(class(pwStations) == c("PWStations", "R6"))) {
    stop("pwStations must be a PWStations object.")
  }

  # Get the stations - is it empty?
  stationsTable <- pwStations$stations
  if (is.na(stationsTable) || nrow(stationsTable) == 0) {
    stop("Need at least one PW Station.")
  }

  if (stationLimit > 0 && stationLimit < nrow(stationsTable)) {
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

  # Check startHour, is it specified, is it a valid hour?
  if (is.na(startHour)) {
    startHour <- 0
  } else if (!is.numeric(startHour) || startHour < 0 ||
               startHour > 23) {
    stop("startHour must be an integer between 0 and 23.")
  } else {
    startHour <- as.integer(startHour)
  }

  # Check endHour, is it specified, is it a valid hour?
  if (is.na(endHour)) {
    endHour <- 0
  } else if (!is.numeric(endHour) || endHour < 0 ||
               endHour > 23) {
    stop("endHour must be an integer between 0 and 23.")
  } else {
    endHour <- as.integer(endHour)
  }

  # Match weatherVars
  weatherVars <- match.arg(weatherVars, several.ok = TRUE)

  if (!all(identical(startDate, pwStations$startDate),
           identical(endDate, pwStations$endDate),
           identical(startHour, pwStations$startHour),
           identical(endHour, pwStations$endHour),
           identical(weatherVars, pwStations$weatherVars))) {
    # For the Wunderground API, we are limited to a call per station, per day
    # We create a SAX handler that will parse a single day of history data for a
    # single station. It saves the first value at each hour for that day and
    # provides a hook to retrieve a data table.

    # SAX Handler that will deal with each observation

    # Variables to indicate if we are processing the first/last days (special)
    observationHandler = function(day) {
      tableHours <- numeric()
      tableVars <- list()
      tableConds <- list()

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
            lapply(weatherVars,
                   function(var) {
                     tableVars[[var]] <<- c(tableVars[[var]],
                                            rep(NA, length(hoursMissing)))}
            )

            tableConds <<- c(tableConds, rep(NA, length(hoursMissing)))
          }

          # We are at the first point of the next hour
          nextHour <<- hour + 1
          lapply(weatherVars, function(var) {
            varToAdd <- NA
            # Does this value exist in this observation?
            if (var %in% names(node)) {
              varToAdd <- as.numeric(XML::xmlValue(node[[var]]))
            }
            tableVars[[var]] <<- c(tableVars[[var]], varToAdd)
          })

          condsToAdd <- NA
          if ("conds" %in% names(node)) {
            condsToAdd <- XML::xmlValue(node[["conds"]])
          }

          tableConds <<- c(tableConds, condsToAdd)
          tableHours <<- c(tableHours, hour)
        }
      }

      getWeatherDT <- function() {
        if (length(tableHours) > 0) {
          if (length(tableHours) < 24) {
            # Looks like we missed some data at the end
            missingHours <- seq(max(tableHours) + 1, 23)
            tableHours <<- c(tableHours, missingHours)
            tableConds <<- c(tableConds, rep(NA, length(missingHours)))
            lapply(weatherVars,
                   function(var) {
                     tableVars[[var]] <<- c(tableVars[[var]],
                                            rep(NA, length(missingHours)))
            })
          }
          weatherDT <- data.table::data.table(day = day,
                                              hour = tableHours)
          weatherDT <- cbind(weatherDT,
                             data.table::data.table(as.data.frame(tableVars)))
          weatherDT$conds <- tableConds

          setNames(weatherDT, c("day", "hour", weatherVars, "conds"))
        } else {
          data.table::data.table()
        }
      }

      errorHandler <- function(context, node, attrs, ...) {
       if ("type" %in% names(node) && "description" %in% names(node)) {
         stop("Error from Wunderground API. Type: %s, Description: %s",
              XML::xmlValue(node[["type"]]),
              XML::xmlValue(node[["description"]]))
       }
      }

      c(observation = XML::xmlParserContextFunction(observation),
        error = XML::xmlParserContextFunction(errorHandler),
        getWeatherDT = getWeatherDT)
    }

    getDayHistory <- function(stationId, day, firstDay, lastDay) {
      apiKey <- Sys.getenv("WUNDERGROUND_API_KEY")
      if (is.na(apiKey) || apiKey == "") {
        stop(paste0("A WUnderground API key must be specified as the environment",
                    "variable 'WUNDERGROUND_API_KEY'."))
      }

      baseurl <- "http://api.wunderground.com/api/"
      historyUrl <- paste0(baseurl,
                           apiKey,
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

      wDT <- obsHandler$getWeatherDT()
      if (firstDay) {
        # Just keep values after the first hour
        wDT <- wDT[wDT$hour >= startHour, ]
      } else if (lastDay) {
        wDT <- wDT[wDT$hour <= endHour, ]
      }

      wDT
    }

    # This call is essentially for each station id, for each day, get the weather
    # data then combine into a single data table for each station and into a list
    # of data tables, one for each station.
    # print(sprintf("There are %d stations to query over %d days.
    # This may take a while.", length(stationIds), length(days)))
    stationCount <- 1
    stationsTotal <- length(stationIds)
    allStations <- lapply(stationIds, function(station) {
      print(sprintf("Processing station %d of %d...", stationCount,
                    stationsTotal))
      stationCount <<- stationCount + 1
      dayCount <- 1
      daysTotal <- length(days)
      stationDays <- lapply(days,
                            function(day) {
                              print(sprintf("Processing day %d of %d...",
                                            dayCount,
                                            daysTotal))
                              dHist <- getDayHistory(
                                station,
                                day,
                                firstDay = (dayCount == 1),
                                lastDay = (dayCount == daysTotal))
                              dayCount <<- dayCount + 1
                              dHist
                            })
      data.table::rbindlist(stationDays)
    })
    names(allStations) <- stationIds

    pwStations$weatherData <- allStations

    # Set state of query
    pwStations$startDate <- startDate
    pwStations$endDate <- endDate
    pwStations$startHour <- startHour
    pwStations$endHour <- endHour
    pwStations$weatherVars <- weatherVars
  }

  pwStations
}
##
##  End jcasale code
##