##
##  Begin jcasale code
##
#' Clean and fill PWS weather data
#' @author Jared Casale
#' @description Takes a PWStation object and loops through all of its
#'              stored weather data. For each weather data table, it will:
#' \itemize{
#' \item  Check all values and determine if they are within a (generously)
#'        reasonable range. If they are not, they will be replaced with an NA.
#' \item  Trim each table so that any columns with less than 2 data points
#'        are removed.
#' \item  Fill in missing values by taking the mean of 5 imputations using
#'        the \code{\link[Amelia]{amelia}} function.
#' }
#' @param pwStations PWStations object with loaded weather data.
#' @param stopAfterValidation Will stop after replacing values outside the
#'        reasonable range with NA and return the resulting list of tables.
#' @param stopAfterTrim Will stop after trimming columns with less than 2 data
#'        points.
#' @return  A list of data tables representing the original weather data with
#'          the specified operations applied.
#' @export validateTrimAndFill
#' @examples
#' data(charlotteStations)
#'
#' # Just keep two stations
#' lessStations <- makeStationSubtable(charlotteStations,
#'                        stationNames = c("KNCCHARL71", "KNCCHARL83"))
#'
#' # Just validate
#' justVal <- validateTrimAndFill(charlotteStations, stopAfterValidation = TRUE)
#' sum(is.na(justVal[[1]]))
#'
#' # Just validate and trim
#' justTrim <- validateTrimAndFill(charlotteStations, stopAfterTrim = TRUE)
#' sum(is.na(justTrim[[1]]))
#'
#' # Validate, trim and then fill missing data
#' filled <- validateTrimAndFill(charlotteStations)
#' sum(is.na(filled[[1]]))
validateTrimAndFill <- function(pwStations,
                                stopAfterValidation = FALSE,
                                stopAfterTrim = FALSE) {
  # Check pwStations object
  if (!all(class(pwStations) == c("PWStations", "R6"))) {
    stop("pwStations must be a PWStations object.")
  }

  # Get the weatherData - is it empty?
  weatherTables <- pwStations$weatherData
  if (is.na(weatherTables) || length(weatherTables) == 0) {
    stop("Need a PWStations object with loaded weather data.")
  }

  # Checks values are within a certain range
  # Range is specified here:
  weatherVars <- c("tempi", "hum", "wspdi", "pressure", "conds")
  validationTable <- data.frame(
    varName = weatherVars,
    canValidate = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    minValue = c(-130, 0, 0, 20, NA),
    #Deg. F, Relative humidity, Miles per hour, Inches of Mercury
    maxValue = c(135, 100, 253, 33, NA))

  # Fills in NA where values are outside the range
  validateAndFill <- function(weatherData) {
    lapply(weatherVars, function(col) {
      valRow <- validationTable[validationTable$varName == col, ]
      if (valRow$canValidate) {
        weatherData[[col]] <<-
          sapply(weatherData[[col]], function(val) {
            returnVal <- val
            if (!is.na(val)) {
              if (val < valRow$minValue ||
                    val > valRow$maxValue) {
                returnVal <- NA
              }
            }
            returnVal
          })
      }
    })

    weatherData
  }

  returnWeatherList <- lapply(weatherTables, validateAndFill)

  # Keep going?
  if (!stopAfterValidation) {
    # Strips all columns that are completely NA values
    trimWeatherData <- function(weatherData) {
      colsWithData <- character()
      lapply(names(weatherData), function(colName) {
        # Only keep columns with more than one data value
        if (sum(!is.na(weatherData[[colName]])) > 1) {
          colsWithData <<- c(colsWithData, colName)
        }
      })

      weatherData[ , colsWithData]
    }

    returnWeatherList <- lapply(returnWeatherList, trimWeatherData)

    if (!stopAfterTrim) {
      fillMissingValues <- function(weatherData) {
        weatherVars <- c("tempi", "hum", "wspdi", "pressure", "conds")
        weatherVarsInData <- weatherVars[weatherVars %in% names(weatherData)]
        toImpute <- weatherData[ , weatherVarsInData]
        colsToImpute <- names(toImpute)[
          sapply(names(toImpute), function(colName) {
            is.numeric(toImpute[[colName]])
          })
          ]

        toImpute <- toImpute[ , colsToImpute]

        # Any missing values?
        if (any(is.na(toImpute))) {
          # Create a sequence so that Amelia has
          # some data point to use (the time series of hours essentially)
          toImpute$ts <- seq_len(nrow(toImpute))

          a.out <- Amelia::amelia(toImpute,
                                  m = 5,
                                  p2s = 0)

          # This method is potentially statistically poor
          # The imputations should be kept separately, or
          # when combined should be examined with the combined
          # variance. For our purposes, this is a reasonable
          # method for filling in missing values.
          collapsedCols <- lapply(colsToImpute, function(colName) {
            imputedCols <- lapply(a.out$imputations,
                                  function(imputedData) {
                                    imputedData[[colName]]
                                  })

            collapsedCol <- rowMeans(as.data.frame(imputedCols))

            # Note that Amelia may estimate something outside
            # the range of possible values
            # Trim the edges
            valRow <- validationTable[validationTable$varName == colName, ]
            minColVal <- valRow$minValue
            maxColVal <- valRow$maxValue
            collapsedCol[collapsedCol < minColVal] <- minColVal
            collapsedCol[collapsedCol > maxColVal] <- maxColVal

            collapsedCol
          })
          names(collapsedCols) <- colsToImpute

          lapply(colsToImpute, function(col) {
            weatherData[[col]] <<- collapsedCols[[col]]
          })
        }

        weatherData
      }

      returnWeatherList <- lapply(returnWeatherList, fillMissingValues)
    }
  }

  returnWeatherList
}
##
##  End jcasale code
##