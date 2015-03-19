##
##  Begin jcasale code
##
#' Retrieve PWS
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description Queries the WUnderground API to get all PWS meeting the
#'              specified criteria. Valid queries are as follows:
#' \itemize{
#'   \item A numeric vector specifying a lat/long pair.
#'   \item A US zipcode character string.
#'   \item A US state or non-US country name.
#'   \item A US state and city pair.
#'   \item A non-US country name and city pair.
#' }
#' The API limits to PWS within 40km (around 25 miles) of the zipcode and only
#' the first 50 results. An optional numeric mile radius restriction may be
#' supplied, limiting to stations within that distance from the specified
#' location. Anything greater than 24 will be ignored
#' (due to the API limitation).
#'
#' @param latlong A numeric vector specifying latitude and longitude.
#' @param zip Character string representing a US zip code.
#' @param state US two character state code, a city may also be supplied. If
#'        no city is supplied, all cities returned for that state will be
#'        queried (may take a lot of time).
#' @param country Name of a (non-US) country. If no city is supplied, all
#'        cities returned for that country will be queried (may take a lot of
#'        time).
#' @param city Name of the requested city.
#' @param radius Optional radius in miles to limit results to. Must be a
#'        numeric value greater than 0 but less than 25. If querying by state
#'        or country, the behavior here may not be intuitive.
#' @return A PWStations object with the nearby stations meeting the specified
#'         criteria.
#' @seealso \code{\link{loadWeatherData}} for a method to retrieve weather data
#'         for each of the retrieved stations.
#' @export getStations
#' @examples
#' \dontrun{
#' # Charlotte
#' latLongStations <- getStations(latlong = c(35.229, -80.8433), radius = 2)
#' zipStations <- getStations(zip = "90210", radius = 10)
#' berlinStations <- getStations(country = "Germany", city = "Berlin")
#' cityStations <- getStations(state = "OR", city = "Portand", radius = 3)
#' countryStations <- getStations(country = "Spain")
#' }
##
##  End jcasale code
##
##
##  Begin nmram code
##
getStations <- function(latlong = NA, zip = NA, state = NA, country = NA,
                        city = NA, radius = NA) {
  # Create a PWStations object to return.
  # This also provides some validation of the arguments
  pwstationObj <- PWStations$new(latlong = latlong,
                                 zip = zip,
                                 state = state,
                                 country = country,
                                 city = city,
                                 radius = radius)

  # Retrieve the API key from environment variable
  apiKey <- Sys.getenv("WUNDERGROUND_API_KEY")
  if (is.na(apiKey) || apiKey == "") {
    stop(paste0("A WUnderground API key must be specified as the environment",
                "variable 'WUNDERGROUND_TOKEN'."))
  }
  baseurl <- "http://api.wunderground.com/api/"

  # This helper function assumes that the returned page contains the
  # nearby_weather_stations node

  # Use this to loop through a set of cities for a Country or State
  getAndParseStationData <- function(queryArg) {
    finalurl <- URLencode(paste0(baseurl,
                                 apiKey,
                                 "/geolookup/q/",
                                 queryArg,
                                 ".xml"))

    pwsXML <- XML::xmlTreeParse(finalurl, useInternalNodes = TRUE)
    pwsRoot <- XML::xmlRoot(pwsXML)
    pwsNamespace <- XML::getDefaultNamespace(pwsRoot)
    # Check for errors
    errorNodes <- XML::getNodeSet(pwsRoot,
                                  "/response/error",
                                  pwsNamespace)

    if (length(errorNodes) > 0) {
      errorList <- XML::xmlToList(errorNodes[[1]])
      stop(sprintf("Error from Wunderground API. Type: %s, Description: %s",
                   errorList$type,
                   errorList$description))
    }

    # Get the stations
    pwsPath <- paste0("/response/location/nearby_weather_stations/pws",
                      sprintf("/station[distance_mi<=%d]", pwstationObj$radius))

    stationNodes <- XML::getNodeSet(pwsRoot, pwsPath, pwsNamespace)

    # Create empty vectors
    ids <- character()
    cities <- character()
    states <- character()
    countries <- character()
    lats <- numeric()
    longs <- numeric()
    distance_mis <- numeric()

    # Loop over each station node
    stationList <- lapply(stationNodes, function(stationNode) {
      xmlList <- XML::xmlToList(stationNode)

      list(id = xmlList$id,
           city = xmlList$city,
           state = ifelse(is.null(xmlList$state), "NA",
                          xmlList$state),
           country = xmlList$country,
           lat = as.numeric(xmlList$lat),
           lon = as.numeric(xmlList$lon),
           distance_mi = as.numeric(xmlList$distance_mi))
    })
##
##  End nmram code
##
##
##  Begin jcasale code
##
    data.table::rbindlist(stationList)
  }

  # If the query is for a whole state or country, then we need to query all
  # cities in that area
  if (!is.na(state) || !is.na(country) && is.na(city)) {
    # Need to request the region then each city returned for that region
    areaUrl <- URLencode(paste0(baseurl,
                                apiKey,
                                "/geolookup/q/",
                                pwstationObj$queryArg,
                                ".xml"))

    areaXML <- XML::xmlTreeParse(areaUrl, useInternalNodes = TRUE)
    areaRoot <- XML::xmlRoot(areaXML)
    areaNamespace <- XML::getDefaultNamespace(areaRoot)
    areaPath <- paste0("/response/results/result/name")

    cityNames <- sapply(XML::getNodeSet(areaRoot, areaPath, areaNamespace),
                        XML::xmlValue)

    cityTotal <- length(cityNames)
    cityCount <- 1
    print(sprintf("There are %d cities to query. This may take a while.",
                  cityTotal))
    cityTables <- lapply(cityNames, function(cityName) {
      print(sprintf("Processing city '%s' (%d of %d)...", cityName,
                    cityCount, cityTotal))
      cityCount <<- cityCount + 1
      getAndParseStationData(paste0(pwstationObj$queryArg, "/",cityName))
    })

    pwstationObj$stations <- data.table::rbindlist(cityTables)
  } else {
    pwstationObj$stations <- getAndParseStationData(pwstationObj$queryArg)
  }

  pwstationObj
}
##
##  End jcasale code
##