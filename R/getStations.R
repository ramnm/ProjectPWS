#' Returns the nearby Personal Weather Stations according to the location requested.
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description Queries the WUnderground API to get all PWS meeting the specified criteria. Valid queries are as follows:
#' \itemize{
#'   \item A numeric vector specifying a lat/long pair.
#'   \item A US zipcode character string.
#'   \item A US state and city pair.
#'   \item A non-US country name and city pair.
#' }
#' The API limits to PWS within 40km (around 25 miles) of the zipcode and only the first 50 results.
#' An optional numeric mile radius restriction may be supplied, limiting to stations within that distance from
#' the specified location. Anything greater than 24 will be ignored (due to the API limitation).
#'
#' @param latlong A numeric vector specifying latitude and longitude.
#' @param zip Character string representing a US zip code.
#' @param state US two character state code, a city must also be supplied.
#' @param country Name of a (non-US) country.
#' @param city Name of the requested city.
#' @param radius Optional radius in miles to limit results to. Must be a numeric value greater than 0 but less than 25.
#' @return A PWStations object with weather stations data
#' @export getStations
#' @examples
#' \dontrun{
#' latLongStations <- getStations(latlong = c(35.229, 80.8433), radius = 2) # Charlotte
#' zipStations <- getStations(zip = "90210", radius = 10)
#' berlinStations <- getStations(country = "Germany", city = "Berlin")
#' cityStations <- getStations(state = "Oregon", city = "Portand", radius = 3)
#' }
getStations <- function(latlong = NA, zip = NA, state = NA, country = NA, city = NA, radius = NA) {
  # Create a PWStations object to return.
  # This also provides some validation of the arguments
  pwstationObj <- PWStations$new(latlong = latlong,
                                 zip = zip,
                                 state = state,
                                 country = country,
                                 radius = radius)

  # Retrieve the API key from environment variable
  usertoken <- Sys.getenv("WUNDERGROUND_TOKEN")

  if (is.na(usertoken)) {
    stop("A WUnderground API key must be specified as the environment variable 'WUNDERGROUND_TOKEN'.")
  }

  baseurl <- "http://api.wunderground.com/api/"

  finalurl <- paste0(baseurl,
                     usertoken,
                     "/geolookup/q/",
                     pwstationObj$queryArg,
                     ".xml")
  pwsXML <- XML::xmlTreeParse(finalurl, useInternalNodes = TRUE)
  pwsRoot <- XML::xmlRoot(pwsXML)
  pwsNamespace <- XML::getDefaultNamespace(pwsRoot)
  pwsPath <- paste0("/response/location/nearby_weather_stations/pws",
                    sprintf("/station[distance_mi<=%d]", radius))

  result <- XML::getNodeSet(pwsRoot, pwsPath, pwsNamespace)

  # Fill in the retrieved station data.
  pwstationObj$stations <- data.table::data.table(
                      id = sapply(result,function(x) {XML::xmlToList(x)$id}),
                      city = sapply(result,
                                    function(x) {XML::xmlToList(x)$city}),
                      state = sapply(result,
                                     function(x) {XML::xmlToList(x)$state}),
                      country = sapply(result,
                                       function(x) {XML::xmlToList(x)$country}),
                      lat = sapply(result,
                                   function(x) {XML::xmlToList(x)$lat}),
                      long = sapply(result,
                                    function(x) {XML::xmlToList(x)$lon}),
                      distance_mi = sapply(result,
                                  function(x) {XML::xmlToList(x)$distance_mi}))
 pwstationObj
}