#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru, Jared Casale
#' @description This function accepts the zip code and a radius in km and
#'              returns all the stations in that range in the PWStations class.
#' @param Zip Code and the radius in kilometers
#' @return PWStations class with weather stations data
#' @export getStations
## @examples
## \dontrun{plotQuakes("red")}
##
getStations <- function(zip, radius=10, state, country){
##
  obj <- PWStations$new(
                        center = zip,
                        range = as.integer(radius),
                        state = state,
                        country = country,
                        stations=data.table())
## Retrieving API key from environment variable
  usertoken <- Sys.getenv("WUNDERGROUND_TOKEN")
  baseurl <- "http://api.wunderground.com/api/"
#
# Zip Code entered
#
  if(!is.na(obj$center)) {
    finalurl <- paste0(baseurl,usertoken,"/geolookup/q/",obj$center,".xml")
    pwsXML <- XML::xmlTreeParse(finalurl, useInternalNodes=TRUE)
    pwsRoot <- XML::xmlRoot(pwsXML)
    pwsNamespace <- XML::getDefaultNamespace(pwsRoot)
    pwsPath <- sprintf(paste("/response/location/nearby_weather_stations/pws",
                             "/station[distance_mi<=%d]",sep=""),obj$range)
    result <- XML::getNodeSet(pwsRoot,pwsPath,pwsNamespace)
  }
#
# State Code entered
#
  if(!is.na(obj$state)){
    finalurl <- paste0(baseurl,usertoken,"/geolookup/q/",obj$state,".xml")
    pwsXML <- XML::xmlTreeParse(finalurl, useInternalNodes=TRUE)
    pwsRoot <- XML::xmlRoot(pwsXML)
    pwsNamespace <- XML::getDefaultNamespace(pwsRoot)
    pwsPath <- "/response/results/result"
    result <- XML::getNodeSet(pwsRoot,pwsPath,pwsNamespace)
  }
#
# Country Code entered
#
  if(!is.na(obj$country)){
    countryName <- countryCd[countryCd$ISO3==obj$country,1]
    finalurl <- paste0(baseurl,usertoken,"/geolookup/q/",countryName,".xml")
    pwsXML <- XML::xmlTreeParse(finalurl, useInternalNodes=TRUE)
    pwsRoot <- XML::xmlRoot(pwsXML)
    pwsNamespace <- XML::getDefaultNamespace(pwsRoot)
    pwsPath <- "/response/location/nearby_weather_stations/pws/station"
    result <- XML::getNodeSet(pwsRoot,pwsPath,pwsNamespace)
  }
##
## Building the data frame to be returned with the PWS data
##
  obj$stations <- data.table::data.table(
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
 obj
}