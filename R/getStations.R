#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru 
#' @description This function accepts the zip code and a radius in miles and 
#'              returns all the stations in that range in the PWStations class.
#' @param Zip Code and the radius in miles
#' @return PWStations class with weather stations data
#' @import XML
#' @import data.table
## @examples
## \dontrun{plotQuakes("red")}
## 
getStations <- function(zip,radius=10){
## 
  obj <- new("PWStations",center=zip,range=as.integer(radius),
             stations=data.table())
##
## Retrieving API key from environment variable
##
  usertoken <- Sys.getenv("WUNDERGROUND_TOKEN")
#
  baseurl <- "http://api.wunderground.com/api/" 
  finalurl <- paste0(baseurl,usertoken,"/geolookup/q/",obj@center,".xml")
  pwsXML <- xmlTreeParse(finalurl, useInternalNodes=TRUE)
  pwsRoot <- xmlRoot(pwsXML)
  pwsNamespace <- getDefaultNamespace(pwsRoot)
  pwsPath <- sprintf(paste("/response/location/nearby_weather_stations/pws",
                           "/station[distance_mi<=%d]",sep=""),obj@range)
  result <- getNodeSet(pwsRoot,pwsPath,pwsNamespace)
##
## Building the data frame to be returned with the PWS data
##
  obj@stations <- data.table(
                    id=sapply(result,function(x) {xmlToList(x)$id}),
                    city=sapply(result,function(x) {xmlToList(x)$city}),
                    state=sapply(result,function(x) {xmlToList(x)$state}),
                    country=sapply(result,function(x) {xmlToList(x)$country}),
                    lat=sapply(result,function(x) {xmlToList(x)$lat}),
                    long=sapply(result,function(x) {xmlToList(x)$lon}),
                    distance_mi=sapply(result,
                                       function(x) {xmlToList(x)$distance_mi})) 
 obj
}