##
## Author - Maruthi Ram Nadakuduru
## Description - This function accepts the zip code and a radius in miles
## and returns all the weather stations in that range in a table.
##
getStations <- function(zip,radius=10){
##
## Retrieving API key from environment variable
##
  usertoken <- Sys.getenv("WUNDERGROUND_TOKEN")
#
  baseurl <- "http://api.wunderground.com/api/" 
  finalurl <- paste0(baseurl,usertoken,"/geolookup/q/",zip,".xml")
  pwsXML <- xmlTreeParse(finalurl, useInternalNodes=TRUE)
  pwsRoot <- xmlRoot(pwsXML)
  pwsNamespace <- getDefaultNamespace(pwsRoot)
  pwsPath <- sprintf(paste("/response/location/nearby_weather_stations/pws",
                           "/station[distance_mi<=%d]",sep=""),radius)
  result <- getNodeSet(pwsRoot,pwsPath,pwsNamespace)
##
## Building the data frame to be returned with the PWS data
##
  pwsTable <- cbind(sapply(result,function(x) {xmlToList(x)$id}),
                    sapply(result,function(x) {xmlToList(x)$city}),
                    sapply(result,function(x) {xmlToList(x)$state}),
                    sapply(result,function(x) {xmlToList(x)$country}),
                    sapply(result,function(x) {xmlToList(x)$lat}),
                    sapply(result,function(x) {xmlToList(x)$lon}),
                    sapply(result,function(x) {xmlToList(x)$distance_mi}))
 pwsTable <- as.data.frame(pwsTable)
 names(pwsTable) <- c("id","city","state","country","lat","lon","distance_mi")
 pwsTable
}