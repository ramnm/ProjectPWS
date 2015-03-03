#' Function defining the PWStations S4 class
#' @author Maruthi Ram Nadakuduru 
#' @description This function includes all the definitions of the PWStations 
#'              S4 class.
#' @import methods data.table
## @examples
## \dontrun{plotQuakes("red")}
## 
setClass("PWStations", representation(center = "character",
                                      range = "integer",
                                      stations = "data.table"),
         validity = function(object) {
           if (object@center == "") {
             return("Please enter a zip code")
           }
           
           if (!any(zipcodes[zipcodes$zip == object@center, 1])) {
             return("Please enter a valid zip code")
           }
           
           if (!is.integer(object@range)) {
             return("Please enter an integer value")
           }
         })
##
##setMethod("plot","PWStations",function(object){
##  plot(object@stations$lat,object@stations$long,type="p",main="Stations Plot",
##       xlab="latitude",ylab="longitude")
##  points(col="red")
##})
##
