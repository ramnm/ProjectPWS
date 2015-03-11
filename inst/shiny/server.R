#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru, Jared Casale
##
library(shiny)
library(leaflet)
shinyServer(function(input,output,session){
#
  sMap <- createLeafletMap(session,"stnMap")
  wMap <- createLeafletMap(session,"weatherMap")
#
  buildObj <- reactive({
    if(input$getStations > 0 ){
      print("inside loadStations")
      sMap$clearMarkers()
      sMap$clearShapes()
      validate(
        need(isolate(input$zipcode)%in%zipcodes$zip,
             "Please enter a valid zip code")
      )
      a <- isolate(input$zipcode)
      b <- isolate(input$range)
      c <- stateCd[stateCd$US.State==isolate(input$stcode),2]
      d <- countryCd[countryCd$Country==isolate(input$country),2]
      switch(isolate(input$input_type),
             "Zip Code" = c <- d <- NA,
             "State Code" = a <- b <- d <- NA,
             "Country Code" = a <- b <- c <- NA)
      ProjectPWS::getStations(zip = a,
                                     radius = b,
                                     state = c,
                                     country = d)
    }
  })
#
  output$stnTable <- renderDataTable({
    obj <- buildObj()
    obj$stations
  })
#
  output$stnMap <- renderLeaflet({
    if(input$getStations > 0 ){
    obj <- buildObj()
    stnTable <- obj$stations
    pins <- data.frame(lon=as.numeric(stnTable$lon),
                       lat=as.numeric(stnTable$lat),
                       id=stnTable$id)
    sMap$fitBounds(min(pins$lat),min(pins$lon),max(pins$lat),max(pins$lon))
      for(i in 1:nrow(pins)){
        sMap$addMarker(lat = pins$lat[i], lng = pins$lon[i],
                       layerId = pins$id[i],
                       list(fillOpacity = 0.5))
      }
    sMap
    }
  })
  wMap$clearMarkers()
  wMap$clearShapes()
#

})