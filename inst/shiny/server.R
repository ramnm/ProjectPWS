#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru, Jared Casale
##
library(leaflet)
# elements defined here are available across sessions
obj <- ProjectPWS::PWStations$new(zip = "28262", radius = 5)
pins <- data.frame()
wPins <- integer()
timesRef <- c("01:00AM","02:00AM","03:00AM","04:00AM",
              "05:00AM","06:00AM","07:00AM","08:00AM",
              "09:00AM","10:00AM","11:00AM","12:00PM",
              "01:00PM","02:00PM","03:00PM","04:00PM",
              "05:00PM","06:00PM","07:00PM","08:00PM",
              "09:00PM","10:00PM","11:00PM","12:00PM")
wRange <- FALSE
wInputs <- list(wStDate=c("X"),
                wStTime=c("X"),
                wEndDate=c("X"),
                wEndTime=c("X"))
# Validation function for the loaded File option
pwsObject <- list()
#
shinyServer(function(input,output,session){
#
  print("in the header part")
  sMap <- createLeafletMap(session,"stnMap")
  wMap <- createLeafletMap(session,"weatherMap")
#
  buildStn <- reactive({
    if(input$getStations > 0 ){
      print("inside loadStations")
      sMap$clearMarkers()
      sMap$clearShapes()
#      isolate({
        if (isolate(input$input_type) == "Local File Load") {
          inFile <- input$fileStn
          err <- try(pwsObject <- readRDS(inFile$datapath))
          validate(
            need(class(err)[1] == "PWStations","'\nPlease select a valid file")
          )
          pwsObject
        } else {
          if (isolate(input$input_type) == "Zip Code"){
            validate(
              need((isolate(input$input_type) == "Zip Code") &
                   (input$zipcode%in%zipcodes$zip),
                   "'\nPlease enter a valid zip code")
          )}
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
#      })
    }
  })
#
  buildWeather <- reactive({
    if(input$getWeather > 0 ){
      print("inside loadWeather")
      wMap$clearMarkers()
      wMap$clearShapes()
      wRange <<- FALSE
      validate(
        need(isolate(input$stDate) <= isolate(input$endDate),
             message = "'\nThe end date should be greater than the start date")
      )
#
      a <- as.character(isolate(input$stDate))
      date1 <- paste0(substr(a,6,7),"/",substr(a,9,10),"/",substr(a,1,4))
      time1 <- which(timesRef == isolate(input$stTime)) - 1
      b <- as.character(isolate(input$endDate))
      date2 <- paste0(substr(b,6,7),"/",substr(b,9,10),"/",substr(b,1,4))
      time2 <- which(timesRef == isolate(input$endTime)) - 1
      wRange <<- TRUE
# check if the dates were changed, only in that scenario run the API calls
      if((wInputs$wStDate[1] == as.character(isolate(input$stDate))) &
         (wInputs$wStTime[1] == as.character(isolate(input$stTime))) &
         (wInputs$wEndDate[1] == as.character(isolate(input$endDate))) &
         (wInputs$wEndTime[1] == as.character(isolate(input$endTime)))){
        obj
      } else {
        wInputs$wStDate[1] <<- as.character(isolate(input$stDate))
        wInputs$wStTime[1] <<- as.character(isolate(input$stTime))
        wInputs$wEndDate[1] <<- as.character(isolate(input$endDate))
        wInputs$wEndTime[1] <<- as.character(isolate(input$endTime))
#
        tempObj <- ProjectPWS::loadWeatherData(pwStations = obj,
                                               startDate = date1,
                                               startHour = time1,
                                               endDate = date2,
                                               endHour = time2)
# Clean-up the weather data so that inconsistent values are not shown on UI
        tempObj$weatherData <-
                   ProjectPWS::validateTrimAndFill(pwStations = tempObj,
                                                   stopAfterValidation = TRUE)
        tempObj
      }
    }
  })
#
  output$stnMap <- renderLeaflet({
    if(input$getStations > 0 ){
      print("in station map")
      obj <<- buildStn()
      print("came back from load stations")
      stnTable <- obj$stations
      pins <<- data.frame(lon=as.numeric(stnTable$lon),
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
#
  output$stnTable <- renderDataTable({
    if(input$getStations > 0){
      print("in stn table")
      obj$stations
    }
  })
#
  output$stnPlot <- renderPlot({
    if(input$getStations > 0){
      print("in stn plot")
      obj$plotStations()
    }
  })
#
  output$saveTable <- downloadHandler(
    filename = function() {
      switch(isolate(input$input_type),
             "Zip Code" = paste0("Zip",obj$zip,"Range",obj$radius,".rds"),
             "State Code" = paste0("State",obj$state,".rds"),
             "Country Code" = paste0("Country",obj$country,".rds"))
    },
    content = function(file) {
      saveRDS(obj, file = file)
    }
  )
#
  output$weatherMap <- renderLeaflet({
    if(input$getWeather > 0){
      print("in weather map")
      obj <<- buildWeather()
      print(obj$weatherData)
# The Weather map is rendered by the observe function down below
    }
  })
#
  output$weatherLegend <- renderImage({
    if(input$getWeather > 0){
      print("in weather legend")
      imageName <- switch(isolate(input$wParm),
                          "1" = "tempLegend.png",
                          "2" = "humidLegend.png",
                          "3" = "windSLegend.png",
                          "4" = "pressureLegend.png")
      print(imageName)
      print(isolate(input$wParm))
      fileName <- normalizePath(file.path('www',imageName),
                                winslash="\\")
      print(fileName)
      list(src = fileName)
    }
  })
#
  output$weatherRange <- renderUI({
    if(input$getWeather > 0){
      if(!wRange) return()
      print("in weatherRange")
      date1 <- as.Date(isolate(input$stDate),format="%Y-%m-%d")
      time1 <- which(timesRef == isolate(input$stTime))
      date2 <- as.Date(isolate(input$endDate),format="%Y-%m-%d")
      time2 <- which(timesRef == isolate(input$endTime))
      print(time1)
      print(time2)
      dateRange <- as.character(seq(from = date1, to = date2, by = 1))
      temp <- character()
      timeRange <- sapply(seq_along(dateRange),function(x){
        if(x == 1){
          if (length(dateRange) == 1) {
            temp <<- paste(dateRange[x],timesRef[time1:time2])
          } else {
            temp <<- paste(dateRange[x],timesRef[time1:length(timesRef)])
          }
        } else {
          if(length(dateRange) == x){
            temp <<- c(temp,paste(dateRange[x],timesRef[1:time2]))
          } else {
            temp <<- c(temp,paste(dateRange[x],timesRef))
          }
        }
      })
      timeRange <- temp
      selectInput("attrRange",label="Select Time",timeRange)
    }
  })
# observe block to show pop-ups on weather map
  wmapPopObs <- observe({
    print("in popup observe")
    event <- input$wMap_marker_click
    if(is.null(event)) return()
    wMap$clearPopups()
    isolate({
      parm <- switch(isolate(input$wParm),
                        "1" = "Temperature:",
                        "2" = "Humidity:",
                        "3" = "Wind Speed:",
                        "4" = "Pressure:")
      content <- as.character(
        tagList(
         paste("Station:",event$id),
         tags$br(),
         paste(parm, wPins[which(pins$id == event$id)])
        )
      )
      wMap$showPopup(event$lat, event$lng, content, event$id)
    })
  })
# observe block for changes to the weatherRange dropdown.
  wmapObs <- observe({
    print("in observe")
    if(input$getWeather == 0) return()
    if(is.null(input$attrRange)) return()
    wRangeVal <- input$attrRange
    isolate({
      tryCatch({
      dateTime <- wRangeVal
      dateW <- substr(dateTime,start=1,stop=10)
      timeW <- substr(dateTime,start=12,stop=18)
      timeW <- which(timesRef==timeW) - 1
#
      wTable <- obj$weatherData
      wIndex <- switch(isolate(input$wParm),
                       "1" = 3,
                       "2" = 4,
                       "3" = 5,
                       "4" = 6)
      wPins <<- sapply(wTable,function(x){
                 a <- which(x$day == dateW & x$hour == as.character(timeW))
                 if (length(a) == 0) {
                   NA
                 } else {
                   x[[a,wIndex]]
                 }
      })
      colors <- switch(isolate(input$wParm),
                       "1" = tempColors,
                       "2" = humidColors,
                       "3" = windSColors,
                       "4" = pressureColors)
      print(wPins)
      wMap$fitBounds(min(pins$lat),min(pins$lon),max(pins$lat),max(pins$lon))
      for(i in 1:nrow(pins)){
        if(!is.na(wPins[i])){
          wMap$addCircleMarker(lat = pins$lat[i], lng = pins$lon[i],
                               radius = 10,
                               layerId = pins$id[i],
                               options = list(color='black',
                                              weight=5,
                                              fillColor=
                                              colors[(colors$from <= wPins[i] &
                                              colors$to > wPins[i]),3],
                                              fillOpacity = 0.5))
        }
      }},
      error = function(e) {print(e)}
      )
    })
  })
#
})