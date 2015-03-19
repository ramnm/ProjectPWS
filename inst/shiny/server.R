##
## Begin nmram Code
##
library(leaflet)

# Loading all the data needed for the shiny UI such as weather color legends,
# zip codes, state codes etc.

load(system.file("shiny\\www\\shinyData.rda", package = "ProjectPWS"),
     envir = environment())

# Data elements defined here are available across sessions. So defining
# key variables in this section that are needed to retain value across sessions

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
pwsObject <- list()

# The Shiny Server session function starts here

shinyServer(function(input,output,session){

# Setting up the leaflet objects to control from server.r

  sMap <- createLeafletMap(session,"stnMap")
  wMap <- createLeafletMap(session,"weatherMap")
##
# This Reactive block responds to the 'Get Stations' action button
##
  buildStn <- reactive({
    if(input$getStations > 0 ){
      sMap$clearMarkers()
      sMap$clearShapes()
      isolate({
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

# Calling the getStations function to get the stations data

        ProjectPWS::getStations(zip = a,
                                radius = b,
                                state = c,
                                country = d)
      }
      })
    }
  })
##
# This Reactive block responds to the 'Get Weather' action button
##
  buildWeather <- reactive({
    if(input$getWeather > 0 ){
      wMap$clearMarkers()
      wMap$clearShapes()
      wRange <<- FALSE
      isolate({

# Validating the starting and ending time provided on the weather criteria

      validate(
        need(isolate(input$stDate) <= isolate(input$endDate),
           message = "'\nThe end date should be greater than the start date"),
        need(isolate(input$endDate) < Sys.Date(),
           message = "'\nInput dates should be less than current date")
      )

# Converting the starting time and ending time to the format required by
# loadWeatherData function

      a <- as.character(isolate(input$stDate))
      date1 <- paste0(substr(a,6,7),"/",substr(a,9,10),"/",substr(a,1,4))
      time1 <- which(timesRef == isolate(input$stTime)) - 1
      b <- as.character(isolate(input$endDate))
      date2 <- paste0(substr(b,6,7),"/",substr(b,9,10),"/",substr(b,1,4))
      time2 <- which(timesRef == isolate(input$endTime)) - 1
      wRange <<- TRUE

# Calling the loadWeatherData with the PWStations object and input criteria

      tempObj <- ProjectPWS::loadWeatherData(pwStations = obj,
                                             startDate = date1,
                                             startHour = time1,                                               endDate = date2,
                                             endHour = time2)

# Clean-up the weather data so that inconsistent values are not shown on UI

      tempObj$weatherData <-
                   ProjectPWS::validateTrimAndFill(pwStations = tempObj,
                                                 stopAfterValidation = TRUE)
      tempObj
    })
    }
  })
##
# Everytime 'Get Stations' is clicked the below block renders the leaflet map
# centering on the station lats and longs and placing markers on the map on
# the 'Stations Map' tab
##
  output$stnMap <- renderLeaflet({
    if(input$getStations > 0 ){
      obj <<- buildStn()
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
##
# Everytime 'Get Stations' is clicked the below block renders the Data table
# on the 'Data Table' tab of the UI
##
  output$stnTable <- renderDataTable({
    if(input$getStations > 0){
      obj$stations
    }
  })
##
# Everytime 'Get Stations' is clicked the below block renders the histogram
# on the 'Data Table' tab of the UI
##
  output$stnPlot <- renderPlot({
    if(input$getStations > 0){

# Calling the R6 plot stations function that returns the histogram
      obj$plotStations()
    }
  })
##
# This function handles the 'Save Data Locally' action button to allow the user
# to save the PWStations object into his local computer
##
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
##
# Everytime 'Get Weather' is clicked the below block calls the reactive
# buildWeather function defined earlier to get the weather info
##
  output$weatherMap <- renderLeaflet({
    if(input$getWeather > 0){
      obj <<- buildWeather()
# The Weather map is rendered by the observe function block down below
    }
  })
##
# Everytime 'Get Weather' is clicked the below block shows the appropriate
# weather attribute legend for the color codes
##
  output$weatherLegend <- renderImage({
      imageName <- switch(input$wParm,
                          "Temperature" = "tempLegend.png",
                          "Humidity" = "humidLegend.png",
                          "Wind Speed" = "windSLegend.png",
                          "Pressure" = "pressureLegend.png")
      fileName <- normalizePath(file.path('www',imageName),
                                winslash="\\")
      list(src = fileName)
  }, deleteFile = FALSE)
##
# Everytime 'Get Weather' is clicked the below block creates the drop down
# with an entry for each hour in the time range provided by the user.
##
  output$weatherRange <- renderUI({
    if(input$getWeather > 0){

# The wRange variable stops the Select Time drop down to be displayed if there
# was a validation error hit in the earlier buildWeather function

      if(!wRange) return()

      date1 <- as.Date(isolate(input$stDate),format="%Y-%m-%d")
      time1 <- which(timesRef == isolate(input$stTime))
      date2 <- as.Date(isolate(input$endDate),format="%Y-%m-%d")
      time2 <- which(timesRef == isolate(input$endTime))
      dateRange <- as.character(seq(from = date1, to = date2, by = 1))
      temp <- character()

# Building the drop down values

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

# Have to render this drop down from server.r because we cannot determine the
# drop down values until the user provides the start and end times

      selectInput("attrRange",label="Select Time",timeRange)
    }
  })
##
# observe block for changes to the weatherRange dropdown or weather attribute
# renders the leaflet map with the updated markings based on weatherRange and
# weather attribute selected
##
  wmapObs <- observe({

# We don't want this block to run if the weather range has not yet been rendered
# that only happens when the user comes for the first time on the 'Weather Map'
# tab and has not yet provided his search criteria and hit 'Get Weather' button

    if(is.null(input$attrRange)) return()

    wRangeVal <- input$attrRange
    wIndex <- switch(input$wParm,
                     "Temperature" = 3,
                     "Humidity"    = 4,
                     "Wind Speed"  = 5,
                     "Pressure"    = 6)
    isolate({
      tryCatch({

# Clearing any previous markers/popups on the map

      wMap$clearMarkers()
      wMap$clearShapes()

      dateTime <- wRangeVal
      dateW <- substr(dateTime,start=1,stop=10)
      timeW <- substr(dateTime,start=12,stop=18)
      timeW <- which(timesRef==timeW) - 1

# Loading the weather data fetched in the buildWeather function above

      wTable <- obj$weatherData
      wPins <<- sapply(wTable,function(x){
                 a <- which(x$day == dateW & x$hour == as.character(timeW))
                 if (length(a) == 0) {
                   NA
                 } else {
                   x[[a,wIndex]]
                 }
      })

# Choosing the corresponding weather colors data frame for color coding

      colors <- switch(input$wParm,
                       "Temperature" = tempColors,
                       "Humidity"    = humidColors,
                       "Wind Speed"  = windSColors,
                       "Pressure"    = pressureColors)

# This fits the maps bounds to include all the stations

      wMap$fitBounds(min(pins$lat),min(pins$lon),max(pins$lat),max(pins$lon))

# Now for each station the circle marker is added to the map with the right
# color coding based on the value of the weather attribute

      for(i in 1:nrow(pins)){
        if(!is.na(wPins[i])){
          col <- colors[(colors$from <= wPins[i] & colors$to > wPins[i]),3]
          wMap$addCircleMarker(lat = pins$lat[i], lng = pins$lon[i],
                               radius = 10,
                               layerId = pins$id[i],
                               options = list(color='black',
                                              weight=5,
                                              fillColor=col,
                                              fillOpacity = 0.5))
        }
      }},
      error = function(e) {print(e)}
      )
    })
  })

# observe block to show pop-ups on weather map, not working!!!

  wmapPopObs <- observe({
    event <- input$wMap_marker_click
    if(is.null(event)) return()
    wMap$clearPopups()
    isolate({
      parm <- switch(isolate(input$wParm),
                       "Temperature" = "Temperature:",
                       "Humidity"    = "Humidity:",
                       "Wind Speed"  = "Wind Speed:",
                       "Pressure"    = "Pressure:")
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
})
##
## End nmram Code
##