#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru, Jared Casale
##
library(shiny)
library(ggmap)
shinyServer(function(input,output){
#
  savePanel <- reactive({
    if(input$getStations > 0){
      TRUE
    } else {FALSE}
  })
#
  buildObj <- reactive({
    if(input$getStations > 0){
      print("in buildobj")
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
      ProjectPWS::getStations(zip = a, radius = b, state = c, country = d)
    }
  })
#
  output$ui11 <- renderUI({
    switch(input$input_type,
           "Zip Code" = textInput("zipcode", label = "Zip Code"),
           "State Code" = selectInput("stcode","State Code",stateCd$US.State),
           "Country Code" = selectInput("country","Country Code",
                                       countryCd$Country),
           "Local File" = fileInput("file",label = "File")
    )
  })
#
  output$ui12 <- renderUI({
    if(input$input_type == "Zip Code") {
      sliderInput("range",label = "Range",min=0,max=40,value=3)
    }
  })
#
  output$uiSave1 <- renderUI({
    if(savePanel()) hr()
  })
#
  output$uiSave2 <- renderUI({
    if(savePanel()) textInput("fileName",label="File Name")
  })
#
  output$uiSave3 <- renderUI({
    if(savePanel()) actionButton("save",label="Save Data")
  })
#
  saveData <- reactive({
    if(input$save > 0){
      validate(
        need(is.null(isolate(input$fileName)),"Please enter a file name")
      )
    }
  })
#
  output$fileName <- renderText({
    saveData()
    savePWSTable(obj$stations,isolate(input$fileName))
  })
#
  output$stnMap <- renderPlot({
    print("in stnMap server")
    if(input$getStations > 0){
      obj <- buildObj()
      stnTable <- obj$stations
      pins <- data.frame(lon=as.numeric(stnTable$lon),
                         lat=as.numeric(stnTable$lat))
      mapCenter <- c(mean(pins$lon),mean(pins$lat))
      zoomVal <- 19 - as.integer(mean(c(abs((max(pins$lon)- min(pins$lon))/6),
                                 abs((max(pins$lat)- min(pins$lat))/5))))
      print(zoomVal)
      x <- get_googlemap(center=mapCenter,
                         zoom=zoomVal,
                         size=c(640,640),
                         scale=2,
                         maptype="roadmap")
      x <- ggmap(x,extent="panel")
      x <- x + geom_point(aes(x=lon,y=lat),
                          data=pins,
                          colour="DarkBlue",
                          fill="DarkBlue",
                          size=4,
                          #shape=6,
                          alpha=.2)
      print(x)
    }
  }, width=800, height=800)
#
  output$stnTable <- renderDataTable({
    print("in stnTable server")
    if(input$getStations > 0){
      obj <- buildObj()
      obj$stations
    }
  })
#
  output$weatherMap <- renderPlot({
      x <- get_googlemap(zoom=21,
                         size=c(640,640),
                         scale=2,
                         maptype="roadmap")
      x <- ggmap(x,extent="panel")
#      x <- x + geom_point(aes(x=lon,y=lat),
#                          data=pins,
#                          colour="Blue",
#                          size=2.5,
#                          shape=6,
#                          alpha=.5)
      print(x)
  }, width=800, height=800)
})