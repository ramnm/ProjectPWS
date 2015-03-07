#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru, Jared Casale
##
library(shiny)
library(ggmap)
shinyServer(function(input,output){
#
  output$ui11 <- renderUI({
    switch(input$input_type,
           "Zip Code" = textInput("zipcode", label = "Zip Code"),
           "State Code" = selectInput("stcode","State Code",stateCd$Code),
           "Country Code" = selectInput("country","Country Code",
                                       countryCd$ISO3),
           "Local File" = fileInput("file",label = "File")
    )
  })
#
  output$ui12 <- renderUI({
    if(input$input_type == "Zip Code") {
      sliderInput("range",label = "Range",min=0,max=40,value=0)
    }
  })
#
  buildObj <- reactive({
    if(input$getStations > 0){
      print("in buildobj")
      a <- isolate(input$zipcode)
      b <- isolate(input$range)
      c <- isolate(input$stcode)
      d <- isolate(input$country)
      switch(isolate(input$input_type),
             "Zip Code" = c <- d <- NA,
             "State Code" = a <- b <- d <- NA,
             "Country Code" = a <- b <- c <- NA)
      ProjectPWS::getStations(a, b, c, d)
    }
  })
#
  output$stnMap <- renderPlot({
    print("in stnMap server")
    if(input$getStations > 0){
      obj <- buildObj()
      stnTable <- obj$stations
      pins <- data.frame(lon=as.numeric(stnTable$long),
                         lat=as.numeric(stnTable$lat))
      mapCenter <- c(mean(pins$lon),mean(pins$lat))
      x <- get_googlemap(center=mapCenter,
                         zoom=10,
                         size=c(640,640),
                         scale=2,
                         maptype="roadmap")
      x <- ggmap(x,extent="panel")
      x <- x + geom_point(aes(x=lon,y=lat),
                          data=pins,
                          colour="DarkBlue",
                          fill="DarkBlue",
                          size=5)
                          #shape=6,
                          #alpha=.5)
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
      x <- get_googlemap(zoom=3,
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