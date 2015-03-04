#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru, Jared Casale
##
library(shiny)
shinyServer(function(input,output){
  output$ui <- renderUI({
    if(is.null(input$input_type)) return()
    switch(input$input_type,
      "Zip Code" = textInput("zipcode", label = "Zip Code"),
      "State Code" = selectInput("stcode","State Code",stateCd$Code),
      "Country Code" = selectInput("country","Country Code",
                                   countryCd$ISO3),
      "Local File" = fileInput("file",label = "File")
    )
  })
#
  output$ui2 <- renderUI({
    if(is.null(input$input_type)) return()
    switch(input$input_type,
      "Zip Code" = sliderInput("range",label = "Range",
                               min=0,max=40,value=0),
      "Local File" = verbatimTextOutput("value")
    )
  })
#
  output$ui3 <- renderUI({
    if(is.null(input$input_type)) return()
    actionButton("getData",label="Get Stations")
  })
#
  output$stnTable <- renderDataTable({
    if(input$getData == 0) return()
    a <- isolate(input$zipcode)
    b <- isolate(input$range)
    c <- isolate(input$stcode)
    d <- isolate(input$country)
    switch(isolate(input$input_type),
                   "Zip Code" = c <- d <- NA,
                   "State Code" = a <- b <- d <- NA,
                   "Country Code" = a <- b <- c <- NA)
    obj <- ProjectPWS::getStations(a, b, c, d)
    obj$stations
  })
})