library(shiny)

shiny::shinyServer(function(input, output) {
  output$stnTable <- renderDataTable({ 
    obj <- getStations(input$zipcode, input$range)
    obj@stations
    })
})