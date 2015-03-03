#' Returns the Personal Weather Stations nearby input provided
#' @author Maruthi Ram Nadakuduru 
library(shiny)
shinyServer(function(input,output){
  output$stnTable <- renderDataTable({ 
    obj <- getStations(input$zipcode,input$range)
    obj@stations
    })
})