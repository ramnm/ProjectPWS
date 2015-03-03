library(shiny)

shinyUI(
  fluidPage(
      titlePanel("ProjectPWS"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(12,textInput("zipcode", label = h3("Location"),
                           value = "Enter Zip code here..."))),
          fluidRow(
            column(12,sliderInput("range", label = h3("Distance in Miles"), 
                               min = 0, max = 50, value = 20))),
          fluidRow(
            column(12,actionButton("savePWS", label = "Save")))
        ),
        mainPanel(
          tabPanel('stations',dataTableOutput('stnTable'))
        ),
        position="left",
        fluid=FALSE
      )
  )
)