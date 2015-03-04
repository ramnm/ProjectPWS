library(shiny)
shinyUI(
  fluidPage(
      titlePanel("ProjectPWS"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(12, wellPanel(selectInput("input_type", "Input type",
                   c("Zip Code", "State Code", "Country Code", "Local File"))))
                  ),
          fluidRow(
            column(12,wellPanel(uiOutput("ui")))),
          fluidRow(
            column(12,wellPanel(uiOutput("ui2")))),
          fluidRow(
            column(12,wellPanel(uiOutput("ui3"))))
        ),
        mainPanel(
          tabPanel('stations',dataTableOutput('stnTable'))
        ),
        position="left",
        fluid=FALSE
      )
  )
)