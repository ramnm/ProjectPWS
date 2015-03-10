suppressMessages(library(shiny))
shinyUI(
  navbarPage(h3("ProjectPWS",style="color:DarkBlue"),
             position="static-top",
    tabPanel(h3("Stations",style="color:blue"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(12,selectInput("input_type", "Input type",
                 c("Zip Code", "State Code", "Country Code", "Local File")))),
          fluidRow(
            column(12,uiOutput("ui11"),
                      uiOutput("ui12"),
                      actionButton("getStations",label="Get Stations"))),
          fluidRow(
            column(12,uiOutput("uiSave1"),
                      uiOutput("uiSave2"))),
          fluidRow(
            column(12,uiOutput("uiSave3")))
        ),
        mainPanel(
          tabsetPanel(
            id = "display",
            tabPanel("Map",plotOutput("stnMap")),
            tabPanel("Table",dataTableOutput('stnTable'))
          )
        )
      )
    ),
    tabPanel(h3("Weather",style="color:blue"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(7,dateInput("stDate",label="Start Date",
                               value="2015-02-01")),
            column(5,selectInput("stTime",label="Time",
                                 c("01:00AM","02:00AM","03:00AM","04:00AM",
                                   "05:00AM","06:00AM","07:00AM","08:00AM",
                                   "09:00AM","10:00AM","11:00AM","12:00PM",
                                   "01:00PM","02:00PM","03:00PM","04:00PM",
                                   "05:00PM","06:00PM","07:00PM","08:00PM",
                                   "09:00PM","10:00PM","11:00PM","12:00PM")))),
          fluidRow(
            column(7,dateInput("endDate",label="End Date",
                               value="2015-02-01")),
            column(5,selectInput("endTime",label="Time",
                                 c("01:00AM","02:00AM","03:00AM","04:00AM",
                                   "05:00AM","06:00AM","07:00AM","08:00AM",
                                   "09:00AM","10:00AM","11:00AM","12:00PM",
                                   "01:00PM","02:00PM","03:00PM","04:00PM",
                                   "05:00PM","06:00PM","07:00PM","08:00PM",
                                   "09:00PM","10:00PM","11:00PM","12:00PM")))),
          fluidRow(
            column(12,radioButtons("wParm",label="Weather Attribute",
                                   choices=list("Temperature"=1,
                                                "Humidity"=2,
                                                "Wind Speed"=3,
                                                "Pressure"=4),selected=1))),
          fluidRow(
            column(12,actionButton("getWeather",label="Get Weather")))
        ),
        mainPanel(
          plotOutput("weatherMap")
        )
      )
    )
  )
)