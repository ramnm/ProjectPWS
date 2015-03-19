##
## Begin nmram Code
##
## Credits
## -------
## Please note that CSS file included in this shiny UI and the panel designs
## are inspired by the Shiny SuperZip example on the Shiny RStudio website
## Here is a link to the website
## http://shiny.rstudio.com/gallery/superzip-example.html
##
## We have also referred to many examples of Shiny packages in the 'Show me
## Shiny' website to get ideas on how to design the UI components. Here is a
## link to that website
## http://www.showmeshiny.com/
##
library(shiny)
library(leaflet)

# Loading all the data needed for the shiny UI such as weather color legends,
# zip codes, state codes etc.

load(system.file("shiny\\www\\shinyData.rda", package = "ProjectPWS"),
     envir = environment())

# The Shiny UI session function starts here

shinyUI(
  navbarPage("ProjectPWS",id="pwsNav",

# This is the Stations Map tab

    tabPanel("Stations Map",
      div(class="outer",
          tags$head(
            tags$link(rel="stylesheet", type = "text/css",
                      href = "projectPWS.css")
          ),

# Leaflet Map for the stations

          leafletMap("stnMap", width="100%", height="100%",
            initialTileLayer =
              "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            initialTileLayerAttribution =
              HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
            options=list(
              center = c(37.45, -93.85),
              zoom = 4,
              maxBounds = list(list(15.961329,-129.92981),
                               list(52.908902,-56.80481)) # Show US only
            )
          ),

# Displays the Station Selection Criteria Panel on the right side

          absolutePanel(id = "controls", class = "panel panel-default",
            fixed = TRUE, draggable = FALSE, top = 40,
            left = "auto", right = 10, bottom = "auto",
            width = 330, height = "auto", h4("Station Selection Criteria"),
            selectInput("input_type", "Input type",
                 c("Zip Code", "State Code", "Country Code",
                   "Local File Load")),
            conditionalPanel("input.input_type == 'Zip Code'",
              textInput("zipcode", label = "Zip Code")),
            conditionalPanel("input.input_type == 'Zip Code'",
              sliderInput("range",label = "Range in Miles",
                          min=0,max=25,value=3)),
            conditionalPanel("input.input_type == 'State Code'",
              selectInput("stcode","State Code",stateCd$US.State)),
            conditionalPanel("input.input_type == 'Country Code'",
              selectInput("country","Country Code",countryCd$Country)),
            conditionalPanel("input.input_type == 'Local File Load'",
              fileInput("fileStn",label = "Load Stations File")),
            actionButton("getStations",label="Get Stations")
          ),
          tags$div(id="cite",
           'Data compiled from APIs provided by www.wunderground.com'
          )
      )
    ),

# This is the Data Table tab

    tabPanel("Data Table",
             fluidRow(
               column(8,conditionalPanel("input.getStations > 0",
                                         plotOutput("stnPlot"))),
               column(4,conditionalPanel("input.getStations > 0 &
                                        input.input_type != 'Local File Load'",
                                         downloadButton('saveTable',
                                                        'Save Data Locally')))
             ),
             fluidRow(hr(),
               dataTableOutput("stnTable"))),

# This is the Weather Map tab

    tabPanel("Weather Map",
      div(class="outer",
          tags$head(
            tags$link(rel="stylesheet", type = "text/css",
                      href = "projectPWS.css")
          ),

# Leaflet Map for the Weather

          leafletMap("weatherMap", width="100%", height="100%",
            initialTileLayer =
              "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            initialTileLayerAttribution =
              HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
            options=list(
              center = c(37.45, -93.85),
              zoom = 4,
              maxBounds = list(list(15.961329,-129.92981),
                               list(52.908902,-56.80481)) # Show US only
            )
          ),

# The Weather Selection Criteria Tab on the 'Weather Map' tab

          absolutePanel(id = "controls", class = "panel panel-default",
            fixed = TRUE, draggable = FALSE, top = 40,
            left = "auto", right = 10, bottom = "auto",
            width = 330, height = "auto", h4("Weather Selection Criteria"),
            fluidRow(
              column(7,dateInput("stDate",label="Start Date",
                                 value="2015-02-01")),
              column(5,selectInput("stTime",label="Start Time",
                                   c("01:00AM","02:00AM","03:00AM","04:00AM",
                                     "05:00AM","06:00AM","07:00AM","08:00AM",
                                     "09:00AM","10:00AM","11:00AM","12:00PM",
                                     "01:00PM","02:00PM","03:00PM","04:00PM",
                                     "05:00PM","06:00PM","07:00PM","08:00PM",
                                    "09:00PM","10:00PM","11:00PM","12:00PM")))),
            fluidRow(
              column(7,dateInput("endDate",label="End Date",
                                 value="2015-02-01")),
              column(5,selectInput("endTime",label="End Time",
                                 c("01:00AM","02:00AM","03:00AM","04:00AM",
                                   "05:00AM","06:00AM","07:00AM","08:00AM",
                                   "09:00AM","10:00AM","11:00AM","12:00PM",
                                   "01:00PM","02:00PM","03:00PM","04:00PM",
                                   "05:00PM","06:00PM","07:00PM","08:00PM",
                                   "09:00PM","10:00PM","11:00PM","12:00PM")))),
            fluidRow(
              column(12,conditionalPanel("input.getStations > 0",
                        actionButton("getWeather",label="Get Weather")))),

# The below UI components are only displayed after the weather data is displayed

            fluidRow(
              column(12,conditionalPanel("input.getWeather > 0",hr(),
                        uiOutput("weatherRange")))),
            fluidRow(
              column(12,conditionalPanel("input.getWeather > 0",
                selectInput("wParm", label="Weather Attribute",
                            c("Temperature", "Humidity", "Wind Speed",
                              "Pressure"))))),
            fluidRow(
              column(12,conditionalPanel("input.getWeather > 0",
                        imageOutput("weatherLegend",width = "100%",
                                    height = "150px"))))
          ),
          tags$div(id="cite",
           'Data compiled from ',
           tags$em('APIs provided by www.wunderground.com')
          )
      )
    )
  )
)
##
## End nmram Code
##