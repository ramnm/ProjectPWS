##
##  Begin jcasale code
##
library(ProjectPWS)
context("Loading weather data for stations")

latLongStations <- getStations(latlong = c(35.229, -80.8433),
                               radius = 2)

test_that("invalid input results in error", {
  expect_error(loadWeatherData(pwStations = "obj",
                               startDate = "3/3/2015"),
               "pwStations must be a PWStations object.")
  expect_error(loadWeatherData(pwStations = latLongStations,
                               startDate = 333),
               "startDate must be specified as a character string.")
  expect_error(loadWeatherData(pwStations = latLongStations,
                               startDate = "March 1st"),
               "invalid startDate specified.")
  expect_error(loadWeatherData(pwStations = latLongStations,
                               startDate = "3/3/2015",
                               endDate = 333),
               "endDate must be a character string if specified.")
  expect_error(loadWeatherData(pwStations = latLongStations,
                               startDate = "3/3/2015",
                               endDate = "3/2/2015"),
               "endDate must be after startDate.")
  expect_error(loadWeatherData(pwStations = latLongStations,
                               startDate = "3/3/2015",
                               endDate = "3/4/2015",
                               startHour = -1),
               "startHour must be an integer between 0 and 23.")
  expect_error(loadWeatherData(pwStations = latLongStations,
                               startDate = "3/3/2015",
                               endDate = "3/4/2015",
                               startHour = 3,
                               endHour = 25),
               "endHour must be an integer between 0 and 23.")
})

# We limit to a single station, over two days,
# since the Wunderground API severely limits
# calls for free access.

loadWeatherData(latLongStations,
                startDate = "3/3/2015",
                endDate = "3/4/2015",
                startHour = 3,
                endHour = 12,
                stationLimit = 1)

weatherData <- latLongStations$weatherData

test_that("weather data is correctly loaded", {
  expect_false(is.na(latLongStations$weatherData))
  expect_true(length(latLongStations$weatherData) == 1)
  expect_true(length(unique(weatherData[[1]]$day)) == 2)
  expect_true(nrow(weatherData[[1]]) == 34)
  expect_true(ncol(weatherData[[1]]) == 7)
  expect_true(weatherData[[1]]$hour[1] == 3)
  expect_true(weatherData[[1]]$hour[34] == 12)
})
##
##  End jcasale code
##
