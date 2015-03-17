library(ProjectPWS)
context("Validating, trimming and filling weather data")

# This test relies on auto-attach of testThatStations,
# a PWStations object with stations and weather data
# pre-loaded.
# To regenerate testThatStations (stored with package),
# run the following:

# testThatStations <- getStations(latlong = c(35.229,
#                                              -80.8433),
#                                  radius = 10)
# loadWeatherData(testThatStations,
#                 startDate = "3/3/2015",
#                 endDate = "3/4/2015",
#                 startHour = 3,
#                 endHour = 12,
#                 stationLimit = 1)

# Set some invalid values
testThatStations$weatherData[[1]]$tempi[c(1, 8, 9)] <- c(-135, 136, 135)
testThatStations$weatherData[[1]]$hum[c(2, 3, 12)] <- c(-1, 101, 100)
testThatStations$weatherData[[1]]$wspdi[c(4, 15, 16)] <- c(-1, 260, 250)
testThatStations$weatherData[[1]]$pressure[c(3, 7, 13)] <- c(19, 34, 20)

# Note that wspdi is completely invalid in the testThatTable.

justValidatedData <-
  validateTrimAndFill(testThatStations,
                      stopAfterValidation = TRUE)

test_that("invalid values are filled with NA", {
  expect_true(sum(is.na(justValidatedData[[1]])) -
                    sum(is.na(testThatStations$weatherData[[1]])) ==
                                          8 + nrow(justValidatedData[[1]]) - 3)
})

justValAndTrimData <-
  validateTrimAndFill(testThatStations,
                      stopAfterTrim = TRUE)

test_that("columns with less than 2 data points are dropped", {
  expect_true(sum(is.na(justValAndTrimData[[1]])) == 4)
})

imputedData <- validateTrimAndFill(testThatStations)

test_that("all missing data is imputed", {
  expect_true(sum(is.na(imputedData[[1]])) == 0)
})