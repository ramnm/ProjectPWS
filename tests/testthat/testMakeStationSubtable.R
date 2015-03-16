library(ProjectPWS)
context("Subsetting station tables")

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

test_that("invalid input results in error", {
  expect_error(makeStationSubtable(),
               "Please specify a filter criteria.")
  expect_error(makeStationSubtable(pwStations = testThatStations,
                                   newRadius = -1),
               "Please specify a positive numeric value for newRadius.")
})

# Decrease radius to 5 miles
decRadiusStations <- makeStationSubtable(testThatStations,
                                         newRadius = 5)

test_that("radius is decreased", {
  expect_true(max(decRadiusStations$stations$distance_mi) <= 5)
})

# Keep 3 closest
closestStations <- makeStationSubtable(testThatStations,
                                       numberToKeep = 3)

test_that("kept 3 closest stations", {
  expect_true(nrow(closestStations$stations) == 3)
  expect_true(max(closestStations$stations$distance_mi) <=
                max(testThatStations$stations$distance_mi))
})

# Keep 3 farthest
farthestStations <- makeStationSubtable(testThatStations,
                                        numberToKeep = 3,
                                        nearest = FALSE)

test_that("kept 3 farthest stations", {
  expect_true(nrow(farthestStations$stations) == 3)
  expect_true(min(farthestStations$stations$distance_mi) >=
                min(testThatStations$stations$distance_mi))
})

# Particular stations
particularStations <-
  makeStationSubtable(testThatStations,
                       stationNames =
                         testThatStations$stations$id[c(1, 2)])

test_that("2 stations kept by name", {
  expect_true(nrow(particularStations$stations) == 2)
})
