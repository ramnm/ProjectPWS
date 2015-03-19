##
##  Begin jcasale code
##
library(ProjectPWS)
context("Loading weather stations")

test_that("invalid input results in error", {
  expect_error(getStations(),
               "A valid location must be specified.")
  expect_error(getStations(radius = 5),
               "A valid location must be specified.")
  expect_error(getStations(latlong = "bogus",
                           radius = "5"),
               "Radius must be numeric.")
  expect_error(getStations(latlong = "bogus",
                           radius = 5),
               "Latlong must be two numeric values.")
  expect_error(getStations(latlong = 35.229,
                           radius = 5),
               "Latlong must be two numeric values.")
  expect_error(getStations(latlong = c(35.229,
                                       -80.8433),
                           zip = "90210",
                           radius = 5),
               "Please specify a single location parameter")
  expect_error(getStations(zip = "902",
                           radius = 5),
               "Please specify a valid zipcode string.")
  expect_error(getStations(state = "Wash",
                           radius = 5),
               "Please specify state as a length 2 character string.")
  expect_error(getStations(country = 5,
                           radius = 5),
               "Please specify country as a chracter string.")
  expect_error(getStations(city = 5,
                           radius = 5),
               "Please specify city as a chracter string.")
})


# Testing some of the functions requires LOTS of calls, only do a
# basic test here.

# Skip if we don't have a valid key or can't connect
checkApiConnection <- function() {
  error <- FALSE
  result = tryCatch({
    apiKey <- Sys.getenv("WUNDERGROUND_API_KEY")
    testurl <-
      sprintf("http://api.wunderground.com/api/%s/geolookup/q/98107.xml",
              apiKey)
    pwsXML <- XML::xmlTreeParse(testurl, useInternalNodes = TRUE)
    pwsRoot <- XML::xmlRoot(pwsXML)
    pwsNamespace <- XML::getDefaultNamespace(pwsRoot)

    # Check for errors
    errorNodes <- XML::getNodeSet(pwsRoot,
                                  "/response/error",
                                  pwsNamespace)

    if (length(errorNodes) > 0) {
      error <- TRUE
    }
  }, error = function(e) {
    error <<- TRUE
  })

  if (error) {
    skip("Unable to query API due to connection or invalid API Key")
  }
}

test_that("stations are queried correctly", {
  checkApiConnection()
  latLongStations <- getStations(latlong = c(35.229, -80.8433),
                                 radius = 10)

  expect_true(nrow(latLongStations$stations) > 1)
  expect_true("Charlotte" %in% latLongStations$stations$city)
  expect_true(max(latLongStations$stations$distance_mi) <= 10)
  expect_equal(latLongStations$queryArg, "35.229,-80.8433")
})
##
##  End jcasale code
##
