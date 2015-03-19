##
##  Begin jcasale code
##
.onAttach <- function(libname, pkgname) {
  # Check for an existing API key
  apiKey <- Sys.getenv("WUNDERGROUND_API_KEY")
  if (is.na(apiKey) || apiKey == "") {
    packageStartupMessage("Thanks for using ProjectPWS. ",
                          "No API key found in the environment,",
                          " setting the variable to the default ",
                          " Key. Please use this SPARINGLY. ",
                          "Alternately, please set an environment ",
                          "variable named WUNDERGROUND_API_KEY with",
                          " your own key.")
    DEFAULT_KEY <- "0d9fae391e026223"
    Sys.setenv(WUNDERGROUND_API_KEY = DEFAULT_KEY)
  }
}

.onDetach <- function(libpath) {
  # Check if we should clean up the default key.
  apiKey <- Sys.getenv("WUNDERGROUND_API_KEY")

  DEFAULT_KEY <- "0d9fae391e026223"
  if (!is.na(apiKey) && apiKey != "" && apiKey == DEFAULT_KEY) {
    Sys.unsetenv("WUNDERGROUND_API_KEY")
  }
}
##
##  End jcasale code
##