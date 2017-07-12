#'
#' The scrapeData script
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' It contains main and helper functions to crawl the energy systems data of the ENTSO-E transparency platform
#' @references \url{https://transparency.entsoe.eu}
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'  Build and Reload Package:  'Cmd + Shift + B'
#'  Check Package:             'Cmd + Shift + E'
#'  Test Package:              'Cmd + Shift + T'
#'

#'---------------------------------------------------------


#' @title setLogging
#'
#' @description This function sets a global options variable called "logging" to TRUE OR FALSE. By default it is FALSE, so no logging is displayed.
#'
#' @param logger - A boolean variable. TRUE for printing out the logging outputs in the console.
#'
#'
#' @export
#'
setLogging <- function(logger) {
  options("logging" = logger)
  ifelse(logger == TRUE, print("Outputs/logs will be displayed!"), print("No console outputs/logs will be displayed!"))
}




#' @title getData
#'
#' @description
#'
#' @param startDate
#' @param endDate
#'
#' @return
#'
#' @examples
#'
#'
#' @export
#'
getData <- function(startDate, endDate) {
  library(logging)

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getReserveNeeds_", gsub(":", "", as.character(Sys.time())), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')



  return(paste(startDate, endDate))

}






