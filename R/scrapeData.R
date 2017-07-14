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




#' @title getLoadDayAheadVsActual
#'
#' @description This method retrieves the actual and one day-ahead forecast of the energy load for the four german TSO and the Netzregelverbund (summation).
#' The resolution of the data is in discrete 15 minutes timesteps (CET (UTC+1) / CEST (UTC+2)) and the values are presented in MW.
#'
#' NOTE:
#'  - Always downloads for the whole year and then subsets the data to the given time period (1-2MB). This is done for each TSO
#'  - For every request a Login POST request is needed. For Login a pseudo account is used.
#'
#' @param startDate date format is YYYY-MM-DD
#' @param endDate date format is YYYY-MM-DD
#'
#' @return a data.frame object with the actual and the day-ahead load forecast of the four german TSO
#'
#' @examples
#' loadData <- getLoadDayAheadVsActual("2017-01-01", "2017-06-30")
#'
#' @export
#'
getLoadDayAheadVsActual <- function(startDate, endDate) {
  library(logging)
  library(httr)
  library(XML)
  library(dplyr)
  library(lubridate)


  # TODO: check if time period exceeds one year --> seperate request into years

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getLoadDayAheadVsActual", gsub(":", "", as.character(Sys.time())), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')



  res <- list()

  tsos <- c("50Hz", "Amprion", "TenneT", "TransnetBW")

  for(t in 1:length(tsos)) {

    # date format is YYYY-MM-DD
    d <- getLoadDataForTSO(startDate, endDate, tsos[t])

    # Only add DateTime column once at the beginning
    if(t > 1) {
      res <- c(res, list(d[,2:3]))
    } else {
      res <- c(res, list(d))
    }

  }

  res <- as.data.frame(res)

  # Add the sumed loads for the Netzregelverbund
  res$ForecastLoad_Netzregelverbund <- res %>% select(starts_with("Forecast")) %>% apply(1, sum)
  res$Actuaload_Netzregelverbund <- res %>% select(starts_with("Actual")) %>% apply(1, sum)


  if(getOption("logging")) logdebug(paste("getLoadDayAheadVsActual - DONE"))


  return(res)




}

# helper function for @seealso getLoadDayAheadVsActual
# returns the data.frame for a tso
getLoadDataForTSO <- function(startDate, endDate, t){

  res <- data.frame()

  # Payload Parameters with example data:
  #
  # name	""
  # defaultValue	"false"
  # viewType	"TABLE"
  # areaType	"CTA"
  # atch	"false"
  # dateTime.dateTime	"12.07.2017+00:00|CET|DAY"
  # biddingZone.values	"CTY|10Y1001A1001A83F!CTA|10YDE-ENBW-----N"
  # dateTime.timezone	"CET_CEST"
  # dateTime.timezone_input	"CET+(UTC+1)+/+CEST+(UTC+2)"
  # dataItem	"ALL"
  # timeRange =	"YEAR" // --> retrieves the data of the year of the given date (not passed days are filled with "-") or "DEFAULT" (--> data of the specified day)
  # exportType	"CSV"


  # TSO Codes:
  # hz = "CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2" # 50Hz
  # amprion = "CTY|10Y1001A1001A83F!CTA|10YDE-RWENET---I" # Amprion
  # tennet = "CTY|10Y1001A1001A83F!CTA|10YDE-EON------1" # TenneT
  # transnet = "CTY|10Y1001A1001A83F!CTA|10YDE-ENBW-----N" # TransnetBW
  #

  # Set the right TSO Code for the tso in the loop
  if(t == "50Hz") {
    tso = "CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2"
  }
  else if(t == "Amprion") {
    tso = "CTY|10Y1001A1001A83F!CTA|10YDE-RWENET---I"
  }
  else if(t == "TenneT") {
    tso = "CTY|10Y1001A1001A83F!CTA|10YDE-EON------1"
  }
  else if(t == "TransnetBW") {
    tso = "CTY|10Y1001A1001A83F!CTA|10YDE-ENBW-----N"
  }

  if(getOption("logging")) logdebug(paste("getLoadDayAheadVsActual - Get the data for TSO", t))

  # LOGIN with a pseudo account
  #
  if(getOption("logging")) logdebug(paste("getLoadDayAheadVsActual - Logging in ..."))

  # Login mail: bipriota@wegwerfemail.info, password: 123123
  login <- list(
    username = "bipriota@wegwerfemail.info",
    password = "123123",
    submit = "Sign In"
  )

  # root url + value in input form at atrribute action here: action="/value"
  # r will never be used again
  r <- POST("https://transparency.entsoe.eu/login", body = login, encode = "form") # , verbose()


  # Once logged in --> get the data
  # Get the years of the time period --> download by years and then subset
  dateArray = paste(seq(year(as.Date(sdate)), year(as.Date(edate)), by = 1), "-01-01", sep = "")

  # for each year retrieve the data
  for(i in 1:length(dateArray)) {

    if(getOption("logging")) logdebug(paste("getLoadDayAheadVsActual - Get the data for year", dateArray[i]))


    # Date Code: e.g. "12.07.2017+00:00|CET|DAY"
    # "%d.%m.%Y" + "+00:00|CET|DAY" --> this suffix stays the same
    #
    date = paste(format(as.Date(dateArray[i]), "%d.%m.%Y"), "+00:00|CET|DAY", sep = "")


    # https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/export?name=&defaultValue=false&viewType=TABLE&areaType=CTA&atch=false&dateTime.dateTime=12.07.2017+00:00|CET|DAY&biddingZone.values=CTY|10Y1001A1001A83F!CTA|10YDE-ENBW-----N&dateTime.timezone=CET_CEST&dateTime.timezone_input=CET+(UTC+1)+/+CEST+(UTC+2)&dataItem=ALL&timeRange=YEAR&exportType=CSV
    url = 'https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/export?';

    request = paste(
      'name=', "","&",
      'defaultValue=', "false","&",
      'viewType=', 'TABLE',"&",
      'areaType=', 'CTA',"&",
      'atch=', "false","&",
      'dateTime.dateTime=', date,"&",
      'biddingZone.values=', tso,"&",
      'dateTime.timezone=', "CET_CEST","&",
      'dateTime.timezone_input=',"CET+(UTC+1)+/+CEST+(UTC+2)","&",
      'dataItem=', "ALL","&",
      'timeRange=', "YEAR","&",
      'exportType=', "CSV", sep = ""
    );

    url = paste(url, request, sep = "")


    data <- GET(url) # , verbose()

    # Read in the file
    data <- read.table(text = content(data, "text"), sep = ",",
                       header = TRUE,
                       na.strings  = "-",
                       colClasses = c("character", "character", "character"),
                       col.names = c("DateTime", paste("ForecastLoad_", t, sep = ""), paste("ActualLoad_", t, sep = "")))

    res <- rbind(res, data)
  }


  # Format the data.frame
  res[,2] <- as.numeric(res[,2]) # Forecasted Load
  res[,3] <- as.numeric(res[,3]) # Actual Load

  l <- strsplit(res$DateTime, " - ")
  f <- function(x) x[1]

  res$DateTime <- as.POSIXct(unlist(lapply(l, f)), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")

  # TODO --> subset data to the given time period
  res <- res %>% filter(DateTime >= as.POSIXct(startDate, tz = "Europe/Berlin") & DateTime <= as.POSIXct(endDate, tz = "Europe/Berlin"))


  return(res)


}



