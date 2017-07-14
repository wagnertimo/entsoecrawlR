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
#' @param startDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
#' @param endDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
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
  res$Forecast_Load_Netzregelverbund <- res %>% select(starts_with("Forecast")) %>% apply(1, sum, na.rm=TRUE)
  res$Actual_Load_Netzregelverbund <- res %>% select(starts_with("Actual")) %>% apply(1, sum, na.rm=TRUE)


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
                       col.names = c("DateTime", paste("Forecast_Load_", t, sep = ""), paste("Actual_Load_", t, sep = "")))

    res <- rbind(res, data)
  }


  # Format the data.frame
  res[,2] <- as.numeric(res[,2]) # Forecasted Load
  res[,3] <- as.numeric(res[,3]) # Actual Load

  l <- strsplit(res$DateTime, " - ")
  f <- function(x) x[1]

  res$DateTime <- as.POSIXct(unlist(lapply(l, f)), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")

  # TODO --> subset data to the given time period by day
  #res <- res %>% filter(date(DateTime) >= date(as.POSIXct(startDate, tz = "Europe/Berlin")) & date(DateTime) <= date(as.POSIXct(endDate, tz = "Europe/Berlin")))
  res <- res %>% filter(DateTime >= as.POSIXct(paste(startDate, "00:00:00", sep=""), tz = "Europe/Berlin") & DateTime <= as.POSIXct(paste(endDate, "23:59:59", sep=""), tz = "Europe/Berlin"))


  return(res)


}






#' @title getWindSolarDayAheadGeneration
#'
#' @description This method retrieves the actual and one day-ahead forecast of the energy generation in Wind (Onshore, Offshore) and Solar for the four german TSO and the Netzregelverbund (summation).
#' The resolution of the data is in discrete 15 minutes timesteps (CET (UTC+1) / CEST (UTC+2)) and the values are presented in MW.
#'
#' NOTE:
#'  - Always downloads for the whole year and then subsets the data to the given time period (1-2MB). This is done for each TSO
#'  - For every request a Login POST request is needed. For Login a pseudo account is used.
#'
#' @param startDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
#' @param endDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
#'
#' @return a data.frame object with the day-ahead generation forecast in Wind (Onshore, Offshore) and Solar of the four german TSO
#'
#' @examples
#' loadData <- getWindSolarDayAheadVsActual("2017-01-01", "2017-06-30")
#'
#' @export
#'
getWindSolarDayAheadGeneration <- function(startDate, endDate) {
  library(logging)
  library(httr)
  library(XML)
  library(dplyr)
  library(lubridate)


  # TODO: check if time period exceeds one year --> seperate request into years

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getWindSolarDayAheadGeneration", gsub(":", "", as.character(Sys.time())), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')



  res <- list()

  tsos <- c("50Hz", "Amprion", "TenneT", "TransnetBW")

  for(t in 1:length(tsos)) {

    # date format is YYYY-MM-DD
    d <- getWindSolarDataForTSO(startDate, endDate, tsos[t])

    # Only add DateTime column once at the beginning
    if(t > 1) {
      res <- c(res, list(d[,2:5]))
    } else {
      res <- c(res, list(d))
    }

  }

  res <- as.data.frame(res)

  # Add the sumed loads for the Netzregelverbund
  res$WindOffshore_Generation_Forecast_Netzregelverbund <- res %>% select(starts_with("Offshore")) %>% apply(1, sum, na.rm=TRUE)
  res$WindOnshore_Generation_Forecast_Netzregelverbund <- res %>% select(starts_with("Onshore")) %>% apply(1, sum, na.rm=TRUE)
  res$Solar_Generation_Forecast_Netzregelverbund <- res %>% select(starts_with("Solar")) %>% apply(1, sum, na.rm=TRUE)
  res$Sum_Generation_Forecast_Netzregelverbund <- res %>% select(starts_with("Sum")) %>% apply(1, sum, na.rm=TRUE)


  if(getOption("logging")) logdebug(paste("getWindSolarDayAheadGeneration - DONE"))


  return(res)




}

# helper function for @seealso getWindSolarDayAheadGeneration
# returns the data.frame for a tso
getWindSolarDataForTSO <- function(startDate, endDate, t){

  res <- data.frame()

  # Payload Parameters with example data:
  #
  # name	""
  # defaultValue	"false"
  # viewType	"TABLE"
  # areaType	"CTA"
  # atch	"false"
  # datepicker-day-offset-select-dv-date-from_input	"D"
  # dateTime.dateTime	"14.07.2017+00:00|CET|DAYTIMERANGE"
  # dateTime.endDateTime	"14.07.2017+00:00|CET|DAYTIMERANGE"
  # area.values	"CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2"
  # productionType.values	[3]
  # 0	"B16"
  # 1	"B18"
  # 2	"B19"
  # dateTime.timezone	"CET_CEST"
  # dateTime.timezone_input	"CET+(UTC+1)+/+CEST+(UTC+2)"
  # dataItem	"ALL"
  # timeRange	"YEAR" // --> retrieves the data of the year of the given date (not passed days are filled with "-") or "DEFAULT" (--> data of the specified day)
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

  if(getOption("logging")) logdebug(paste("getWindSolarDayAheadGeneration - Get the data for TSO", t))

  # LOGIN with a pseudo account
  #
  if(getOption("logging")) logdebug(paste("getWindSolarDayAheadGeneration - Logging in ..."))

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

    if(getOption("logging")) logdebug(paste("getWindSolarDayAheadGeneration - Get the data for year", dateArray[i]))


    # Date Code: e.g. "12.07.2017+00:00|CET|DAYTIMERANGE"
    # "%d.%m.%Y" + "+00:00|CET|DAYTIMERANGE" --> this suffix stays the same
    #
    date = paste(format(as.Date(dateArray[i]), "%d.%m.%Y"), "+00:00|CET|DAYTIMERANGE", sep = "")


    # https://transparency.entsoe.eu/generation/r2/dayAheadGenerationForecastWindAndSolar/export?name=&defaultValue=false&viewType=TABLE&areaType=CTA&atch=false&datepicker-day-offset-select-dv-date-from_input=D&dateTime.dateTime=14.07.2017+00:00|CET|DAYTIMERANGE&dateTime.endDateTime=14.07.2017+00:00|CET|DAYTIMERANGE&area.values=CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2&productionType.values=B16&productionType.values=B18&productionType.values=B19&dateTime.timezone=CET_CEST&dateTime.timezone_input=CET+(UTC+1)+/+CEST+(UTC+2)&dataItem=ALL&timeRange=YEAR&exportType=CSV
    url = 'https://transparency.entsoe.eu/generation/r2/dayAheadGenerationForecastWindAndSolar/export?';

    request = paste(
      'name=', "","&",
      'defaultValue=', "false","&",
      'viewType=', 'TABLE',"&",
      'areaType=', 'CTA',"&",
      'atch=', "false","&",
      'datepicker-day-offset-select-dv-date-from_input', "D", "&",
      'dateTime.dateTime=', date,"&",
      'dateTime.endDateTime=', date,"&",
      'area.values=', tso,"&",
      'productionType.values=', "B16","&",
      'productionType.values=', "B18","&",
      'productionType.values=', "B19","&",
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
                       col.names = c("DateTime",
                                     paste("Sum_Generation_Forecast_", t, sep = ""),
                                     paste("Solar_Generation_Forecast_", t, sep = ""),
                                     paste("WindOffshore_Generation_Forecast_", t, sep = ""),
                                     paste("WindOnshore_Generation_Forecast_", t, sep = ""))
                       )

    res <- rbind(res, data)
  }


  # Format the data.frame
  res[,2] <- as.numeric(res[,2]) # Generation Sum
  res[,3] <- as.numeric(res[,3]) # Solar Generation Forecast
  res[,4] <- as.numeric(res[,4]) # Offshore Generation Forecast
  res[,5] <- as.numeric(res[,5]) # Onshore Generation Forecast

  l <- strsplit(res$DateTime, " - ")
  f <- function(x) x[1]

  res$DateTime <- as.POSIXct(unlist(lapply(l, f)), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")

  # # TODO --> subset data to the given time period by day
  # res <- res %>% filter(date(DateTime) >= date(as.POSIXct(startDate, tz = "Europe/Berlin")) & date(DateTime) <= date(as.POSIXct(endDate, tz = "Europe/Berlin")))
  res <- res %>% filter(DateTime >= as.POSIXct(paste(startDate, "00:00:00", sep=""), tz = "Europe/Berlin") & DateTime <= as.POSIXct(paste(endDate, "23:59:59", sep=""), tz = "Europe/Berlin"))


  return(res)


}









#' @title getActualGeneration
#'
#' @description This method retrieves the actual energy generation for different product typse of the four german TSO and the Netzregelverbund (summation).
#' The resolution of the data is in discrete 15 minutes timesteps (CET (UTC+1) / CEST (UTC+2)) and the values are presented in MW.
#' The product types are Biomass, Fossil Brown coal/Lignite, Fossil Coal-derived gas, Fossil Gas, Fossil Hard coal, Fossil Oil, Fossil Oil shale, Fossil Peat, Geothermal, Hydro Pumped Storage, Hydro Run-of-river and poundage, Hydro Water Reservoir, Marine, Nuclear, Other, Other renewable, Solar, Waste, Wind Offshore, Wind Onshore.
#'
#' NOTE:
#'  - Always downloads for the whole year and then subsets the data to the given time period (3-4MB). This is done for each TSO
#'  - For every request a Login POST request is needed. For Login a pseudo account is used.
#'
#' @param startDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
#' @param endDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
#'
#' @return a data.frame object with the actual power generation per product type of the four german TSO
#'
#' @examples
#' loadData <- getActualGeneration("2017-01-01", "2017-06-30")
#'
#' @export
#'
getActualGeneration <- function(startDate, endDate) {
  library(logging)
  library(httr)
  library(XML)
  library(dplyr)
  library(lubridate)


  # TODO: check if time period exceeds one year --> seperate request into years

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getActualGeneration", gsub(":", "", as.character(Sys.time())), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')



  res <- list()

  tsos <- c("50Hz", "Amprion", "TenneT", "TransnetBW")

  for(t in 1:length(tsos)) {

    # date format is YYYY-MM-DD
    d <- getGenerationDataForTSO(startDate, endDate, tsos[t])

    # Only add DateTime column once at the beginning
    if(t > 1) {
      res <- c(res, list(d[,2:ncol(d)]))
    } else {
      res <- c(res, list(d))
    }

  }

  res <- as.data.frame(res)

  # Add the sumed loads for the Netzregelverbund
  res$Biomass_Aggregated_Netzregelverbund <- res %>% select(starts_with("Biomass_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Biomass_Aggregated_Netzregelverbund <- res %>% select(starts_with("Biomass_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilBrown_Aggregated_Netzregelverbund <- res %>% select(starts_with("FossilBrown_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilBrown_Consumption_Netzregelverbund <- res %>% select(starts_with("FossilBrown_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilCoalGas_Aggregated_Netzregelverbund <- res %>% select(starts_with("FossilCoalGas_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilCoalGas_Consumption_Netzregelverbund <- res %>% select(starts_with("FossilCoalGas_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilGas_Aggregated_Netzregelverbund <- res %>% select(starts_with("FossilGas_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilGas_Consumption_Netzregelverbund <- res %>% select(starts_with("FossilGas_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilHardCoal_Aggregated_Netzregelverbund <- res %>% select(starts_with("FossilHardCoal_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilHardCoal_Consumption_Netzregelverbund <- res %>% select(starts_with("FossilHardCoal_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilOil_Aggregated_Netzregelverbund <- res %>% select(starts_with("FossilOil_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilOil_Consumption_Netzregelverbund <- res %>% select(starts_with("FossilOil_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilOilShale_Aggregated_Netzregelverbund <- res %>% select(starts_with("FossilOilShale_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilOilShale_Consumption_Netzregelverbund <- res %>% select(starts_with("FossilOilShale_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilPeat_Aggregated_Netzregelverbund <- res %>% select(starts_with("FossilPeat_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$FossilPeat_Consumption_Netzregelverbund <- res %>% select(starts_with("FossilPeat_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$Geothermal_Aggregated_Netzregelverbund <- res %>% select(starts_with("Geothermal_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Geothermal_Consumption_Netzregelverbund <- res %>% select(starts_with("Geothermal_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$HydroPumpedStorage_Aggregated_Netzregelverbund <- res %>% select(starts_with("HydroPumpedStorage_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$HydroPumpedStorage_Consumption_Netzregelverbund <- res %>% select(starts_with("HydroPumpedStorage_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$HydroRiverPoundage_Aggregated_Netzregelverbund <- res %>% select(starts_with("HydroRiverPoundage_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$HydroRiverPoundage_Consumption_Netzregelverbund <- res %>% select(starts_with("HydroRiverPoundage_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$WaterReservoir_Aggregated_Netzregelverbund <- res %>% select(starts_with("WaterReservoir_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$WaterReservoir_Consumption_Netzregelverbund <- res %>% select(starts_with("WaterReservoir_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$Nuclear_Aggregated_Netzregelverbund <- res %>% select(starts_with("Nuclear_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Nuclear_Consumption_Netzregelverbund <- res %>% select(starts_with("Nuclear_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$Other_Aggregated_Netzregelverbund <- res %>% select(starts_with("Other_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Other_Consumption_Netzregelverbund <- res %>% select(starts_with("Other_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$Other_RenewableAggregated_Netzregelverbund <- res %>% select(starts_with("OtherRenewable_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Other_RenewableConsumption_Netzregelverbund <- res %>% select(starts_with("OtherRenewable_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$Solar_Aggregated_Netzregelverbund <- res %>% select(starts_with("Solar_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Solar_Consumption_Netzregelverbund <- res %>% select(starts_with("Solar_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$Waste_Aggregated_Netzregelverbund <- res %>% select(starts_with("Waste_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Waste_Consumption_Netzregelverbund <- res %>% select(starts_with("Waste_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$WindOffshore_Aggregated_Netzregelverbund <- res %>% select(starts_with("WindOffshore_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$WindOffshore_Consumption_Netzregelverbund <- res %>% select(starts_with("WindOffshore_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$WindOnshore_Aggregated_Netzregelverbund <- res %>% select(starts_with("WindOnshore_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$WindOnshore_Consumption_Netzregelverbund <- res %>% select(starts_with("WindOnshore_Consumption_")) %>% apply(1, sum, na.rm=TRUE)
  res$Sum_Aggregated_Netzregelverbund <- res %>% select(starts_with("Sum_Aggregated_")) %>% apply(1, sum, na.rm=TRUE)
  res$Sum_Consumption_Netzregelverbund <- res %>% select(starts_with("Sum_Consumption_")) %>% apply(1, sum, na.rm=TRUE)


  if(getOption("logging")) logdebug(paste("getActualGeneration - DONE"))


  return(res)




}

# helper function for @seealso getActualGeneration
# returns the data.frame for a tso
getGenerationDataForTSO <- function(startDate, endDate, t){

  res <- data.frame()

  # Payload Parameters with example data:
  #
  # name	""
  # defaultValue	"false"
  # viewType	"TABLE"
  # areaType	"CTA"
  # atch	"false"
  # datepicker-day-offset-select-dv-date-from_input	"D"
  # dateTime.dateTime	"14.07.2017+00:00|CET|DAYTIMERANGE"
  # dateTime.endDateTime	"14.07.2017+00:00|CET|DAYTIMERANGE"
  # area.values	"CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2"
  # productionType.values	[20]
  # 0	"B01"
  # 1	"B02"
  # 2	"B03"
  # 3	"B04"
  # 4	"B05"
  # 5	"B06"
  # 6	"B07"
  # 7	"B08"
  # 8	"B09"
  # 9	"B10"
  # 10	"B11"
  # 11	"B12"
  # 12	"B13"
  # 13	"B14"
  # 14	"B20"
  # 15	"B15"
  # 16	"B16"
  # 17	"B17"
  # 18	"B18"
  # 19	"B19"
  # dateTime.timezone	"CET_CEST"
  # dateTime.timezone_input	"CET+(UTC+1)+/+CEST+(UTC+2)"
  # dataItem	"ALL"
  # timeRange	"YEAR"  // --> retrieves the data of the year of the given date (not passed days are filled with "-") or "DEFAULT" (--> data of the specified day)
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

  if(getOption("logging")) logdebug(paste("getActualGeneration - Get the data for TSO", t))

  # LOGIN with a pseudo account
  #
  if(getOption("logging")) logdebug(paste("getActualGeneration - Logging in ..."))

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

    if(getOption("logging")) logdebug(paste("getActualGeneration - Get the data for year", dateArray[i]))


    # Date Code: e.g. "12.07.2017+00:00|CET|DAYTIMERANGE"
    # "%d.%m.%Y" + "+00:00|CET|DAYTIMERANGE" --> this suffix stays the same
    #
    date = paste(format(as.Date(dateArray[i]), "%d.%m.%Y"), "+00:00|CET|DAYTIMERANGE", sep = "")

    # https://transparency.entsoe.eu/generation/r2/actualGenerationPerProductionType/export?name=&defaultValue=false&viewType=TABLE&areaType=CTA&atch=false&datepicker-day-offset-select-dv-date-from_input=D&dateTime.dateTime=14.07.2017+00:00|CET|DAYTIMERANGE&dateTime.endDateTime=14.07.2017+00:00|CET|DAYTIMERANGE&area.values=CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2&productionType.values=B01&productionType.values=B02&productionType.values=B03&productionType.values=B04&productionType.values=B05&productionType.values=B06&productionType.values=B07&productionType.values=B08&productionType.values=B09&productionType.values=B10&productionType.values=B11&productionType.values=B12&productionType.values=B13&productionType.values=B14&productionType.values=B20&productionType.values=B15&productionType.values=B16&productionType.values=B17&productionType.values=B18&productionType.values=B19&dateTime.timezone=CET_CEST&dateTime.timezone_input=CET+(UTC+1)+/+CEST+(UTC+2)&dataItem=ALL&timeRange=YEAR&exportType=CSV
    url = 'https://transparency.entsoe.eu/generation/r2/actualGenerationPerProductionType/export?';

    request = paste(
      'name=', "","&",
      'defaultValue=', "false","&",
      'viewType=', 'TABLE',"&",
      'areaType=', 'CTA',"&",
      'atch=', "false","&",
      'datepicker-day-offset-select-dv-date-from_input', "D", "&",
      'dateTime.dateTime=', date,"&",
      'dateTime.endDateTime=', date,"&",
      'area.values=', tso,"&",
      'productionType.values=', "B01","&",
      'productionType.values=', "B02","&",
      'productionType.values=', "B03","&",
      'productionType.values=', "B04","&",
      'productionType.values=', "B05","&",
      'productionType.values=', "B06","&",
      'productionType.values=', "B07","&",
      'productionType.values=', "B08","&",
      'productionType.values=', "B09","&",
      'productionType.values=', "B10","&",
      'productionType.values=', "B11","&",
      'productionType.values=', "B12","&",
      'productionType.values=', "B13","&",
      'productionType.values=', "B14","&",
      'productionType.values=', "B20","&",
      'productionType.values=', "B15","&",
      'productionType.values=', "B16","&",
      'productionType.values=', "B17","&",
      'productionType.values=', "B18","&",
      'productionType.values=', "B19","&",
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
                       na.strings  = c("-", "", "n/e"),
                       colClasses = c("character", "character"),
                       col.names = c("Area", "DateTime",
                                     paste("Biomass_Aggregated_", t, sep = ""),
                                     paste("Biomass_Consumption_", t, sep = ""),
                                     paste("FossilBrown_Aggregated_", t, sep = ""),
                                     paste("FossilBrown_Consumption_", t, sep = ""),
                                     paste("FossilCoalGas_Aggregated_", t, sep = ""),
                                     paste("FossilCoalGas_Consumption_", t, sep = ""),
                                     paste("FossilGas_Aggregated_", t, sep = ""),
                                     paste("FossilGas_Consumption_", t, sep = ""),
                                     paste("FossilHardCoal_Aggregated_", t, sep = ""),
                                     paste("FossilHardCoal_Consumption_", t, sep = ""),
                                     paste("FossilOil_Aggregated_", t, sep = ""),
                                     paste("FossilOil_Consumption_", t, sep = ""),
                                     paste("FossilOilShale_Aggregated_", t, sep = ""),
                                     paste("FossilOilShale_Consumption_", t, sep = ""),
                                     paste("FossilPeat_Aggregated_", t, sep = ""),
                                     paste("FossilPeat_Consumption_", t, sep = ""),
                                     paste("Geothermal_Aggregated_", t, sep = ""),
                                     paste("Geothermal_Consumption_", t, sep = ""),
                                     paste("HydroPumpedStorage_Aggregated_", t, sep = ""),
                                     paste("HydroPumpedStorage_Consumption_", t, sep = ""),
                                     paste("HydroRiverPoundage_Aggregated_", t, sep = ""),
                                     paste("HydroRiverPoundage_Consumption_", t, sep = ""),
                                     paste("WaterReservoir_Aggregated_", t, sep = ""),
                                     paste("WaterReservoir_Consumption_", t, sep = ""),
                                     paste("Marine_Aggregated_", t, sep = ""),
                                     paste("Marine_Consumption_", t, sep = ""),
                                     paste("Nuclear_Aggregated_", t, sep = ""),
                                     paste("Nuclear_Consumption_", t, sep = ""),
                                     paste("Other_Aggregated_", t, sep = ""),
                                     paste("Other_Consumption_", t, sep = ""),
                                     paste("OtherRenewable_Aggregated_", t, sep = ""),
                                     paste("OtherRenewable_Consumption_", t, sep = ""),
                                     paste("Solar_Aggregated_", t, sep = ""),
                                     paste("Solar_Consumption_", t, sep = ""),
                                     paste("Waste_Aggregated_", t, sep = ""),
                                     paste("Waste_Consumption_", t, sep = ""),
                                     paste("WindOffshore_Aggregated_", t, sep = ""),
                                     paste("WindOffshore_Consumption_", t, sep = ""),
                                     paste("WindOnshore_Aggregated_", t, sep = ""),
                                     paste("WindOnshore_Consumption_", t, sep = "")
                       ))

    res <- rbind(res, data)
  }

  # Get rid of Area Column
  res <- res[ , !(names(data) %in% c("Area"))]

  # Format the data.frame

  # convert characters to numeric values
  res[,2:ncol(res)] <- lapply(res[,2:ncol(res)], function(x) as.numeric(x))

  # convert DateTime character to POSIXct object
  l <- strsplit(res$DateTime, " - ")
  f <- function(x) x[1]

  res$DateTime <- as.POSIXct(unlist(lapply(l, f)), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")

  # # TODO --> subset data to the given time period by day
  # res <- res %>% filter(date(DateTime) >= date(as.POSIXct(startDate, tz = "Europe/Berlin")) & date(DateTime) <= date(as.POSIXct(endDate, tz = "Europe/Berlin")))
  res <- res %>% filter(DateTime >= as.POSIXct(paste(startDate, "00:00:00", sep=""), tz = "Europe/Berlin") & DateTime <= as.POSIXct(paste(endDate, "23:59:59", sep=""), tz = "Europe/Berlin"))

  # Add Actual Generation and Consumption sum for each TSO
  res[ , paste("Sum_Aggregated_", t, sep="")] <- res %>% select(contains("Aggregated")) %>% apply(1, sum, na.rm=TRUE)
  res[ , paste("Sum_Consumption_", t, sep="")] <- res %>% select(contains("Consumption")) %>% apply(1, sum, na.rm=TRUE)



  return(res)


}













#' @title getForecastGeneration
#'
#' @description This method retrieves the forecasted energy generation and consumption for the four german TSO and the Netzregelverbund (summation).
#' The resolution of the data is in one hour timesteps (CET (UTC+1) / CEST (UTC+2)) and the values are presented in MW.
#'
#' NOTE:
#'  - Always downloads for the whole year and then subsets the data to the given time period (3-4MB). This is done for each TSO
#'  - For every request a Login POST request is needed. For Login a pseudo account is used.
#'
#' @param startDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
#' @param endDate date format is YYYY-MM-DD Distinguishing of hours or minutes is not allowed. If you e.g. only need half of a day, please subset the data yourself.
#'
#' @return a data.frame object with the forecasted power generation and consumption of the four german TSO.
#'
#' @examples
#' loadData <- getForecastGeneration("2017-01-01", "2017-06-30")
#'
#' @export
#'
getForecastGeneration <- function(startDate, endDate) {
  library(logging)
  library(httr)
  library(XML)
  library(dplyr)
  library(lubridate)


  # TODO: check if time period exceeds one year --> seperate request into years

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getForecastGeneration", gsub(":", "", as.character(Sys.time())), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')



  res <- list()

  tsos <- c("50Hz", "Amprion", "TenneT", "TransnetBW")

  for(t in 1:length(tsos)) {

    # date format is YYYY-MM-DD
    d <- getGenerationForecastDataForTSO(startDate, endDate, tsos[t])

    # Only add DateTime column once at the beginning
    if(t > 1) {
      res <- c(res, list(d[,2:ncol(d)]))
    } else {
      res <- c(res, list(d))
    }

  }

  res <- as.data.frame(res)

  # Add the sumed loads for the Netzregelverbund
  res$Forecast_Generation_Netzregelverbund <- res %>% select(starts_with("Generation")) %>% apply(1, sum, na.rm=TRUE)
  res$Forecast_Consumption_Netzregelverbund <- res %>% select(starts_with("Consuption")) %>% apply(1, sum, na.rm=TRUE)


  if(getOption("logging")) logdebug(paste("getForecastGeneration - DONE"))


  return(res)




}

# helper function for @seealso getActualGeneration
# returns the data.frame for a tso
getGenerationForecastDataForTSO <- function(startDate, endDate, t){

  res <- data.frame()

  # Payload Parameters with example data:
  #
  # name	""
  # defaultValue	"false"
  # viewType	"TABLE"
  # areaType	"CTA"
  # atch	"false"
  # datepicker-day-offset-select-dv-date-from_input	"D"
  # dateTime.dateTime	"01.01.2017+00:00|CET|DAYTIMERANGE"
  # dateTime.endDateTime	"01.01.2017+00:00|CET|DAYTIMERANGE"
  # area.values	"CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2"
  # dateTime.timezone	"CET_CEST"
  # dateTime.timezone_input	"CET+(UTC+1)+/+CEST+(UTC+2)"
  # dataItem	"ALL"
  # timeRange	"YEAR" // --> retrieves the data of the year of the given date (not passed days are filled with "-") or "DEFAULT" (--> data of the specified day)
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

  if(getOption("logging")) logdebug(paste("getActualGeneration - Get the data for TSO", t))

  # LOGIN with a pseudo account
  #
  if(getOption("logging")) logdebug(paste("getActualGeneration - Logging in ..."))

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

    if(getOption("logging")) logdebug(paste("getActualGeneration - Get the data for year", dateArray[i]))


    # Date Code: e.g. "12.07.2017+00:00|CET|DAYTIMERANGE"
    # "%d.%m.%Y" + "+00:00|CET|DAYTIMERANGE" --> this suffix stays the same
    #
    date = paste(format(as.Date(dateArray[i]), "%d.%m.%Y"), "+00:00|CET|DAYTIMERANGE", sep = "")

    # https://transparency.entsoe.eu/generation/r2/dayAheadAggregatedGeneration/export?name=&defaultValue=false&viewType=TABLE&areaType=CTA&atch=false&datepicker-day-offset-select-dv-date-from_input=D&dateTime.dateTime=01.01.2017+00:00|CET|DAYTIMERANGE&dateTime.endDateTime=01.01.2017+00:00|CET|DAYTIMERANGE&area.values=CTY|10Y1001A1001A83F!CTA|10YDE-VE-------2&dateTime.timezone=CET_CEST&dateTime.timezone_input=CET+(UTC+1)+/+CEST+(UTC+2)&dataItem=ALL&timeRange=YEAR&exportType=CSV
    url = 'https://transparency.entsoe.eu/generation/r2/dayAheadAggregatedGeneration/export?';

    request = paste(
      'name=', "","&",
      'defaultValue=', "false","&",
      'viewType=', 'TABLE',"&",
      'areaType=', 'CTA',"&",
      'atch=', "false","&",
      'datepicker-day-offset-select-dv-date-from_input', "D", "&",
      'dateTime.dateTime=', date,"&",
      'dateTime.endDateTime=', date,"&",
      'area.values=', tso,"&",
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
                       na.strings  = c("-", "", "n/e"),
                       colClasses = c("character", "character"),
                       col.names = c("DateTime",
                                     paste("Forecast_Generation_", t, sep = ""),
                                     paste("Forecast_Consumption_", t, sep = "")
                       ))

    res <- rbind(res, data)
  }

  # Format the data.frame

  # convert characters to numeric values
  res[,2:ncol(res)] <- lapply(res[,2:ncol(res)], function(x) as.numeric(x))

  # convert DateTime character to POSIXct object
  l <- strsplit(res$DateTime, " - ")
  f <- function(x) x[1]

  res$DateTime <- as.POSIXct(unlist(lapply(l, f)), "%d.%m.%Y %H:%M", tz = "Europe/Berlin")

  # # TODO --> subset data to the given time period by day
  # res <- res %>% filter(date(DateTime) >= date(as.POSIXct(startDate, tz = "Europe/Berlin")) & date(DateTime) <= date(as.POSIXct(endDate, tz = "Europe/Berlin")))
  res <- res %>% filter(DateTime >= as.POSIXct(paste(startDate, "00:00:00", sep=""), tz = "Europe/Berlin") & DateTime <= as.POSIXct(paste(endDate, "23:59:59", sep=""), tz = "Europe/Berlin"))


  return(res)


}










