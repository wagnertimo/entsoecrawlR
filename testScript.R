#'
#' This script is only for testing the R package's functionalities
#'

library(entsoecrawlR)

setLogging(TRUE)


sdate = "2015-04-01"
edate = "2017-05-31"


d <- getLoadDayAheadVsActual(sdate, edate)
LoadData.2015.2017 = getLoadDayAheadVsActual("2015-01-01", "2017-06-30")

head(d)


sdate = "2015-04-01"
edate = "2017-05-31"

data <- getWindSolarDayAheadGeneration(sdate, edate)
WindSolarGeneration.2015.2017 = getWindSolarDayAheadGeneration("2015-01-01", "2017-06-30")




str(data)


sdate = "2017-01-01"
edate = "2017-01-01"

data2 <- getActualGeneration(sdate, edate)
ActualGeneration.2015.2017 = getActualGeneration("2015-01-01", "2017-06-30")





sdate = "2017-01-01"
edate = "2017-01-01"

data3 <- getForecastGeneration(sdate, edate)
ForecastGeneration.2015.2017 = getForecastGeneration("2015-01-01", "2017-06-30")

head(data3)







