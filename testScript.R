#'
#' This script is only for testing the R package's functionalities
#'

library(entsoecrawlR)

setLogging(TRUE)


sdate = "2017-05-29"
edate = "2017-05-31"


d <- getLoadDayAheadVsActual(sdate, edate)
LoadData.2015.2017 = getLoadDayAheadVsActual("2015-01-01", "2017-06-30")

head(d)


sdate = "2015-04-01"
edate = "2017-05-31"

data <- getWindSolarDayAheadGeneration(sdate, edate)
WindSolarForecastGeneration.2015.2017 = getWindSolarDayAheadGeneration("2015-01-01", "2017-06-30")

write.csv(WindSolarForecastGeneration.2015.2017, "WindSolarForecastGeneration.2015.2017.csv")


str(data)


sdate = "2017-01-01"
edate = "2017-01-01"

data2 <- getActualGeneration(sdate, edate)
ActualGeneration.2015.2017 = getActualGeneration("2015-01-01", "2017-06-30")





sdate = "2017-01-01"
edate = "2017-01-01"

data3 <- getForecastGeneration(sdate, edate)
ForecastGeneration.2015.2017 = getForecastGeneration("2015-01-01", "2017-06-30")


write.csv(ForecastGeneration.2015.2017, "ForecastGeneration.2015.2017.csv")

head(data3)







