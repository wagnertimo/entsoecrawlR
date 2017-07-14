#'
#' This script is only for testing the R package's functionalities
#'

library(entsoecrawlR)

setLogging(TRUE)


sdate = "2015-04-01"
edate = "2017-05-31"


d <- getLoadDayAheadVsActual(sdate, edate)

head(d)


