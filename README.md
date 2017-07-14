# entsoecrawlR: crawling energy system data of the ENTSO-E


## Goal

This R package provides functions to crawl energy system data of the European Network of Transmission System Operators (ENTSO-E) at https://transparency.entsoe.eu.
It is build as part of my master thesis to analyze the call durations of secondary control reserve (SCR) depending on the offered energy price. 
The aim is to forecast such call durations to improve existing energy management optimization algorithms which relay on assumptions and are not very accurate.
For further information please get in contact or follow my blog (https://wagnertimo.github.io).

## Get Started

### Installing

When installing this package you should at least use the *R version 3.3.0 (2016-05-03)*. For the library dependecies see the section below. You can easily install this R package by using the `install_github()` function from the `devtools` package:

```r
library(devtools)
install_github("wagnertimo/entsoecrawlR")
```
### Library dependencies

Before using this R package, please check that you have installed the following R packages. Normally, with the installation of the package those dependencies will also be installed. If not, you have to do it manually.

- `httr`
- `xml2`
- `XML`
- `zoo`
- `lubridate`
- `timeDate`
- `dplyr`
- `tidyr`
- `magrittr`
- `data.table`
- `logging`


### Usage


**Total Load - Day Ahead / Actual**: The function `getLoadDayAheadVsActual()` is implemented to retrieve the actual and forecasted total loads of each TSO and their common load values of the Netzregelverbund (source: https://transparency.entsoe.eu/load-domain/r2/totalLoadR2/show). The values are in MW in a resolution of 15 minutes where the timestamp represents the start of the quarter hour. The data is returned by days. If you only want minutes within the day, pplease subset the data yourself. The code snippet below gives an example. It is important not forgetting to activate the library and set logging off or on.

```{r}
# Activate the library
library(entsoecrawlR)

# Set logging. Creates also a loggin file in the workspace. Forgetting to set a value will break the codes.
setLogging(TRUE)

# Retrieve the Load data for 2015-04-01 to 2017-05-31
loadData <- getLoadDayAheadVsActual("2015-04-01", "2017-05-31")

head(loadData)
# Output:
#              DateTime ForecastLoad_50Hz ActualLoad_50Hz ForecastLoad_Amprion ActualLoad_Amprion ForecastLoad_TenneT ActualLoad_TenneT
# 1 2015-04-01 00:00:00              7519            8832                19777              20149               16736             17371
# 2 2015-04-01 00:15:00              7390            8232                19485              20112               16756             16841
# 3 2015-04-01 00:30:00              7301            7865                19247              19753               16492             16373
# 4 2015-04-01 00:45:00              7161            7840                19019              19706               16320             16322
# 5 2015-04-01 01:00:00              7100            7754                18870              19418               16146             15947
# 6 2015-04-01 01:15:00              7056            8110                18739              19230               15963             15808
#   ForecastLoad_TransnetBW ActualLoad_TransnetBW ForecastLoad_Netzregelverbund Actuaload_Netzregelverbund
# 1                    6589                  6712                         50621                      53064
# 2                    6463                  6457                         50094                      51642
# 3                    6261                  6344                         49301                      50335
# 4                    6197                  6167                         48697                      50035
# 5                    6071                  6135                         48187                      49254
# 6                    5994                  5974                         47752                      49122

```




**Generation Forecasts - Day Ahead for Wind and Solar**: The function `getWindSolarDayAheadGeneration()` is implemented to retrieve the forecasted Wind (Onshore, Offshore) and Solar generation of each TSO and their common generation energy of the Netzregelverbund (source: https://transparency.entsoe.eu/generation/r2/dayAheadGenerationForecastWindAndSolar/show). The values are in MW in a resolution of 15 minutes where the timestamp represents the start of the quarter hour. The data is returned by days. If you only want minutes within the day, pplease subset the data yourself. The code snippet below gives an example. It is important not forgetting to activate the library and set logging off or on.

```{r}
# Activate the library
library(entsoecrawlR)

# Set logging. Creates also a loggin file in the workspace. Forgetting to set a value will break the codes.
setLogging(TRUE)

# Retrieve the forecasted Wind and Solar generation data for 2015-04-01 to 2017-05-31
windSolarData <- getWindSolarDayAheadGeneration("2015-04-01", "2017-05-31")

str(windSolarData)
# Output:
# 'data.frame':	96 obs. of  21 variables:
#  $ DateTime                                         : POSIXct, format: "2017-01-01 00:00:00" ...
#  $ Sum_Generation_Forecast50Hz                      : num  5253 5221 5190 5164 5139 ...
#  $ Solar_Generation_Forecast_50Hz                   : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ WindOffshore_Generation_Forecast_50Hz            : num  331 331 331 331 331 331 331 331 331 331 ...
#  $ WindOnshore_Generation_Forecast_50Hz             : num  4922 4890 4859 4833 4808 ...
#  $ Sum_Generation_ForecastAmprion                   : num  1366 1369 1375 1380 1383 ...
#  $ Solar_Generation_Forecast_Amprion                : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ WindOffshore_Generation_Forecast_Amprion         : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ WindOnshore_Generation_Forecast_Amprion          : num  1366 1369 1375 1380 1383 ...
#  $ Sum_Generation_ForecastTenneT                    : num  9645 9621 9603 9580 9117 ...
#  $ Solar_Generation_Forecast_TenneT                 : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ WindOffshore_Generation_Forecast_TenneT          : num  3009 3007 3008 3008 3039 ...
#  $ WindOnshore_Generation_Forecast_TenneT           : num  6636 6614 6595 6572 6078 ...
#  $ Sum_Generation_ForecastTransnetBW                : num  42 43 44 45 46 48 50 52 55 59 ...
#  $ Solar_Generation_Forecast_TransnetBW             : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ WindOffshore_Generation_Forecast_TransnetBW      : num  NA NA NA NA NA NA NA NA NA NA ...
#  $ WindOnshore_Generation_Forecast_TransnetBW       : num  42 43 44 45 46 48 50 52 55 59 ...
#  $ WindOffshore_Generation_Forecast_Netzregelverbund: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ WindOnshore_Generation_Forecast_Netzregelverbund : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ Solar_Generation_Forecast_Netzregelverbund       : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ Sum_Generation_Forecast_Netzregelverbund         : num  16306 16254 16212 16169 15685 ...

```




**Actual Generation per Production Type**: The function `getActualGeneration()` is implemented to retrieve the actual power generation per product type of each TSO and their common generation energy of the Netzregelverbund (source: https://transparency.entsoe.eu/generation/r2/actualGenerationPerProductionType/show). 

The product types are Biomass, Fossil Brown coal/Lignite, Fossil Coal-derived gas, Fossil Gas, Fossil Hard coal, Fossil Oil, Fossil Oil shale, Fossil Peat, Geothermal, Hydro Pumped Storage, Hydro Run-of-river and poundage, Hydro Water Reservoir, Marine, Nuclear, Other, Other renewable, Solar, Waste, Wind Offshore, Wind Onshore. For each product type the aggregated generation and consumption is returned, as well as the total generation over all prodution types for each TSO and the Netzregelverbund.

The values are in MW in a resolution of 15 minutes where the timestamp represents the start of the quarter hour. The data is returned by days. If you only want minutes within the day, pplease subset the data yourself. The code snippet below gives an example. It is important not forgetting to activate the library and set logging off or on.

```{r}
# Activate the library
library(entsoecrawlR)

# Set logging. Creates also a loggin file in the workspace. Forgetting to set a value will break the codes.
setLogging(TRUE)

# Retrieve the actual generation and consumption for each TSO and the Netzregelverbund for 2015-04-01 to 2017-05-31. 208 Variables!
actualGen <- getActualGeneration("2015-04-01", "2017-05-31")

```








## Notes

> Data for the energy loads (day-ahead forecast and actual loads) are only retrievable since 2015-01-01.







