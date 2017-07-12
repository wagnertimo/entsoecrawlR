# entsoecrawlR: crawling energy system data of the ENTSO-E


## Goal

This R package provides functions to crawl energy system data of the European Network of Transmission System Operators (ENTSO-E) at https://transparency.entsoe.eu.
It is build as part of my master thesis to analyze the call durations of secondary control reserve (SCR) depending on the offered energy price. 
The aim is to forecast such call durations to improve existing energy management optimization algorithms which relay on assumptions and are not very accurate.
For further information please get in contact or follow my (blog)[https://wagnertimo.github.io].

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








