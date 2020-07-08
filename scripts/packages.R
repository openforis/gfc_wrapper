########################################
# include all the needed packages here #
options(stringsAsFactors = F)
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
## load packages

## Packages to download GFC data
packages(devtools)
#install_github('azvoleff/gfcanalysis')
library(gfcanalysis)

## Packages for geospatial data handling
packages(raster)
packages(rgeos)
packages(rgdal)
packages(Formula)
packages(gdalUtils)

## Packages for Shiny 
packages(shiny)
packages(shinydashboard)
packages(shinyFiles)
packages(snow)
packages(htmltools)
# packages(shinycssloaders)

#packages(RCurl)

## Packages for data table handling
packages(xtable)
packages(DT)
packages(dismo)
packages(stringr)
packages(plyr)

## Packages for graphics and interactive maps
packages(ggplot2)
packages(leaflet)
packages(leaflet.extras)
packages(RColorBrewer)
packages(data.table)
