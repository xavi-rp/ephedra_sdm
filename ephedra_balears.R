
###########################################################################
########  Ephedra fragilis subsp. fragilis in the Balearic Is. ############
###########################################################################


# ephedra_balears.R

# Created on: under construction

# Contact: Xavier Rotllan-Puig (xavi.rotllan.puig@gmail.com)

# Description: The aim of this script is to made the SDM for Ephedra fragilis subsp. fragilis on 
# the Balearic Islands. We use presence data from the Bioatles, and free climatic, altitude and Land Use data
# as predictors.
# We use MaxEnt to create the models


# ------------------------------------------


getwd()
setwd("~/Dropbox/ephedra_balears/data")
#load("~/Documents/ephedra_balears/data/ephedra_ws.RData")


#### Importing and mapping presences on the Balearics ####
library(sp)
library(rgdal)
library(raster)
library(graphics)

ephedra_bioatles <- read.table("dades_bioatles.txt", sep="\t", header = TRUE)
head(ephedra_bioatles)
summary(ephedra_bioatles)

coordinates(ephedra_bioatles) <- c("x", "y")  # set spatial coordinates
plot(ephedra_bioatles)
proj4string(ephedra_bioatles) <- CRS("+init=EPSG:23031")  # define projection: European Datum 1950 (31N)
summary(ephedra_bioatles)
str(ephedra_bioatles)

CRS.new <- CRS("+init=EPSG:25831") #new Coordinate Reference System to project data: ETRS89 31N

ephedra_ETRS89 <- spTransform(ephedra_bioatles, CRS.new)  #projecting 
str(ephedra_ETRS89)
summary(ephedra_ETRS89)
write.csv(ephedra_ETRS89, file = "ephedra_ETRS89.csv")



#### Predictors ####

lst_predictors <- list.files(path="~/Dropbox/ephedra_balears/data/variables_cneor", pattern=".asc$", full.names = TRUE)
lst_predictors

fun_proj <- function(x){ # to project all the rasters of the folder
  
  for (i in 1:length(x)){
    predictor <- raster(x[i])
    proj4string(predictor) <- CRS("+init=epsg:4326")
    #predictor_etrs89 <- projectRaster(predictor, crs="+init=EPSG:25831") #to ETRS89
    proj4string(predictor) <- proj4string(ephedra_ETRS89)
    
    nm <- sub(".*?cneor/(.*?)\\.asc.*", "\\1", x[i] )

    writeRaster(predictor, filename=paste0("/Users/xavi/Dropbox/ephedra_balears/data/variables_cneor/etrs89/", nm), format="ascii", crs="+init=EPSG:25831", overwrite=TRUE)
  } 
}


fun_proj(lst_predictors)


plot(bio19)
str(bio19)
contour(bio19, add=TRUE)

#### Importing climatic data from WorldClim (19 bioclimatic variables in a RasterStack)

bioclim_15 <- getData('worldclim', var='bio', res=0.5, lon=-8, lat=35)  # to import tile 15
bioclim_16 <- getData('worldclim', var='bio', res=0.5, lon=3, lat=39)  # to import tile 16




### To import a Digital Elevation Model (DEM)

# The data is downloaded from http://srtm.csi.cgiar.org/
# Citation: Jarvis A., H.I. Reuter, A.  Nelson, E. Guevara, 2008, Hole-filled  seamless SRTM
# data V4, International  Centre for Tropical  Agriculture (CIAT), available  from
# http://srtm.csi.cgiar.org.
# Download date: 9/02/2016
# Original resolution: approx. 90m (3arc-sec)
# 
# http://www.ngdc.noaa.gov/mgg/topo/gltiles.html    this is another option (100m res)


elev_36_04 <- raster("~/Documents/ephedra_balears/data/elevation/srtm_36_04/srtm_36_04.asc")
elev_36_05 <- raster("~/Documents/ephedra_balears/data/elevation/srtm_36_05/srtm_36_05.asc")
elev_37_04 <- raster("~/Documents/ephedra_balears/data/elevation/srtm_37_04/srtm_37_04.asc")
elev_37_05 <- raster("~/Documents/ephedra_balears/data/elevation/srtm_37_05/srtm_37_05.asc")


#



save.image("~/Documents/ephedra_balears/data/ephedra_ws.RData")

