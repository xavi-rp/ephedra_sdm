
###########################################################################
########  Ephedra fragilis subsp. fragilis in the Balearic Is. ############
###########################################################################


# ephedra_balears.R

# Created on: under construction

# Contact: Xavier Rotllan-Puig (xavi.rotllan.puig@gmail.com)

# Description: The aim of this script is to make the SDM for Ephedra fragilis subsp. fragilis on 
# the Balearic Islands. We use presence data from the Bioatles, and free climatic, altitude and Land Use data
# as predictors.
# We use MaxEnt to build the model


# ------------------------------------------


getwd()
setwd("~/Dropbox/ephedra_balears/data")
#load("~/Documents/ephedra_balears/data/ephedra_ws.RData")

#### packages ####
library(sp)
library(rgdal)
library(raster)
library(graphics)
library(rgeos)


#### Importing and mapping presences on the Balearics ####

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

    writeRaster(predictor, filename=paste0("/Users/xavi/Dropbox/ephedra_balears/data/variables_cneor/etrs89/", nm), format="HFA", overwrite=TRUE)
  } 
}

fun_proj(lst_predictors)


#### Creating a contour of the Balearics (polygon) and a mask (raster) ####
bio2 <- raster(paste0("/Users/xavi/Dropbox/ephedra_balears/data/variables_cneor/etrs89/", "bio2.img"))
plot(bio2)

mask <- reclassify(bio2, c(0, 100, 1)) # reclassify to 1 and NA
plot(mask, legend=FALSE)
summary(mask)
writeRaster(mask, filename=paste0("/Users/xavi/Dropbox/ephedra_balears/data/variables_cneor/etrs89/", "mask_balearix"), format="HFA", overwrite=TRUE)

balearix_ETRS89 <- rasterToPolygons(mask, dissolve=TRUE)
plot(balearix_ETRS89)
str(balearix_ETRS89)
writeOGR(balearix_ETRS89, layer = "balearix_ETRS89", driver = "ESRI Shapefile", dsn = "/Users/xavi/Dropbox/ephedra_balears/data/variables_cneor/etrs89/balearix_ETRS89", verbose = TRUE, overwrite_layer = TRUE)

rm(bio2)



#### Importing climatic data from WorldClim (19 bioclimatic variables in a RasterStack) ####
# WorldClim 1.4; date of download: 13/04/2017; 30 arc-seconds (~1 km);
# latitude / longitude coordinate reference system (not projected) and the datum is WGS84
# temperature data are in °C * 10; precipitation data is mm (millimeter)

bioclim_15 <- getData("worldclim", var="bio", res=0.5, lon=-8, lat=35,
                      path = "/Users/xavi/Dropbox/ephedra_balears/data/worldclim05")  # importing tile 15
bioclim_16 <- getData('worldclim', var='bio', res=0.5, lon=3, lat=39,
                      path = "/Users/xavi/Dropbox/ephedra_balears/data/worldclim05")  # importing tile 16

bioclim_15_WGS84 <- bioclim_15 
proj4string(bioclim_15_WGS84) <- CRS("+init=epsg:4326")

bioclim_16_WGS84 <- bioclim_16
proj4string(bioclim_16_WGS84) <- CRS("+init=epsg:4326")

plot(bioclim_15[[1]])
plot(bioclim_16[[1]], add=TRUE)


# Projecting to ETRS89 31N

bioclim_15_ETRS89 <- bioclim_15_WGS84
bioclim_15_ETRS89 <- projectRaster(bioclim_15_WGS84, crs="+init=EPSG:25831") #to ETRS89

bioclim_16_ETRS89 <- bioclim_16_WGS84
bioclim_16_ETRS89 <- projectRaster(bioclim_16_WGS84, crs="+init=EPSG:25831") #to ETRS89


# Clipping the variables with a polygon (Balearic Is.)

crop(bioclim_16_ETRS89, balearix_ETRS89, filename="bioc_16_bal")
bioc_16_bal <- raster("bioc_16_bal")
plot(bioc_16_bal)


#### Importing a Digital Elevation Model (DEM) ####

# The data is downloaded from http://srtm.csi.cgiar.org/
# Citation: Jarvis A., H.I. Reuter, A.  Nelson, E. Guevara, 2008, Hole-filled  seamless SRTM
# data V4, International  Centre for Tropical  Agriculture (CIAT), available  from
# http://srtm.csi.cgiar.org.
# Download date: 9/02/2016
# Original resolution: approx. 90m (3arc-sec)
# Version: 4.1
# Coord Sys: geographic coordinate system - WGS84 datum.
# 
# http://www.ngdc.noaa.gov/mgg/topo/gltiles.html    this is another option (100m res)


#elev_36_04 <- raster("elevation/srtm_36_04/srtm_36_04.asc") # NW Iberian Peninsula
#elev_36_05 <- raster("elevation/srtm_36_05/srtm_36_05.asc") # SW Ib. P.
elev_37_04 <- raster("elevation/srtm_37_04/srtm_37_04.asc")  # NE Ib. P.
elev_37_05 <- raster("elevation/srtm_37_05/srtm_37_05.asc")  # SE + Balearics

elev <- merge(elev_37_04, elev_37_05, tolerance=0.05)
plot(elev)

elev_WGS84 <- elev 
proj4string(elev_WGS84) <- CRS("+init=epsg:4326") # giving coord system, etc
elev_ETRS89 <- projectRaster(elev_WGS84, crs="+init=EPSG:25831") #to ETRS89

crop(elev_ETRS89, balearix_ETRS89, filename="elev_bal", overwrite=TRUE)
elev_bal <- raster("elev_bal")
plot(elev_bal)


#



save.image("ephedra_ws.RData")

