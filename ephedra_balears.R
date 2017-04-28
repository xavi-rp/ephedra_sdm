
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
#load("~/Dropbox/ephedra_balears/data/ephedra_ws.RData")

#### packages ####
library(sp)
library(rgdal)
library(raster)
library(graphics)
library(rgeos)
library(dismo)
library(devtools)


#### Importing and mapping presences on the Balearics ####
# Data downloaded from Bioatles (http://bioatles.caib.es/serproesfront/VisorServlet), on 01/07/2015

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
write.csv(ephedra_ETRS89, file = "ephedra_ETRS89.csv", row.names = FALSE)



#### Predictors (from Cneorum/Daphne study) ####

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

pdf("Ephedra_bioatles.pdf")
plot(balearix_ETRS89)
points(ephedra_ETRS89, cex=0.01, col=2)
dev.off()

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

crop(bioclim_16_ETRS89, balearix_ETRS89, filename="bioc_16_bal", overwrite = TRUE)
bioc_16_bal <- brick("bioc_16_bal")
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

crop(elev_ETRS89, balearix_ETRS89, filename="elev_bal", overwrite=TRUE) # croping to Balearics
elev_bal <- raster("elev_bal")

resample(elev_bal, bioc_16_bal, method="bilinear", filename="elev_bal_agg", overwrite=TRUE)  # upscaling ( to lower resolution: larger cells)
elev_bal <- raster("elev_bal_agg")
elev_bal@data@names <- "elev_bal"


plot(elev_bal)


#### Distance to the coast ####

mask_inv <- mask
mask_inv[is.na(mask_inv)] <- 2
mask_inv[mask_inv == 1] <- NA

dist_coast <- distance(mask_inv)
resample(dist_coast, bioc_16_bal, method="bilinear", filename="dist_coast_agg", overwrite=TRUE)  # upscaling ( to lower resolution: larger cells)
dist_coast <- raster("dist_coast_agg")
dist_coast@data@names <- "dist_coast"
plot(dist_coast)
plot(balearix_ETRS89, add=TRUE)

#


#### Selecting non-correlated variables ####

library(virtualspecies)

# Creating a stack with the variables

stack_rstrs <- stack()  # creating a RasterStack (for predictors)
stack_rstrs <- stack(stack_rstrs, bioc_16_bal, elev_bal, dist_coast)    #filling in the stack

vables_NoC <- removeCollinearity(stack_rstrs,
                                 multicollinearity.cutoff = 0.85,
                                 select.variables = TRUE,  # if TRUE, randomly select one variable of the group. If FALSE, returns a list with the groups
                                 sample.points = FALSE,
                                 plot = TRUE)
vables_NoC

dev.copy(pdf, "vars_collinearity.pdf")
dev.off()   # saving plot


# A rasterStack with no-correlated variables

predictors <- subset(stack_rstrs, vables_NoC)
predictors@layers



#### Modelling parameters ####
# ENMEval package
# Evaluating which is the optimal combination of regularization and features parameters 
# in order to improve model performance while limiting overfitting.
# The Akaike Information Criterion corrected for small samples sizes reflects both model
# goodness-of-fit and complexity and it is independent of the partitioning method because 
# it is computed with the full set of presences.
# The model with the lowest AICc value (i.e. Delta_AICc = 0) is considered the best model out 
# of the current suite of models.
# Big AUC_diff, equal to overfit models.
#

install_github("bobmuscarella/ENMeval@master")   #to install the last version of ENMeval from the GitHub repository of Bob Muscarella (https://github.com/bobmuscarella/ENMeval)
library(ENMeval)


# reading presences
ephedra <- read.csv(file = "ephedra_ETRS89.csv", header = TRUE)
head(ephedra)

#prevalences table ??

# running ENMEval: https://github.com/bobmuscarella/ENMeval       #source code of the library
# MaxEnt: Version 3.3.3e, November 2010 (https://github.com/mrmaxent/Maxent/tree/master/ArchivedReleases/3.3.3e)

enm_eval <- ENMevaluate(ephedra, predictors, bg.coords = NULL, occ.grp = NULL,
                        bg.grp = NULL, RMvalues = seq(0.5, 4, 0.5),
                        fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                        categoricals = NULL, n.bg = 10000, method = "randomkfold",
                        overlap = FALSE, aggregation.factor = c(2, 2),
                        kfolds = 3, bin.output = FALSE, clamp = TRUE,
                        rasterPreds = TRUE, parallel = FALSE, numCores = NULL,
                        progbar = TRUE, updateProgress = FALSE)

enm_eval@results # data.frame with comparison results
names(enm_eval@results)
best_comb <- enm_eval@results[enm_eval@results$delta.AICc == 0, ]  # best result (rm, fc)

features <- as.vector(best_comb$features)
rm <- best_comb$rm

best_model <- enm_eval@models[[18]]   # best model
response(best_model)   # response curves of the best model

best_model_preds <- enm_eval@predictions[[18]]  # predictions of the best model ("row" format)
plot(best_model_preds)
plot(balearix_ETRS89, add=TRUE)
plot(ephedra_ETRS89, add=TRUE, cex=0.01, col=2)

best_model_preds_log <- predict(best_model, predictors, args = c("outputformat=logistic")) # predictions of the best model ("logistic" format)
plot(best_model_preds_log)

# saving to a PDF file, with presences (red dots)
pdf("sdm_ephedra_logistic.pdf")   
plot(best_model_preds_log)
dev.off()   # saving plot

#






save.image("ephedra_ws.RData")

