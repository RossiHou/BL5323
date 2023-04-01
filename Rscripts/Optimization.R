###library
library(rgdal)
library(sp)
library(sf)
library(terra)
library(raster)

library(gurobi)
library(dplyr)



setwd("/Users/zhangfengqi/Desktop/NUS/BL5323/FinalProject/BL5323")

Country = vect("/Users/zhangfengqi/Desktop/Crop Failure/WorldCountries/WorldCountriesWithISO.shp")
VNM = Country[Country$ISO3 == "VNM",]

NewRast = rast(ext = ext(VNM), nrow= 200, ncol = 100)
NewRast =  project(NewRast, crs(VNM))

NewVect = as.polygons(NewRast)
PUs = terra::intersect(NewVect, VNM)
plot(PUs)















