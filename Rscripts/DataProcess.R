###BL5323 Final Project###

#Please remember to change the working directory when running on your laptop.

###library
library(rgdal)
library(sp)
library(sf)
library(terra)

#set the working directory
setwd("/Users/zhangfengqi/Desktop/NUS/BL5323/FinalProject/BL5323")

#input the country boundary file
Country = vect("./DataMap//WorldCountries/WorldCountriesWithISO.shp")

VNM = Country[Country$ISO3 == "VNM",]

#input files about
deForRisk = rast("./DataMap/Global_transition_potential.tif")
deForRisk = project(deForRisk, crs(Country))

AbGBio = rast("./DataMap/AbGBio.tif")
AbGBio = project(AbGBio, crs(Country))
BGBio = rast("./DataMap/BGBio.tif")
BGBio = project(BGBio, crs(Country))

KBA_VNM = vect("./DataMap/Vietnam_KBA/Vietnam_KBA.shp")
KBA_VNM = project(KBA_VNM, crs(Country))

deForRisk_VNM = crop(deForRisk, VNM, mask = T)
AbGBio_VNM = crop(AbGBio, VNM, mask = T)
BGBio_VNM = crop(BGBio, VNM, mask = T)

plot(deForRisk_VNM, main = "Deforest Risk (VNM)")
plot(AbGBio_VNM, main = "Above Ground Biomass (VNM)")
plot(BGBio_VNM, main = "Below Ground Biomass (VNM)")

plot(VNM, main = "KBA (VNM)")
plot(KBA_VNM, add = T, col = 'green')


writeRaster(deForRisk_VNM, "./DataMap/deForRisk_VNM.tif")
writeRaster(AbGBio_VNM, "./DataMap/AbGBio_VNM.tif")
writeRaster(BGBio_VNM, "./DataMap/BGBio_VNM.tif")

###Comparison between two versions of aboveground biomass map
AbGBio_VNM = rast("./DataMap/AbGBio_VNM.tif")

AbGBio_Avi = rast("./DataMap/Avitabile_AGB_Map/Avitabile_AGB_Map.tif")
AbGBio_Avi = project(AbGBio_Avi, crs(Country))

AbGBio_Avi_VNM = crop(AbGBio_Avi, VNM, mask = T)

par(mfrow = c(1,2))
plot(AbGBio_VNM, main = "2019 Prediction", range = c(0, 500))
plot(AbGBio_Avi_VNM, main = "Avitabile", range = c(0, 500))


###Soil carbon map
SoC = rast("./DataMap/GSOCmap1.6.1.tif")
SoC = project(SoC, crs(Country))
SoC_VNM = crop(SoC, VNM, mask = T)
plot(SoC_VNM, main = "Soil Carbon (VNM)")











