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
BGBio_VNM = rast("./DataMap/BGBio_VNM.tif")

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

###resolution: 30s

#resample
AbGBio_VNM = resample(AbGBio_VNM, SoC_VNM, method = 'bilinear')
BGBio_VNM = resample(BGBio_VNM, SoC_VNM, method = 'bilinear')
#the unit of Biomass maps is Mg/ha (i.e. tons/ha)

writeRaster(AbGBio_VNM, "./DataMap/AbGBio_VNM_30s.tif")
writeRaster(BGBio_VNM, "./DataMap/BGBio_VNM_30s.tif")

par(mfrow = c(1,3))
plot(AbGBio_VNM, main = "AbG (VNM)")
plot(BGBio_VNM, main = "BG (VNM)")
plot(SoC_VNM, main = "SoC (VNM)")

#Sum of Biomass
Bio_VNM = AbGBio_VNM + BGBio_VNM
CO2_VNM = Bio_VNM*0.49*(44/12) + SoC_VNM*0.25*(44/12) + 2.1*(44/12)
plot(CO2_VNM, main = "Potential CO2 Emissions (VNM)")
 
PA_VNM = vect("./DataMap/WDPA_WDOECM_Mar2023_Public_VNM_shp/WDPA_WDOECM_Mar2023_Public_VNM_shp_2/WDPA_WDOECM_Mar2023_Public_VNM_shp-polygons.shp")
PA_VNM = project(PA_VNM, crs(Country))

plot(VNM, main = "WDPA (VNM)")
plot(PA_VNM, add = T, col = 'green')

deForRisk_VNM = rast("./DataMap/deForRisk_VNM.tif")
deForRisk_VNM = project(deForRisk_VNM, crs(Country))
deForRisk_VNM = resample(deForRisk_VNM, CO2_VNM, method = 'bilinear')

CO2_VNM_deFor_3 = CO2_VNM*(deForRisk_VNM > 0.3)
CO2_VNM_deFor_4 = CO2_VNM*(deForRisk_VNM > 0.4)
CO2_VNM_deFor_5 = CO2_VNM*(deForRisk_VNM > 0.5)

par(mfrow = c(1, 3))
plot(CO2_VNM_deFor_3, main = "Additional Carbon (30%)")
plot(PA_VNM, col = 'grey', alpha = 0.5, add = T)
plot(CO2_VNM_deFor_4, main = "Additional Carbon (40%)")
plot(PA_VNM, col = 'grey', alpha = 0.5, add = T)
plot(CO2_VNM_deFor_5, main = "Additional Carbon (50%)")
plot(PA_VNM, col = 'grey', alpha = 0.5, add = T)

###the cost
AgriRent = rast("./DataMap/Rent2016Com.tif")
AgriRent = project(AgriRent, crs(Country))
AgriRent_VNM = crop(AgriRent, VNM, mask = T)

plot(AgriRent_VNM, main = "Agricultural Rents (VNM, USD)")
#revenue (euro dollar)
CCredit = CO2_VNM_deFor_5*100*1.066
plot(CCredit, main = "Carbon Credit per hectare (USD)")



