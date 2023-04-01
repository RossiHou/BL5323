###library
library(rgdal)
library(sp)
library(sf)
library(terra)
library(raster)

library(gurobi)
library(dplyr)
library(prioritizr)



setwd("/Users/zhangfengqi/Desktop/NUS/BL5323/FinalProject/BL5323")

Country = vect("/Users/zhangfengqi/Desktop/Crop Failure/WorldCountries/WorldCountriesWithISO.shp")
VNM = Country[Country$ISO3 == "VNM",]

NewRast = rast(ext = ext(VNM), nrow= 200, ncol = 100)
NewRast =  project(NewRast, crs(VNM))

NewVect = as.polygons(NewRast)
PUs = terra::intersect(NewVect, VNM)
plot(PUs)

#import the species distribution
Amphi =  vect("./DataMap/amphVNM/amphVNM.shp")
Birds = vect("./DataMap/birdsVNM/birdsVNM.shp")
Mammals = vect("./DataMap/mammalsVNM/mammalVNM.shp")

Amphi = project(Amphi, crs(VNM))
Birds = project(Birds, crs(VNM))
Mammals = project(Mammals, crs(VNM))

Amphi$SciName = str_replace_all(Amphi$binomial, " ", "_")
Birds$SciName = str_replace_all(Birds$SCINAME, " ", "_")
Mammals$SciName = str_replace_all(Mammals$sci_name, " ", "_")

writeVector(Amphi, "./DataMap/amphVNM/amphVNM.shp", overwrite = T)
writeVector(Birds, "./DataMap/birdsVNM/birdsVNM.shp", overwrite = T)
writeVector(Mammals, "./DataMap/mammalsVNM/mammalVNM.shp", overwrite = T)

Amphi$val1 = 1
Birds$val1 = 1
Mammals$val1 = 1


#Extract  the range of each species
AmphiMatrix = matrix(nrow = nrow(PUs), ncol = nrow(Amphi))

for (i in 1:nrow(Amphi))
{
  Sps_i = Amphi[i,]
  
  Sps_i_rast = rasterize(Sps_i, NewRast, field = "val1", background = 0)
  
  AmphiMatrix[,i] = terra::extract(Sps_i_rast, PUs, method = 'simple', buffer = NULL, fun = mean, na.rm = T)$val1
  print(paste0("Amphi", i))
}

BirdsMatrix = matrix(nrow = nrow(PUs), ncol = nrow(Birds))

for (i in 1:nrow(Birds))
{
  Sps_i = Birds[i,]
  
  Sps_i_rast = rasterize(Sps_i, NewRast, field = "val1", background = 0)
  
  BirdsMatrix[,i] = terra::extract(Sps_i_rast, PUs, method = 'simple', buffer = NULL, fun = mean, na.rm = T)$val1
  print(paste0("Birds", i))
}

MammalsMatrix = matrix(nrow = nrow(PUs), ncol = nrow(Mammals))

for (i in 1:nrow(Mammals))
{
  Sps_i = Mammals[i,]
  
  Sps_i_rast = rasterize(Sps_i, NewRast, field = "val1", background = 0)
  
  MammalsMatrix[,i] = terra::extract(Sps_i_rast, PUs, method = 'simple', buffer = NULL, fun = mean, na.rm = T)$val1
  print(paste0("Mammals", i))
}



#name the matrix
colnames(AmphiMatrix) = Amphi$SciName
colnames(BirdsMatrix) = Birds$SciName
colnames(MammalsMatrix) = Mammals$SciName
PUs$ = cbind(PUs, data.frame(AmphiMatrix), data.frame(BirdsMatrix), data.frame(MammalsMatrix))

#6 + 1641

#input the protected areas

PAs = vect("./DataMap/WDPA_WDOECM_Mar2023_Public_VNM_shp/WDPA_WDOECM_Mar2023_Public_VNM_shp_0/WDPA_WDOECM_Mar2023_Public_VNM_shp-polygons.shp")
PAs$val1 = 1

NewRast1 = rast(ext(VNM), nrow = 400, ncol = 200)
NewRast1 = project(NewRast1, crs(VNM))

PAs_Rast = rasterize(PAs, NewRast1, field = 'val1', background = 0)

PUs$PA = extract(PAs_Rast, PUs, method = 'simple', buffer = NULL, fun = mean, na.rm = T)$val1

writeVector(PUs, "./DataMap/PUs/PUs.shp", overwrite = T)

#inpu the CO2 map
CO2_VNM = rast("./DataMap/CO2_VNM.tif")
CO2_VNM = project(CO2_VNM, crs(VNM))
PUs$CO2 = extract(CO2_VNM, PUs, method = 'simple', buffer = NULL, fun = mean, na.rm = T)$predicted

deForRisk_VNM = rast("./DataMap/deForRisk_VNM.tif")
PUs$deForRisk = extract(deForRisk_VNM, PUs, method = "simple", buffer = NULL, fun = mean, na.rm = T)$Global_transition_potential

PUs$AgriRent = extract(AgriRent_VNM, PUs, method = "simple", buffer = NULL, fun = mean, na.rm = T)[,2]

writeVector(PUs, "./DataMap/PUs/PUs.shp", overwrite = T)


#Convert the planning units shapefile into a sp object. Put the species in your planning unit into a stack raster. Make sure not to include columns for variables that are not species. Change the units of AR to millions.
RastList = list()
for (i in 7:1647)
{
  R = rasterize(PUs, NewRast, field = names(PUs)[i], background = 0)
  RastList[[i]] = raster(R)
  print(i)
}

SpsStack = stack(RastList[7:1647])

PUs$AgriRent = PUs$AgriRent/1000000


#optimization
######
#target: 0.30

#target: 0.25

#target: 0.20

#target: 0.15

#target: 0.10

######



PUs$PAlog = PUs$PA > 0.5
PUs$deForRisk_5 = PUs$deForRisk < 0.5

PUs_Opt = as(PUs, "Spatial")

P1 = problem(PUs_Opt, features = SpsStack, cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") #%>%
  #add_locked_in_constraints("deForRisk_5")

S1 = solve(P1)






















