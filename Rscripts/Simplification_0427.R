###library
library(rgdal)
library(sp)
library(sf)
library(terra)
library(raster)

library(gurobi)
library(dplyr)
library(prioritizr)

library(stringr)
library(crayon)
library(rgdal)


setwd("C:/BL5323")


Country = vect("./DataMap/DataMap/WorldCountries/WorldCountriesWithISO.shp")
VNM = Country[Country$ISO3 == "VNM",]

NewRast = rast(ext = ext(VNM), nrow= 200, ncol = 100)
NewRast =  project(NewRast, crs(VNM))

NewVect = as.polygons(NewRast)
PUs = terra::intersect(NewVect, VNM)
plot(PUs)

#import the species distribution
### set IUCN Red List API 
# set your api key as an environmental variable so you do not upload to public domains
# now call the variable that you set

Amphi = vect("./DataMap/DataMap/amphVNM/amphVNM1.shp")
Birds = vect("./DataMap/DataMap/birdsVNM/birdsVNM1.shp")
Mammals = vect("./DataMap/DataMap/mammalsVNM/mammalVNM1.shp")


Amphi$val1 = 1
Birds$val1 = 1
Mammals$val1 = 1


Amphi1 = Amphi[,c("val1")]
Birds1 = Birds[,c("val1")]
Mammals1 = Mammals[,c("val1")]

Animals = rbind(Amphi1, Birds1, Mammals1)
Areas = expanse(Animals, unit = "km")
Areas1 = Areas[order(Areas)]
Areas_VNM = expanse(VNM, unit = "km")
Areas_prop = Areas1/Areas_VNM



#6 + 222 + 4

#input the protected areas

PAs = vect("./DataMap/WDPA_WDOECM_Mar2023_Public_VNM_shp/WDPA_WDOECM_Mar2023_Public_VNM_shp_0/WDPA_WDOECM_Mar2023_Public_VNM_shp-polygons.shp")
PAs$val1 = 1

NewRast1 = rast(ext(VNM), nrow = 400, ncol = 200)
NewRast1 = project(NewRast1, crs(VNM))

#input the maps
CO2_VNM = rast("./DataMap/CO2_VNM.tif")
CO2_VNM = project(CO2_VNM, crs(VNM))

deForRisk_VNM = rast("./DataMap/deForRisk_VNM.tif")
PUs$deForRisk = terra::extract(deForRisk_VNM, PUs, method = "simple", buffer = NULL, fun = mean, na.rm = T)$Global_transition_potential

AgriRent_VNM = rast("./DataMap/AgriRent_VNM.tif")
AgriRent_VNM = project(AgriRent_VNM, crs(VNM))


#Convert the planning units shapefile into a sp object. Put the species in your planning unit into a stack raster. Make sure not to include columns for variables that are not species. Change the units of AR to millions.
PUs = vect("./DataMap/DataMap/PUs/PUs.shp")

RastList = list()
for (i in 7:228)
{
  R = rasterize(PUs, NewRast, field = names(PUs)[i], background = 0)
  RastList[[i]] = raster(R)
  print(i)
}

SpsStack = stack(RastList[7:228])
SpsStack1 = SpsStack[[order(Areas)]]



#optimization
######
#target: 0.30

#target: 0.25

#target: 0.20

#target: 0.15

#target: 0.10

######



PUsT = PUs
PUsT[is.na(PUsT$CO2),]$CO2 = 0


PUsT_Opt = as(PUsT, "Spatial")
PUsT_Opt = PUsT_Opt[,c("AgriRent", "PA","deForRisk")]
PUsT_Opt$CO2 = max(PUsT$CO2) - PUsT$CO2
PUsT_Opt$PAlog <- PUsT_Opt$PA>0.5
PUsT_Opt$deForRisk_5 <- PUsT_Opt$deForRisk<0.5



#The whole selected area with target 0.3 and constraint of PA
P6_1 = problem(PUsT_Opt, features = SpsStack1[[54:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog")

S6_1 = solve(P6_1)

saveRDS(P6_1,file = "./Results/P6_1.rds")
saveRDS(S6_1,file = "./Results/S6_1.rds")

P6_1 = readRDS("./Results/P6_1.rds")



#Setting up different penalty values of not protecting CO2 emission
penaltiesVec = c(0.1, 1, 5, 50, 100, 300, 1000, 2000)

trade_off_results <- lapply(penaltiesVec, function(x){
  s <-
    P6_1 %>%
    add_linear_penalties(x, data = "CO2") %>%
    solve()
  ## store data frame with solution
  s <- data.frame(s = s$solution_1)
  names(s) <- paste0("penalty_", x)
  s
})
saveRDS(trade_off_results,file = "./Results/trade_off_result0426.rds")
trade_offs <- cbind(PUsT_Opt, do.call(bind_cols, trade_off_results))
saveRDS(trade_offs,file = "./Results/trade_offs0426.rds")

test = vect(trade_offs)
writeVector(test, "./Results/Trade_off_new0426.shp",overwrite = T)

par(mfrow=c(2,4))
for(i in penaltiesVec){
  plot(st_as_sf(trade_offs[, paste("penalty_",i,sep="")]), main = paste("penalty_",i),
       pal = c("grey90", "darkgreen"), key.pos = NULL, reset = FALSE)
}




#Constraint of contiguity: cannot find a solution!
P6_2 <- P6_1 %>%
  add_contiguity_constraints()
S6_2 = solve(P6_2)


#Constraint of compactness: penalty is too high!
P6_3 <- P6_1 %>%
  add_boundary_penalties(penalty = 50, edge_factor = 0.5)
S6_3 = solve(P6_3)



#Irreplaceability
P_new = problem(PUsT_Opt, features = SpsStack1[[54:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(50, data = "CO2") 

irrep <- eval_ferrier_importance(P_new, trade_offs["penalty_50"])
print(irrep)
saveRDS(irrep,file = "./Results/irreplaceability0426.rds")

irrep$plot_total <- irrep$total
irrep$plot_total[trade_offs$penalty_50 < 0.5] <- NA_real_


#Plot irreplaceability:
cuts=c(0,0.0025,0.005,0.01,0.015,0.02) #set breaks
pal <- colorRampPalette(c("pink","purple"))
plot(st_as_sf(irrep[, "plot_total"]), lwd = 0.01, main = "Overall importance",breaks=cuts, pal = pal(5))




#Penalty for compactness: run for 6 days!
P13 <- problem(PUsT_Opt, features = SpsStack1[[54:222]],
              cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog")%>%
  add_boundary_penalties(penalty = 10, edge_factor = 0.5)

S13 <- solve(P13)
plot(st_as_sf(S13[, "solution_1"]), pal = c("grey90", "darkgreen"))






#Excluding PAs and low loss risk areas
#to spatVector
test$solution_1 <- ifelse(is.na(test$penalty_50), 0, test$penalty_50)
test$solution_2 <- ifelse(test$solution_1 == 1, "Considering Protection", "Not Considering")

test = project(test, crs(VNM))

###filter out PAs
PAs = vect("./DataMap/DataMap/WDPA_WDOECM_Mar2023_Public_VNM_shp/WDPA_WDOECM_Mar2023_Public_VNM_shp_2/WDPA_WDOECM_Mar2023_Public_VNM_shp-polygons.shp")
PAs$val1 = "Protected Area"
test_nonPA = mask(test, PAs, inverse = T)
writeVector(test_nonPA,"./Results/Result_nonPA0426.shp")

test_nonPA = vect("./Results/Result_nonPA0426.shp")

plot(test, lwd = 0.1, col = "grey90")
plot(test_nonPA, "solution_2", col = c("darkgreen", "grey90"), lwd = 0.1, main = "Target: 30% (non-PA)")


#calculating minimum carbon price
test_target_nonPA = test_nonPA[test_nonPA$solution_1 == 1,]
plot(test_target_nonPA)
test_target_nonPA$CO2 = extract(CO2_VNM, test_target_nonPA, na.rm = T, mean, ID = F)[,1]
test_target_nonPA$AR = extract(AgriRent_VNM, test_target_nonPA, na.rm = T, mean, ID = F)[,1]
test_target_nonPA$CarbonPrice = test_target_nonPA$AR/test_target_nonPA$CO2
plot(test, lwd = 0.1, col = "grey90")
plot(test_target_nonPA, "CarbonPrice", lwd = 0.1, main = "Minimum Carbon Price")

writeVector(test_target_nonPA,"./Results/Result_targetnonPA0426.shp",overwrite = T)


###filter out low loss risk areas
deForRisk = rast("./DataMap/deForRisk_VNM.tif")
deForRisk = project(deForRisk, crs(VNM))

deForRisk_5 = 1 * (deForRisk < 0.5)
plot(deForRisk_5)
deForRisk_5 = ifel(deForRisk_5 == 0, NA, deForRisk_5)

deForRisk_3 = 1 * (deForRisk < 0.3)
plot(deForRisk_3)
deForRisk_3 = ifel(deForRisk_3 == 0, NA, deForRisk_3)

deForRisk_5_vect = as.polygons(deForRisk_5)
deForRisk_3_vect = as.polygons(deForRisk_3)

test_deFor_50 = mask(test_nonPA, deForRisk_5_vect, inverse = T)
writeVector(test_deFor_50,"./Results/Result_deF50_0426.shp")
plot(test, lwd = 0.1, col = "grey90")
plot(test_deFor_50, "solution_2", col = c("darkgreen", "grey90"), lwd = 0.1, main = "Target: 30% (Deforestation Risk > 50%)", add = T)

test_deFor_30 = mask(test_nonPA, deForRisk_3_vect, inverse = T)
plot(test, lwd = 0.1, col = "grey90")
plot(test_deFor_30, "solution_2", col = c("darkgreen", "grey90"), lwd = 0.1, main = "Target: 30% (Deforestation Risk > 30%)", add = T)
writeVector(test_deFor_30,"./Results/Result_deF30_0426.shp")


