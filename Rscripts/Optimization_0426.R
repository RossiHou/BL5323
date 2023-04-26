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

BirdCheck = read_excel("/Users/zhangfengqi/Desktop/NUS/BL5323/FinalProject/BL5323/BirdData.xlsx")
colnames(BirdCheck) = c("ID", "SCINAME", "Category")
Birds_df = data.frame(Birds)
Birds_df = merge(x = Birds_df, y = BirdCheck, by = "SCINAME", all.x = T)
write.csv(Birds_df, "./Birds_df.csv")

Amphi =  vect("./DataMap/amphVNM/amphVNM.shp")
Birds = vect("./DataMap/birdsVNM/birdsVNM.shp")
Mammals = vect("./DataMap/mammalsVNM/mammalVNM.shp")

Birds$Category = Birds_df$Category

colnames(data.frame(Birds))
colnames(data.frame(Mammals))

Mammals$ca

Amphi = Amphi[(Amphi$code  == "CR") | (Amphi$code  == "EN") | (Amphi$code  == "VU"),]
Birds = Birds[(Birds$Category  == "CR") | (Birds$Category  == "EN") | (Birds$Category  == "VU"),]
Mammals = Mammals[(Mammals$category  == "CR") | (Mammals$category  == "EN") | (Mammals$category  == "VU"),]



Amphi = project(Amphi, crs(VNM))
Birds = project(Birds, crs(VNM))
Mammals = project(Mammals, crs(VNM))

Amphi$SciName = str_replace_all(Amphi$binomial, " ", "_")
Birds$SciName = str_replace_all(Birds$SCINAME, " ", "_")
Mammals$SciName = str_replace_all(Mammals$sci_name, " ", "_")

writeVector(Amphi, "./DataMap/amphVNM/amphVNM1.shp", overwrite = T)
writeVector(Birds, "./DataMap/birdsVNM/birdsVNM1.shp", overwrite = T)
writeVector(Mammals, "./DataMap/mammalsVNM/mammalVNM1.shp", overwrite = T)

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



######filtering the endagered animals


######filtering the endagered animals


Amphi =  vect("./DataMap/amphVNM/amphVNM1.shp")
Birds = vect("./DataMap/birdsVNM/birdsVNM1.shp")
Mammals = vect("./DataMap/mammalsVNM/mammalVNM1.shp")



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
PUs = cbind(PUs, data.frame(AmphiMatrix), data.frame(BirdsMatrix), data.frame(MammalsMatrix))



#6 + 222 + 4

#input the protected areas

PAs = vect("./DataMap/WDPA_WDOECM_Mar2023_Public_VNM_shp/WDPA_WDOECM_Mar2023_Public_VNM_shp_0/WDPA_WDOECM_Mar2023_Public_VNM_shp-polygons.shp")
PAs$val1 = 1

NewRast1 = rast(ext(VNM), nrow = 400, ncol = 200)
NewRast1 = project(NewRast1, crs(VNM))

PAs_Rast = rasterize(PAs, NewRast1, field = 'val1', background = 0)

PUs$PA = terra::extract(PAs_Rast, PUs, method = 'simple', buffer = NULL, fun = mean, na.rm = T)$val1

writeVector(PUs, "./DataMap/PUs/PUs.shp", overwrite = T)

#inpu the CO2 map
CO2_VNM = rast("./DataMap/CO2_VNM.tif")
CO2_VNM = project(CO2_VNM, crs(VNM))
PUs$CO2 = terra::extract(CO2_VNM, PUs, method = 'simple', buffer = NULL, fun = mean, na.rm = T)$predicted

deForRisk_VNM = rast("./DataMap/deForRisk_VNM.tif")
PUs$deForRisk = terra::extract(deForRisk_VNM, PUs, method = "simple", buffer = NULL, fun = mean, na.rm = T)$Global_transition_potential

AgriRent_VNM = rast("./DataMap/AgriRent_VNM.tif")
AgriRent_VNM = project(AgriRent_VNM, crs(VNM))

PUs$AgriRent = terra::extract(AgriRent_VNM, PUs, method = "simple", buffer = NULL, fun = mean, na.rm = T)[,2]


writeVector(PUs, "./DataMap/PUs/PUs.shp", overwrite = T)


#Convert the planning units shapefile into a sp object. Put the species in your planning unit into a stack raster. Make sure not to include columns for variables that are not species. Change the units of AR to millions.
RastList = list()
for (i in 7:228)
{
  R = rasterize(PUs, NewRast, field = names(PUs)[i], background = 0)
  RastList[[i]] = raster(R)
  print(i)
}

SpsStack = stack(RastList[7:228])
SpsStack1 = SpsStack[[order(Areas)]]

PUs$AgriRent = PUs$AgriRent



#optimization
######
#target: 0.30

#target: 0.25

#target: 0.20

#target: 0.15

#target: 0.10

######

PUs = vect("./DataMap/DataMap/PUs/PUs.shp")
plot(PUs, "deForRisk")
names(PUs)
PUs$PA

dataPU = as.data.frame(PUs)



PUsT = PUs
PUsT[is.na(PUsT$CO2),]$CO2 = 0


PUsT_Opt = as(PUsT, "Spatial")
PUsT_Opt = PUsT_Opt[,c("AgriRent", "PA","deForRisk")]
PUsT_Opt$CO2 = max(PUsT$CO2) - PUsT$CO2
PUsT_Opt$PAlog <- PUsT_Opt$PA>0.5
PUsT_Opt$deForRisk_5 <- PUsT_Opt$deForRisk<0.5

PUsT2 = PUsT_Opt[,1]
plot(PUsT2, col = PUsT2$AgriRent, legend)



PUs1 = as.data.frame(PUs)
str(PUs1[,220:232])

PUs2 = as.data.frame(PUsT_Opt)
View(PUs2)

P1 = problem(PUsT_Opt, features = SpsStack1[[103:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(10, data = "CO2") 
   #%>% #trade-off")
  #add_boundary_penalties(penalty = 100, edge_factor = 0.5)



#%>%
  #add_locked_in_constraints("deForRisk_5")

#code for carbon

S1 = solve(P1)

plot(st_as_sf(S1[, "solution_1"]), pal = c("grey90", "darkgreen"))

sum1 = eval_target_coverage_summary(P1,S1[,"solution_1"])



P1_1 = problem(PUsT_Opt, features = SpsStack1[[103:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(10, data = "CO2") %>%
  add_locked_in_constraints("deForRisk_5") 

S1_1 = solve(P1_1)

P1_2 = problem(PUsT_Opt, features = SpsStack1[[103:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)%>%
  add_locked_in_constraints("deForRisk_5")  %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(10, data = "CO2") 

S1_2 = solve(P1_2)

S1data = as.data.frame(S1)
View(S1data)

S1_test = S1data[S1data$PAlog == 1,]
View(S1_test)

S1_1data = as.data.frame(S1_1)
View(S1_1data)

combine = cbind(S1data,S1_1data)
View(combine)

S1_2data = as.data.frame(S1_2)
View(S1_2data)

combine2 = cbind(S1_1data,S1_2data)
View(combine2)


saveRDS(P1,file = "./Results/P1.rds")
saveRDS(S1,file = "./Results/S1.rds")
saveRDS(P1_1,file = "./Results/P1_1.rds")
saveRDS(S1_1,file = "./Results/S1_1.rds")
saveRDS(P1_2,file = "./Results/P1_2.rds")
saveRDS(S1_2,file = "./Results/S1_2.rds")


P2 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(10, data = "CO2") 

S2 = solve(P2)

plot(st_as_sf(S2[, "solution_1"]), pal = c("grey90", "darkgreen"))

saveRDS(P2,file = "./Results/P2.rds")
saveRDS(S2,file = "./Results/S2.rds")


P3 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.15) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(50, data = "CO2") 

S3 = solve(P3)

plot(st_as_sf(S3[, "solution_1"]), pal = c("grey90", "darkgreen"))

saveRDS(P3,file = "./Results/P3.rds")
saveRDS(S3,file = "./Results/S3.rds")


P3_1 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.15) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_locked_in_constraints("deForRisk_5") %>%
  add_linear_penalties(50, data = "CO2") 

S3_1 = solve(P3_1)
plot(st_as_sf(S3_1[, "solution_1"]), pal = c("grey90", "green"))

saveRDS(P3_1,file = "./Results/P3_1.rds")
saveRDS(S3_1,file = "./Results/S3_1.rds")




P4 = problem(PUsT_Opt, 
             features = SpsStack1[[47:222]], 
             cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(50, data = "CO2") 

S4 = solve(P4)

plot(st_as_sf(S4[, "solution_1"]), pal = c("grey90", "darkgreen"))

saveRDS(P4,file = "./Results/P4.rds")
saveRDS(S4,file = "./Results/S4.rds")

sum4 = eval_target_coverage_summary(P4,S4[,"solution_1"])
View(sum4)

P4 = readRDS("./Results/P4.rds")
S4 = readRDS("./Results/S4.rds")
dataS4 = as.data.frame(S4)



P4_1 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_locked_in_constraints("deForRisk_5") %>%
  add_linear_penalties(50, data = "CO2") 

S4_1 = solve(P4_1)
plot(st_as_sf(S4_1[, "solution_1"]), pal = c("grey90", "green"))

sum4_1 = eval_target_coverage_summary(P4_1,S4_1[,"solution_1"])
View(sum4_1)

saveRDS(P4_1,file = "./Results/P4_1.rds")
saveRDS(S4_1,file = "./Results/S4_1.rds")



#Changing PA to deForestLoss
P4_2 = problem(PUsT_Opt, 
             features = SpsStack1[[47:222]], 
             cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(50, data = "CO2") 

S4_2 = solve(P4_2)
plot(st_as_sf(S4_2[, "solution_1"]), pal = c("grey90", "green"))


P5 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_penalties(100, data = "CO2") 

S5 = solve(P5)

plot(st_as_sf(S5[, "solution_1"]), pal = c("grey90", "darkgreen"))

saveRDS(P5,file = "./Results/P5.rds")
saveRDS(S5,file = "./Results/S5.rds")

S5 = readRDS("./Results/S5.rds")
dataS5 = as.data.frame(S5)


P5_1 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_locked_in_constraints("deForRisk_5") %>%
  add_linear_penalties(100, data = "CO2") 

S5_1 = solve(P5_1)
plot(st_as_sf(S5_1[, "solution_1"]), pal = c("grey90", "green"))

saveRDS(P5_1,file = "./Results/P5_1.rds")
saveRDS(S5_1,file = "./Results/S5_1.rds")

PUsT_Opt$deForRisk_8 <- PUsT_Opt$deForRisk<0.8

P5_2 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_locked_in_constraints("deForRisk_8") %>%
  add_linear_penalties(100, data = "CO2") 

S5_2 = solve(P5_2)
plot(st_as_sf(S5_2[, "solution_1"]), pal = c("grey90", "green"))

saveRDS(P5_2,file = "./Results/P5_2.rds")
saveRDS(S5_2,file = "./Results/S5_2.rds")


PUsT_Opt$deForRisk_2 <- PUsT_Opt$deForRisk<0.2

P5_3 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_locked_in_constraints("deForRisk_2") %>%
  add_linear_penalties(100, data = "CO2") 

S5_3 = solve(P5_3)
plot(st_as_sf(S5_3[, "solution_1"]), pal = c("grey90", "green"))

saveRDS(P5_3,file = "./Results/P5_3.rds")
saveRDS(S5_3,file = "./Results/S5_3.rds")



Amphi_area = expanse(Amphi, unit = "km")
Birds_area = expanse(Birds, unit = "km")
Mammals_area = expanse(Mammals, unit = "km")

plot(VNM)
plot(Amphi[13,], col = "green", add = T)
plot(VNM, add = T)


P4 = readRDS("./Results/P4.rds")
S1 = readRDS("./Results/S1.rds")
S4 = readRDS("./Results/S4.rds")
S5 = readRDS("./Results/S5.rds")



#Setting up different penalty values of not protecting CO2 emission
P6 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog")

S6 = solve(P6)

saveRDS(P6,file = "./Results/P6.rds")
saveRDS(S6,file = "./Results/S6.rds")

P6 = readRDS("./Results/P6.rds")


penaltiesVec = c(5, 15, 30, 50, 75, 100)

trade_off_results <- lapply(penaltiesVec, function(x){
  s <-
    P6 %>%
    add_linear_penalties(x, data = "CO2") %>%
    solve()
  ## store data frame with solution
  s <- data.frame(s = s$solution_1)
  names(s) <- paste0("penalty_", x)
  s
})
trade_offs <- cbind(PUsT_Opt, do.call(bind_cols, trade_off_results))

par(mfrow=c(2,3))
for(i in penaltiesVec){
  plot(st_as_sf(trade_offs[, paste("penalty_",i,sep="")]), main = paste("penalty_",i),
       pal = c("grey90", "darkgreen"), key.pos = NULL, reset = FALSE)
}

saveRDS(trade_offs_result,file = "./Results/trade_offs_result.rds")
saveRDS(trade_offs,file = "./Results/trade_offs.rds")

trade_offs = readRDS("./Results/trade_offs.rds")
dataTrade = as.data.frame(trade_offs)


#Setting up different penalty values of not protecting CO2 emission
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




P6_2 <- P6_1 %>%
  add_contiguity_constraints()
S6_2 = solve(P6_2)




P6_3 <- P6_1 %>%
  add_boundary_penalties(penalty = 50, edge_factor = 0.5)
S6_3 = solve(P6_3)




P6_4 = problem(PUsT_Opt, features = SpsStack1[[54:222]], cost_column = "AgriRent") %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog")


penaltiesVec1 = c(0.1, 2000)

trade_off_results1 <- lapply(penaltiesVec1, function(x){
  s <-
    P6_4 %>%
    add_linear_penalties(x, data = "CO2") %>%
    solve()
  ## store data frame with solution
  s <- data.frame(s = s$solution_1)
  names(s) <- paste0("penalty_", x)
  s
})



#Error: CO2 value is NA!
spsCovVec = c()
co2vec2 = c()
for(i in 1:6){
  P8 <- P6 %>% 
    add_linear_penalties(penaltiesVec[i], data = "CO2")
  S8 <- solve(P8)
  tc_s <- eval_target_coverage_summary(P8, S8[, "solution_1"])
  co2vec2[i] = sum(data.frame(S8[, "solution_1"])*(PUsT_Opt$CO2))
  spsCovVec[i] = sum(tc_s$met)
}




#Error:cannot find solution
P9 = problem(PUsT_Opt, features = SpsStack1[[47:222]], cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0) %>%
  add_locked_in_constraints("PAlog") %>%
  add_linear_constraints(threshold = 0.1, sense = "<=", data = "deForRisk")

S9 = solve(P9)

saveRDS(P9,file = "./Results/P9.rds")
saveRDS(S9,file = "./Results/S9.rds")




# Try to run this on 04/12
penaltiesVec1 = c(5, 20, 50, 100, 200, 300)

trade_off_results1 <- lapply(penaltiesVec1, function(x){
  s <-
    P6 %>%
    add_linear_penalties(x, data = "CO2") %>%
    solve()
  ## store data frame with solution
  s <- data.frame(s = s$solution_1)
  names(s) <- paste0("penalty_", x)
  s
})
trade_offs_1 <- cbind(PUsT_Opt, do.call(bind_cols, trade_off_results1))

par(mfrow=c(2,4))
for(i in penaltiesVec){
  plot(st_as_sf(trade_offs[, paste("penalty_",i,sep="")]), lwd = 0.01, main = paste("penalty_",i),
       pal = c("grey90", "darkgreen"), key.pos = NULL, reset = FALSE)
}



saveRDS(trade_off_results1,file = "./Results/trade_off_results1.rds")
saveRDS(trade_offs_1,file = "./Results/trade_offs_1.rds")





#Contiguity constraints
P7 <- P4 %>%
  add_contiguity_constraints()

S7 <- solve(P7)
#No solution

#Carbon


#Irreplaceability
irrep <- eval_ferrier_importance(P4, S4["solution_1"])
print(irrep)
saveRDS(irrep,file = "./Results/irreplaceability.rds")

irrep$plot_total <- irrep$total
irrep$plot_total[S4$solution_1 < 0.5] <- NA_real_

#Too small:
cuts=c(0,0.1,0.2,0.5,0.75,1,5) #set breaks
pal <- colorRampPalette(c("pink","purple"))
plot(st_as_sf(irrep[, "plot_total"]), main = "Overall importance",breaks=cuts, pal = pal(6))

#Finally:
cuts=c(0,0.0025,0.005,0.0075,0.01,0.015,0.02) #set breaks
pal <- colorRampPalette(c("pink","purple"))
plot(st_as_sf(irrep[, "plot_total"]), main = "Overall importance",breaks=cuts, pal = pal(6))


#04/26:
cuts=c(0,0.0025,0.005,0.01,0.015,0.02) #set breaks
pal <- colorRampPalette(c("pink","purple"))
plot(st_as_sf(irrep[, "plot_total"]), lwd = 0.01, main = "Overall importance",breaks=cuts, pal = pal(5))


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

i = as.data.frame(irrep)
View(i)




P10 = problem(PUsT_Opt, 
             features = SpsStack1[[47:222]], 
             cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)

S10 <- solve(P10)

saveRDS(P10,file = "./Results/P10.rds")
saveRDS(S10,file = "./Results/S10.rds")

plot(st_as_sf(S10[, "solution_1"]), pal = c("grey90", "darkgreen"))



P11 = problem(PUsT_Opt, 
              features = SpsStack1[[54:222]], 
              cost_column = "AgriRent") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)

S11 <- solve(P11)

saveRDS(P11,file = "./Results/P_onlytarget0.3.rds")
saveRDS(S11,file = "./Results/S_onlytarget0.3.rds")

plot(st_as_sf(S11[, "solution_1"]), pal = c("grey90", "darkgreen"))




S12 = readRDS("./Results/S12.rds")
vect_S12 = vect(S12)
writeVector(vect_S12,"./Results/vect_S12.shp",overwrite = T)


#%>%
 # add_locked_in_constraints("PAlog") %>%
  #add_linear_penalties(50, data = "CO2") 


#Penalty for compactness
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



test = vect(trade_offs)
writeVector(test, "./Results/Trade_off_new0426.shp",overwrite = T)



#to spatVector
test$solution_1 <- ifelse(is.na(test$penalty_50), 0, test$penalty_50)
test$solution_2 <- ifelse(test$solution_1 == 1, "Considering Protection", "Not Considering")

test = project(test, crs(VNM))

###filter out
PAs = vect("./DataMap/DataMap/WDPA_WDOECM_Mar2023_Public_VNM_shp/WDPA_WDOECM_Mar2023_Public_VNM_shp_2/WDPA_WDOECM_Mar2023_Public_VNM_shp-polygons.shp")
PAs$val1 = "Protected Area"
test_nonPA = mask(test, PAs, inverse = T)
writeVector(test_nonPA,"./Results/Result_nonPA0426.shp")

plot(test, lwd = 0.1, col = "grey90")
plot(test_nonPA, "solution_2", col = c("darkgreen", "grey90"), lwd = 0.1, main = "Target: 30% (non-PA)")

test_target_nonPA = test_nonPA[test_nonPA$solution_1 == 1,]
plot(test_target_nonPA)
test_target_nonPA$CO2 = extract(CO2_VNM, test_target_nonPA, na.rm = T, mean, ID = F)[,1]
test_target_nonPA$AR = extract(AgriRent_VNM, test_target_nonPA, na.rm = T, mean, ID = F)[,1]
test_target_nonPA$CarbonPrice = test_target_nonPA$AR/test_target_nonPA$CO2
plot(test, lwd = 0.1, col = "grey90")
plot(test_target_nonPA, "CarbonPrice", lwd = 0.1, main = "Minimum Carbon Price")


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


