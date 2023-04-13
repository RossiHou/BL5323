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



setwd("C:/BL5323")


Country = vect("./DataMap/DataMap/WorldCountries/WorldCountriesWithISO.shp")
VNM = Country[Country$ISO3 == "VNM",]

NewRast = rast(ext = ext(VNM), nrow= 200, ncol = 100)
NewRast =  project(NewRast, crs(VNM))



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


PUs = vect("./DataMap/DataMap/PUs/PUs.shp")


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

S2 = readRDS("./Results/S2.rds")


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

S3 = readRDS("./Results/S3.rds")


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
saveRDS(sum4,file = "./Results/sum4.rds")
write.csv(sum4,file = "./Results/sum4.csv")
data_sum4 = read.csv("./Results/sum4.csv", head = T)
View(sum4)


P4 = readRDS("./Results/P4.rds")
S4 = readRDS("./Results/S4.rds")
dataS4 = as.data.frame(S4)

plot(st_as_sf(S2[, "solution_1"]), pal = c("grey90", "darkgreen"))
plot(st_as_sf(S3[, "solution_1"]), pal = c("grey90", "darkgreen"))
plot(st_as_sf(S4[, "solution_1"]), pal = c("grey90", "darkgreen"))


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

par(mfrow=c(2,3))
for(i in penaltiesVec1){
  plot(st_as_sf(trade_offs_1[, paste("penalty_",i,sep="")]), main = paste("penalty_",i),
       pal = c("grey90", "red"), key.pos = NULL, reset = FALSE)
}

saveRDS(trade_off_results1,file = "./Results/trade_off_results1.rds")
saveRDS(trade_offs_1,file = "./Results/trade_offs_1.rds")


trade_offs_1 = readRDS("./Results/trade_offs_1.rds")


#Contiguity constraints
P7 <- P4 %>%
  add_contiguity_constraints()

S7 <- solve(P7)
#No solution


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
