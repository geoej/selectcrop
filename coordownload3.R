setwd("D:/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx")
library(rjson)
library(sp)
#library(rgdal)
library(dplyr)
library(maptools)
# > 15535 + 48 + 2280 + 4076 +159 
# [1] 22098


#15535
pearlmil <- read.csv("./data/accession/genesys-accessions-filtered _pearl_millet.csv") #15535
pearlmil = pearlmil[which(pearlmil$SPECIES == "glaucum"),] #14674
coordinates(pearlmil) <- ~ lon + lat
proj4string(pearlmil) <- CRS("+init=epsg:4326")

#48
# breadf <- read.csv("./data/accession/genesys-accessions-filtered_breadfruit.csv") #48
# breadf = breadf[which(breadf$SPECIES == "altilis"),] #28
# coordinates(breadf) <- ~ lon + lat
# proj4string(breadf) <- CRS("+init=epsg:4326")

#2280
# fingermil <- read.csv("./data/accession/genesys-accessions-filtered_finger_millet.csv")
# coordinates(fingermil) <- ~ lon + lat
# proj4string(fingermil) <- CRS("+init=epsg:4326")

#4076
pigeon <- read.csv("./data/accession/genesys-accessions-filtered_pigeon_pea.csv")
pigeon = pigeon[which(pigeon$SPECIES == "cajan"),] #3828
coordinates(pigeon) <- ~ lon + lat
proj4string(pigeon) <- CRS("+init=epsg:4326")

#2
# taro <- read.csv("./data/accession/genesys-accessions-filtered_taro.csv")
# coordinates(taro) <- ~ lon + lat
# proj4string(taro) <- CRS("+init=epsg:4326")

#159
# yam <- read.csv("./data/accession/genesys-accessions-filtered_yam.csv")
# coordinates(yam) <- ~ lon + lat
# proj4string(yam) <- CRS("+init=epsg:4326")

#world_rands <- readOGR("./data/boundary/", "world_rands")
# world_rands <- readShapePoints("./data/boundary/world_rands.shp")
# proj4string(world_rands) <- CRS("+init=epsg:4326")

## code to extract soil data based on coordinates -----------------------------
for (j in c("pearlmil","pigeon")){  #"breadf", "taro","breadf",,"world_rands""fingermil","yam"
  y <- get(j)
  out <- NULL
  for (i in 1:nrow(y)) {
    uri = paste("https://rest.soilgrids.org/query?lon=",y@coords[i,1],"&lat=",y@coords[i,2],"&attributes=BLDFIE,CLYPPT,SLTPPT,SNDPPT,PHIHOX,BDRICM,CECSOL,ORCDRC,CRFVOL", sep="")
    try( download.file(uri, "ret.txt", method="wininet"), silent = TRUE )
    try(ret <- rjson::fromJSON(file="ret.txt"),silent = T)
    if(!class(.Last.value)[1]=="try-error" & !length(ret$properties)==0){
      try(out[[i]] <- data.frame(ret$properties),silent = T)
      try(out[[i]]$lon <- y@coords[i,1],silent = T)
      try(out[[i]]$lat <- y@coords[i,2],silent = T)
      
    } else {
      try(out[[i]] <- data.frame(lon=y@coords[i,1], lat=y@coords[i,2]),silent = T)
      return(i)
    }
  }
  ## bind all elements together:
  out <- plyr::rbind.fill(out)
  #out1 <- cbind(plyr::rbind.fill(out), as.data.frame(y))
  write.csv(out, paste("ISRICextract_",j,".csv",sep=""))
  #write.csv(out1, paste("ISRICextract_genesys",j,".csv",sep=""))
}

## code to extract climate data based on the coordinates--------------------

# get the raster of the area for temperature and rain fall
# require(raster)
# raindata = stack()
# tempdata = stack()
# for (i in 1:12) {
#   temprast = raster(paste("/data/sci_data/worldClimData/tmean",i,"_global.tif",sep=""));
#   rainrast = raster(paste("/data/sci_data/worldClimData/prec",i,"_global.tif",sep=""));
#   #temprast = mask(crop(temprast, extent(boundary)), boundary);
#   #rainrast = mask(crop(rainrast, extent(boundary)), boundary);
#   crs(temprast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
#   crs(rainrast) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
#   #  assign (paste("tmean", i, district, country, sep=""), temprast) # in case of write out
#   #  assign (paste("rain", i, district, country, sep=""), rainrast) # in case of write out
#   raindata = addLayer(raindata, rainrast)
#   tempdata = addLayer(tempdata, temprast)
# }
# names(tempdata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
# names(raindata) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
# 
# # extract data based on the acesssion coordinates
# for (j in c("pearlmil","breadf","fingermil","pigeon","taro","yam","world_rands")){
#   crop = get(j)
#   temp = data.frame()
#   rain = data.frame()
#   
#   for (i in 1:12){
#     temp = rbind(temp, as.numeric(extract(tempdata[[i]], crop)))
#     rain = rbind(rain, as.numeric(extract(raindata[[i]], crop)))
#   }
#   
#   row.names(temp) = c("tJan", "tFeb", "tMar", "tApr", "tMay", "tJun", "tJul", "tAug", "tSep",  "tOct", "tNov", "tDec")
#   row.names(rain) = c("rJan", "rFeb", "rMar", "rApr", "rMay", "run", "rJul", "rAug", "rSep",  "rOct", "rNov", "rDec")
#   
#   write.csv(cbind(t(temp), t(rain),coordinates(crop)), paste("WCLIMextract_",j,".csv",sep=""))
# }    
# 
# 
# ## combining soil and climate data
# pearlmils <- read.csv("./data/accession/ISRICextract_pearlmil.csv")#15535
# pearlmilc <- read.csv("./data/accession/WCLIMextract_pearlmil.csv")
# pearlmilm <-right_join(pearlmilc,pearlmils, by = c("lon","lat"),copy=T) 
# 
# pearlmil <- read.csv("./data/accession/genesys-accessions-filtered _pearl_millet.csv")
# pearlmilmm <-right_join(pearlmilm,pearlmil, by = c("lon","lat"),copy=T) 
# 
# 
# r=cbind(pearlmilmm$lon, pearlmilmm$lat)
#  t=which(duplicated(r) )
#  pearlmilmm=pearlmilmm[-t,]
# pearlmilmm=pearlmilm[complete.cases(pearlmilmm),]
# write.csv(pearlmilm, "./data/accession/WCLIMISRICextract_pearlmilm.csv", row.names = F)
# 
# breadfs <- read.csv("./data/accession/ISRICextract_breadf.csv") 
# breadfc <- read.csv("./data/accession/WCLIMextract_breadf.csv")
# breadfm <-right_join(breadfc,breadfs, by = c("lon","lat"),copy=T)
#  r=cbind(breadfm$lon, breadfm$lat)
#  t=which(duplicated(r) )
#  breadfm=breadfm[-t,]
# breadfm=breadfm[complete.cases(breadfm),]
# write.csv(breadfm, "./data/accession/WCLIMISRICextract_breadfm.csv", row.names = F)
# 
# fingermils <- read.csv("./data/accession/ISRICextract_fingermil.csv")#
# fingermilc <- read.csv("./data/accession/WCLIMextract_fingermil.csv")
# fingermilm <-right_join(fingermilc,fingermils, by = c("lon","lat"),copy=T)
# r=cbind(fingermilm$lon, fingermilm$lat)
#  t=which(duplicated(r) )
#  fingermilm=fingermilm[-t,]
#  fingermilm=fingermilm[complete.cases(fingermilm),]
# write.csv(fingermilm, "./data/accession/WCLIMISRICextract_fingermilm.csv",row.names=F)
# 
# pigeons <- read.csv("./data/accession/ISRICextract_pigeon.csv") #
# pigeonc <- read.csv("./data/accession/WCLIMextract_pigeon.csv")
# pigeonm <-right_join(pigeonc,pigeons, by = c("lon","lat"),copy=T)
#  r=cbind(pigeonm$lon, pigeonm$lat)
#  t=which(duplicated(r) )
#  pigeonm=pigeonm[-t,]
#  pigeonm=pigeonm[complete.cases(pigeonm),]
# write.csv(pigeonm, "./data/accession/WCLIMISRICextract_pigeonm.csv",row.names=F)
# 
# taros <- read.csv("./data/accession/ISRICextract_taro.csv")
# taroc <- read.csv("./data/accession/WCLIMextract_taro.csv")
# tarom <-right_join(taroc,taros, by = c("lon","lat"),copy=T)
#  r=cbind(tarom$lon, tarom$lat)
#  t=which(duplicated(r) )
#  tarom=tarom[-t,]
#  tarom=tarom[complete.cases(tarom),]
# write.csv(tarom, "./data/accession/WCLIMISRICextract_tarom.csv",row.names=F)
# 
# 
# yams <- read.csv("./data/accession/ISRICextract_yam.csv")
# yamc <- read.csv("./data/accession/WCLIMextract_yam.csv")
# yamm <-right_join(yamc,yams, by = c("lon","lat"),copy=T)
#  r=cbind(yamm$lon, yamm$lat)
#  t=which(duplicated(r) )
#  yamm=yamm[-t,]
# yamm=yamm[complete.cases(yamm),]
# write.csv(yamm, "./data/accession/WCLIMISRICextract_yamm.csv",row.names=F)
# 
# worlds <- read.csv("./data/accession/ISRICextract_world_rands.csv") #
# worldc <- read.csv("./data/accession/WCLIMextract_world_rands.csv")
# worlds <- worlds[complete.cases(worlds),]
# worldc <- worldc[complete.cases(worldc),]
# worlds$lat <- round(worlds$lat, 2)
# worldc$lat <- round(worldc$lat, 2)
# worlds$lon <- round(worlds$lon, 2)
# worldc$lon <- round(worldc$lon, 2)
# worldm <-inner_join(worldc,worlds, by = c("lon","lat"),copy=T)
# nrow(worldm)
# worldm=worldm[complete.cases(worldm),]
# nrow(worldm)
# write.csv(worldm, "./data/accession/WCLIMISRICextract_worldm.csv",row.names=F)
# 
