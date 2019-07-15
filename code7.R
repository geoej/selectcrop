# soil-climate suitability index
# ebrahim.jahanshiri@cffresearch.org
# e.jahanshiri@gmail.com
# library(RCurl)
# library(raster)
# library(rgdal)
# library(gdalUtils)
# library(maptools) #gpclibPermit()

# Section One ---------------------------------

# Section Two =================================

### Section Three ############################# 


### Reading ecological niche data ---------------------------------

setwd("D:\\CFFRC\\04-Research\\Soil\\climatesoilindex\\cmbndsuitindx")
ecology <- read.csv("./data/ecocrop/Ecocrop.csv")
# 10001000039 pear_millet
# 10001000059 bredfruit
# 10001000021 finger millet
# 10001000035 pigeon_pea
# 10001000048 taro
# 10001000051 yam


# getting the soil and climate data for accessions ---------------------------------

pearlmilm = read.csv("./data/accession/WCLIMISRICextract_pearlmilm.csv")
breadfm = read.csv("./data/accession/WCLIMISRICextract_breadfm.csv")
fingermilm = read.csv("./data/accession/WCLIMISRICextract_fingermilm.csv")
pigeonm = read.csv("./data/accession/WCLIMISRICextract_pigeonm.csv")
yamm = read.csv("./data/accession/WCLIMISRICextract_yamm.csv") 

for (crop in c("breadfm","pearlmilm","fingermilm","pigeonm","yamm")){
  #crop = "breadfm"
  cropdata = get(crop)#[1:30,]
  if (crop == "breadfm") {idx = 4}
  if (crop == "pearlmilm") {idx = 6}
  if (crop == "fingermilm") {idx = 5}
  if (crop == "pigeonm") {idx = 1}
  if (crop == "yam") {idx = 3}
  
  tempdata = cropdata[,2:13]
  raindata = cropdata[,14:25]
 
  # Ecocrop.Code         CropID                        Sci.Name Ecocrop.Lifeform
  # 1          576      Pigeonpea                   Cajanus cajan            shrub
  # 2          758 Taro (Cocoyam)             Colocasia esculenta             herb
  # 3          936      White Yam                 Dioscorea alata             vine
  # 4         3423     Breadfruit              Artocarpus altilis             tree
  # 5         5657  Finger Millet Eleusine coracana ssp. coracana            grass
  # 6         8418   Pearl Millet              Pennisetum glaucum            grass

    # GETTING THE SEASONS RIGHT 
# average the temp for that crop to be used for temp suitability

season = round ((ecology[idx,"Ecocrop.Crop.Cycle.Min"] + ecology[idx,"Ecocrop.Crop.Cycle.Max"])/30)

for (k in 1:nrow(cropdata)){
#for (j in 1:nrow(data)){
  tempdataverage = numeric()
  tempdatapren = numeric()
  if (season == 0) {     #{stop("Season can not be 0")}
    # This is wrong but for the sake of now
    # to calcuate for the prennials all layers are the same
    for (i in 1:12){
      tempdataverage = append(tempdataverage, mean(as.numeric(tempdata[k,1:12])))
    }
  }
  if ((season <= 12) & (season > 0)) {
    for (i in 1:(12-(season-1))){
      tempdataverage = append(tempdataverage, mean(as.numeric(tempdata[k,i:(i+(season-1))])))
    }
    
    #creating a stack for the months that fall over dec
    tempdatapren = c(tempdata[k,(12-(season-2)):12], tempdata[k,1:(season-1)])
    
    # adding the aggreages for the rest of the year
    for (i in 1:(length(tempdatapren)-(season-1))){
      tempdataverage = append(tempdataverage, mean(as.numeric(tempdatapren[i:(i+(season-1))])))
    }
  }
  if (season > 12) {
    # to calcuate for the prennials all layers are the same
    for (i in 1:12){
      tempdataverage = append(tempdataverage, mean(as.numeric(tempdata[k,1:12])))
    }
  }
  names(tempdataverage) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  assign(paste("tempaverage",k, sep="") , tempdataverage) 
#}
}

# calculate the seasonal suitability for average seasonal temperature for n crops

tempsuitfun = function(x) {
  y=0
  tabs_min=ecology[idx,"Agr.Ecol.Abs.Temp.Min"] 
  topt_min=ecology[idx,"Agr.Ecol.Opt.Temp.Min"] 
  topt_max=ecology[idx,"Agr.Ecol.Opt.Temp.Max"] 
  tabs_max=ecology[idx,"Agr.Ecol.Abs.Temp.Max"]
  if (any(x > tabs_min) & any(x < topt_min))  {y = round(((x - tabs_min)/(topt_min - tabs_min))*100)}
  if (any(x > topt_min) & any(x < topt_max)) {y  = 100}
  if (any(x > topt_max) & any(x < tabs_max))  {y  = round((1-( (x - topt_max)/(tabs_max - topt_max)))*100)}
  if (any(x < tabs_min) | any(x > tabs_max))  {y = 0}
  return(y)
}  

for (i in 1:nrow(cropdata)){
  tempsuit = numeric()  
  for (j in 1:12){
    
    tempcalc <- get(paste("tempaverage",i, sep=""))[j]/10 
    #tempcalc[is.na(tempcalc)] <- -9999 # to get rid of NA's
    
    tempcalc <- lapply(tempcalc, tempsuitfun )
    
    tempsuit= append(tempsuit, tempcalc)
    #tempsuit = addLayer(tempsuit, tempcalc)
  }
  names(tempsuit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  #assign(paste(data[i,1],"_tempsuit", sep="") , tempsuit)
  assign(paste("tempsuit",i, sep="") , tempsuit)
}

# #control the quality
# text=numeric()
# for (i in 1:nrow(cropdata)){
#   text = rbind(text,get(paste("tempsuit",i,sep="")))
#                 }


# Aggregate the rain for each season starting from jan

# For a number of crops from 1 to n, get the season right 
# aggreate the rain for that crop to be use for rain suitability
for (k in 1:nrow(cropdata)){
  raindataggreg = numeric()
  raindatapren = numeric()
  if (season == 0) {     #{stop("Season can not be 0")}
    # This is wrong but for the sake of now
    # to calcuate for the prennials all layers are the same
    for (i in 1:12){
      raindataggreg = append(raindataggreg, sum(raindata[k,1:12]))
    }
  }
  if ((season <= 12) & (season > 0)) {
    for (i in 1:(12-(season-1))){
      raindataggreg = append(raindataggreg, sum(as.numeric(raindata[k,i:(i+(season-1))])))
    }
    
    #creating a stack for the months that fall over dec
    raindatapren = c(raindata[k,(12-(season-2)):12], raindata[k,1:(season-1)])
    
    # adding the aggreages for the rest of the year
    for (i in 1:(length(raindatapren)-(season-1))){
      raindataggreg = append(raindataggreg, sum(as.numeric(raindatapren[i:(i+(season-1))])))
    }
  }
  if (season > 12) {
    # to calcuate for the prennials all layers are the same
    for (i in 1:12){
      raindataggreg = append(raindataggreg, sum(as.numeric(raindata[k,1:12])))
    }
  }
  names(raindataggreg) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  assign(paste("rainaggreg",k, sep="") , raindataggreg) 
}
#    spplot(raindataggreg, main="Aggregate rainfal (for 6 month season starting each month) - Hulu Langat - Malaysia",
#       col.regions = rainbow(99, start=0.1))

#---

# calcualte rain suitability

rainsuitfun = function(x) {
  y=0
  rabs_min=ecology[idx,"Agr.Ecol.Abst.Rain.Min"] 
  ropt_min=ecology[idx,"Agr.Ecol.Opt.Rain.Min"] 
  ropt_max=ecology[idx,"Agr.Ecol.Opt.Rain.Max"] 
  rabs_max=ecology[idx,"Agr.Ecol.Abst.Rain.Max"]
  if (any(x > rabs_min) & any(x < ropt_min))  {y = round(((x - rabs_min)/(ropt_min - rabs_min))*100)}
  if (any(x > ropt_min) & any(x < ropt_max)) {y  = 100}
  if (any(x > ropt_max) & any(x < rabs_max))  {y  = round((1-( (x - ropt_max)/(rabs_max - ropt_max)))*100)}
  if (any(x < rabs_min) | any(x > rabs_max))  {y = 0}
  return(y)
}  

for (i in 1:nrow(cropdata)){
  rainsuit = numeric()  
  for (j in 1:12){
    
    raincalc <- get(paste("rainaggreg",i, sep=""))[j]
    #raincalc[is.na(raincalc)] <- -9999 # to get rid of NA's
    
    raincalc <- lapply(raincalc, rainsuitfun )
    
    #raincalc = mask(crop(raincalc, extent(boundary)), boundary) #masking zeros 
    
    rainsuit= append(rainsuit, raincalc)
    
  }
  names(rainsuit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  assign(paste("rainsuit",i, sep="") , rainsuit)
}

# control the quality
#text=numeric()
# for (i in 1:nrow(cropdata)){
#   text = rbind(text,get(paste("rainsuit",i,sep="")))
# }


# calcualte PH suitability
phsuitfun = function(x) {
  y=0
  phabs_min=ecology[idx,"Agr.Ecol.Abs.Ph.Min"] 
  phopt_min=ecology[idx,"Agr.Ecol.Opt.Ph.Min"] 
  phopt_max=ecology[idx,"Agr.Ecol.Opt.Ph.Max"] 
  phabs_max=ecology[idx,"Agr.Ecol.Abs.Ph.Max"]
  if (any(x > phabs_min) & any(x < phopt_min))  {y = round(((x - phabs_min)/(phopt_min - phabs_min))*100)}
  if (any(x > phopt_min) & any(x < phopt_max)) {y  = 100}
  if (any(x > phopt_max) & any(x < phabs_max))  {y  = round((1-( (x - phopt_max)/(phabs_max - phopt_max)))*100)}
  if (any(x < phabs_min) | any(x > phabs_max))  {y = 0}
  return(y)
}  

for (k in 1:nrow(cropdata)){
  phsuit = numeric()  
  for (j in 1:7){
     
    phcalc <- cropdata[k,72+j]/10

    phcalc <- lapply(phcalc, phsuitfun )
    
    phsuit= append(phsuit, phcalc)
    
  }
  names(phsuit) <- c("L1","L2","L3","L4","L5","L6","L7")
  assign(paste("phsuit",k, sep="") , phsuit)
}

# control the quality
# text=numeric()
# for (i in 1:nrow(cropdata)){
#   text = rbind(text,get(paste("phsuit",i,sep="")))
# }



# calcualte depth suitability


depthsuitfun = function(x) {
  y=0
  t="t"    
  if (x < 20) {t = "very shallow (<20 cm)"}
  if ((x >=20) & (x< 50)) {t = "shallow (20-50 cm)"}
  if ((x >= 50) & (x< 150)) {t = "medium (50-150 cm)"}
  if (x >= 150) {t = "deep (>>150 cm)"}
  optz = ecology[idx,"Agr.Ecol.Opt.Soildp.Medium"]
  absz = ecology[idx,"Agr.Ecol.Abs.Soildp.Mean"]
  if (t == optz & t == absz) {y=100}
  if (t != optz & t == absz) {y=50}
  if (t == optz & t != absz) {y=80}
  if (t != optz & t != absz) {y=10}
  y
  return(y)
}  
depthsuit = numeric() 
for (k in 1:nrow(cropdata)){
  
  depthcalc <- lapply(cropdata[k,"BDRICM.BDRICM_M" ], depthsuitfun )
  depthsuit= append(depthsuit, depthcalc)
  
}
depthsuit <- as.numeric(depthsuit)
names(depthsuit) <- c(sprintf("Depthsuit",seq(1:k)))
                           



# calculate the soil texture suitability for the selected crops
txtopt <- strsplit(as.character(ecology[idx,"Agr.Ecol.Opt.Soiltxt.Med"]), ";")
txtabs <- strsplit(as.character(ecology[idx,"Agr.Ecol.Abs.Soiltxt.Med"]), ";")

texturesuit = numeric()  

for (k in 1:nrow(cropdata)){
  # for (j in 1:7){
  #texturesuitfun = function(x) {
  #y=0
  #texture = list()
  
  #txtopt <- data[i,"Agr.Ecol.Opt.Soiltxt.Med"]
  #txtabs <- data[i,"Agr.Ecol.Abs.Soiltxt.Med"]
  
  #if (SLTPPT[1] <= 10) {t = "heavy"}
  if (((0 <= cropdata[k,"SNDPPT.M.sl1"]) & (cropdata[k,"SNDPPT.M.sl1"] <= 65)) &
      ((0 <= cropdata[k,"SLTPPT.M.sl1"]) & (cropdata[k,"SLTPPT.M.sl1"] <= 60)) &
      ((35 <= cropdata[k,"CLYPPT.M.sl1"]) & (cropdata[k,"CLYPPT.M.sl1"] <= 100))
  ) { texture = "heavy"}
  
  if (((0 <= cropdata[k,"SNDPPT.M.sl1"]) & (cropdata[k,"SNDPPT.M.sl1"] <= 52)) &
      ((28 <= cropdata[k,"SLTPPT.M.sl1"]) & (cropdata[k,"SLTPPT.M.sl1"] <= 100)) &
      ((0 <= cropdata[k,"CLYPPT.M.sl1"]) & (cropdata[k,"CLYPPT.M.sl1"] <= 27))
  ) { texture = "medium"}
  
  if (((70 <= cropdata[k,"SNDPPT.M.sl1"]) & (cropdata[k,"SNDPPT.M.sl1"] <= 100)) &
      ((0 <= cropdata[k,"SLTPPT.M.sl1"]) & (cropdata[k,"SLTPPT.M.sl1"] <= 30)) &
      ((0 <= cropdata[k,"CLYPPT.M.sl1"]) & (cropdata[k,"CLYPPT.M.sl1"] <= 13))
  ) { texture = "light"}
  
  if (exists("texture")){
    if (texture == txtopt[[1]][1]) { y = 100}
    if (texture == txtabs[[1]][1]) { y = 50}
      } else {y = 10}
  #return(y)
  #}  
  #texturecalc <- lapply(uBDRICM, texturesuitfun )
  texturesuit = append(texturesuit, y)
}


# calculate total suitability 
for (k in 1:nrow(cropdata)){
  
  temptotcal = as.numeric(get(paste("tempsuit",k, sep="")))
  raintotcal = as.numeric(get(paste("rainsuit",k, sep="")))
  phtotcal = as.numeric(get(paste("phsuit",k, sep="")))[1]
  depthtotcal = as.numeric(depthsuit[k])
  txturtotcal = as.numeric(texturesuit[k])
  totalsuit = numeric()
  averagesuit = numeric()
  norainsuit = numeric()
  
  for (i in 1:12){
   totalsuit=append(totalsuit,(0.00000001*(temptotcal[i]* raintotcal[i] * phtotcal * depthtotcal * txturtotcal))) 
   averagesuit=append(averagesuit,(mean(c(temptotcal[i], raintotcal[i] , phtotcal , depthtotcal , txturtotcal)))) 
   norainsuit=append(norainsuit,(0.000001*(temptotcal[i]* phtotcal * depthtotcal * txturtotcal))) 
   
   #totalsuit=append(totalsuit,(0.01*(temptotcal[i]* raintotcal[i]))) 
    
    }
  #names(totalsuit) <- data[1:nrow(data),1]
  #names(totalsuit) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
  assign(paste("totalsuit",k, sep="") , totalsuit)
  assign(paste("averagesuit",k, sep="") , averagesuit)
  assign(paste("norainesuit",k, sep="") , norainsuit)
  
  assign(paste("calc",k, sep=""), cbind(temp = temptotcal, rain =raintotcal , ph=rep(phtotcal,12) , depth=rep(depthtotcal,12), texture=rep(txturtotcal,12), total = totalsuit, noraintotal=norainsuit, avetotal = averagesuit))
}


#control the quality
TS=data.frame()
calcs= data.frame()
yearlycalc <- data.frame()
aveTS=data.frame()



for (i in 1:nrow(cropdata)){
  TS = rbind(TS,get(paste("totalsuit",i,sep="")))
  calcs = rbind(calcs,get(paste("calc",i,sep="")))
  yearlycalc = rbind(yearlycalc, c((mean(c(get(paste("calc",i,sep=""))[,"temp"]))),
                                   (mean(c(get(paste("calc",i,sep=""))[,"rain"]))),
                                     (mean(c(get(paste("calc",i,sep=""))[,"ph"]))),
                                       (mean(c(get(paste("calc",i,sep=""))[,"depth"]))),
                                         (mean(c(get(paste("calc",i,sep=""))[,"texture"]))),
                                   (mean(c(get(paste("calc",i,sep=""))[,"total"]))),
                                   (mean(c(get(paste("calc",i,sep=""))[,"noraintotal"]))),
                                   (mean(c(get(paste("calc",i,sep=""))[,"avetotal"])))))
  aveTS = rbind(aveTS,get(paste("averagesuit",i,sep="")))
}

names(TS) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
names(yearlycalc) = names(calcs)

write.csv(cbind(TS,crop), paste("TS",crop,".csv",sep=""))
write.csv(cbind(aveTS,crop), paste("aveTS",crop,".csv",sep=""))
write.csv(cbind(calcs,crop), paste("Calcs",crop,".csv",sep=""))
write.csv(cbind(yearlycalc,cropdata[,c("lon","lat")]), paste("yearlycalc",crop,".csv",sep=""))

#rm(list = ls())
}

