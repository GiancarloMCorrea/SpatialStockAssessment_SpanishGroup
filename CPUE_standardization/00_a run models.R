rm(list = ls())

library(foreach)
library(doParallel)
library(doSNOW)

setwd('C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup/221 CPUE')
# Number of replicates
nRep = 100

# Create directory: Model iid
# out_01<-paste(getwd(),"/output/01 iid model", sep="")
# dir.create(out_01)
# out_02<-paste(getwd(),"/output/01 iid model/SSinput", sep="")
# dir.create(out_02)
# out_011<-paste(getwd(),"/output/01 iid model/predicted CPUE st", sep="")
# dir.create(out_011)

# Create directory: Model besag
out_11<-paste(getwd(),"/output/02 besag model", sep="")
dir.create(out_11)
out_12<-paste(getwd(),"/output/02 besag model/SSinput", sep="")
dir.create(out_12)
out_111<-paste(getwd(),"/output/02 besag model/predicted CPUE st", sep="")
dir.create(out_111)

# Begin running
#cores = detectCores()
#cl = makeCluster(cores[1] - 5)
#registerDoSNOW(cl)

#foreach(ix = 1:nRep) %dopar% {
for(ix in 1:nRep){
  
  library(INLA)
  library(sf)
  library(ggspatial)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(maps)
  library(ggplot2)
  library(dplyr)
  library(plyr)
  library(magick)
  library(spdep)
  
  #source('01 iid model.R', local = TRUE)
  source('02 besag model.R', local = TRUE)
  
}

#stopCluster(cl)
