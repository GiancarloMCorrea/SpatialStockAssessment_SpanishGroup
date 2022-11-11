rm(list = ls())

library(foreach)
library(doParallel)
library(doSNOW)

setwd('C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup/221 CPUE')
# Number of replicates
nRep = 100

# Create directory
out_01<-paste(getwd(),"/output/01 iid model", sep="")
dir.create(out_01)
out_02<-paste(getwd(),"/output/01 iid model/SSinput", sep="")
dir.create(out_02)
out_011<-paste(getwd(),"/output/01 iid model/predicted CPUE st", sep="")
dir.create(out_011)

# Begin running
cores = detectCores()
cl = makeCluster(cores[1] - 5)
registerDoSNOW(cl)

foreach(ix = 1:nRep) %dopar% {
  
  library(INLA)
  library(sf)
  library(ggspatial)
  library(rnaturalearth)
  library(maps)
  library(ggplot2)
  library(dplyr)
  library(plyr)
  library(magick)
  
  source('01 iid model.R')
  
}

stopCluster(cl)
