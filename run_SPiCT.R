# Stochastic surplus production model in continuous time (SPiCT)
# Marta Cousido-Rocha and Francisco Izquierdo

# This code takes the input SPiCT data from the data.ss file. This means
# that for each run i the code goes to the "1area_25" folder, and then
# to the dat_1A_i folder and read the data.ss file. 
# Then, previously to run the current script the first part of "01 run_models.R" 
# must be run. More precisely, run such script until line 78 (end of first loop)
# to create the 100 folders of SS models.
# Then the script runs SPiCT for each data set and save the biomass and fishing
# mortality trajectories and the MSY reference points.

rm(list = ls())
# Packages ---------------------------------------------------------------------
#remotes::install_github("DTUAqua/spict/spict")
library(spict)
library(readxl)
library(plyr)
library(dplyr)

saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
dir_mod = '1A_25_ageS_PY_CPUEst_tags'

# Important information:
n_areas = 1
Nsamp = 25
yrvector = 1001:1256
trueyr = rep(1952:2015, each = 4)

# Load data:
type_data = paste0(n_areas, 'area_', Nsamp)
this_file = paste0("sim_data/YFT_", n_areas,"area_observations_1_100_ESS_", Nsamp,".RData")
mydata = load(this_file)

Bio_spict_cpue_original=matrix(0, ncol=length(seq_along(mydata)), nrow=length(1972:2015))
F_spict_cpue_original=matrix(0, ncol=length(seq_along(mydata)), nrow=length(1972:2015))

Drp=matrix(0, ncol=length(seq_along(mydata)), nrow=2)
Srp=matrix(0, ncol=length(seq_along(mydata)), nrow=2)

rownames(Drp)=c("Bmsy","Fmsy")
rownames(Srp)=c("Bmsy","Fmsy")
  
for(j in seq_along(mydata)) {
  
  template_dat = r4ss::SS_readdat_3.30(file = paste0(file.path(saveDir, dir_mod, mydata[j]), '/data_ss.dat'))
  # Catch by fleet
  catch_by_fleet=template_dat$catch
  catch<-ddply(catch_by_fleet, .(year,seas), summarize,  cat=sum(catch))

  # LDFs by fleet
  LFD_by_fleet=template_dat$lencomp
  
  # Weight-length relationship:
  len=as.numeric(substr(colnames(LFD_by_fleet)[-(1:6)], 2, 4))
  a=2.459*10^{-5}
  b=2.9667
  wei=a*len^b
 
  # LFDs sum across fleets:
  LFD<-LFD_by_fleet %>% dplyr::group_by(Yr, Seas) %>% dplyr::summarise(across(everything(), list(sum)))
  LFD=LFD[,-(3:6)]
  LFD$Yr = trueyr[yrvector %in% LFD$Yr] # only for PY configuration
  
  # Combine catch (numbers), LFDs and weight to obtain catch (tons):
  l=dim(LFD)[1]
  
  resul=as.data.frame(LFD[,1:2])
  resul$cat=rep(0,l)
  for (i in 1:l){
    
    tot=sum(LFD[i,-c(1,2)])
    
    lfd=(LFD[i,-c(1,2)]*(catch[i,3]/tot))
    
    resul[i,3]=sum(lfd*wei)
  }
  
  # Sum across year:
  catch_year=ddply(resul, .(Yr), summarize,  cat=sum(cat))

  # CPUE
  cpue=template_dat$CPUE
  cpue$year = trueyr[yrvector %in% cpue$year] # only for PY configuration
  cpue_year<-ddply(cpue, .(year), summarize,  obs=sum(obs))
  
  ind1=which(unique(resul$Yr)==1972)
  ind2=which(unique(resul$Yr)==2015)
  
  inp1 <- list(timeC = unique(resul$Yr)[ind1:ind2], obsC =catch_year$cat[ind1:ind2],
               timeI = list(cpue_year$year+0.5),
               obsI = list(cpue_year$obs))
  
  inp1=check.inp(inp1)
  res1 <- fit.spict(inp1)
  
  bio1=get.par("logB", res1, exp = TRUE)[seq(1,length(inp1$time),by=16),2]
  F1=get.par("logFs", res1, exp = TRUE)[seq(1,length(inp1$time),by=16),2]
  
  Bio_spict_cpue_original[,j]=bio1[1:length(1972:2015)]
  F_spict_cpue_original[,j]=F1[1:length(1972:2015)]
  
  Drp[,j]= c(get.par('logBmsyd', res1, exp=TRUE)[,2],
             get.par('logFmsyd', res1, exp=TRUE)[,2])
  Srp[,j]= c(get.par('logBmsys', res1, exp=TRUE)[,2],
             get.par('logFmsys', res1, exp=TRUE)[,2])
  
  print(j)
}

dir.create(path = "SPiCT results")
save(Bio_spict_cpue_original,file=paste0("SPiCT results/Bio_spict_cpue_original.RData"))
save(F_spict_cpue_original,file=paste0("SPiCT results/F_spict_cpue_original.RData"))
save(Drp,file=paste0("SPiCT results/Drp.RData"))
save(Srp,file=paste0("SPiCT results/Srp.RData"))

# HACER PLOT EN ESCALA ABSOLUTA Y RELATIVA (SSB/SSBmsy)