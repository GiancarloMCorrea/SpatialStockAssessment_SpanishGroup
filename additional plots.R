#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate additional plots for SS 4 areas model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 31/05/2022 #
#~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo #
# Marta Cousido       #
#~~~~~~~~~~~~~~~~~~~~~~~

## Press Ctrl + Shift + O to see the document outline

# Edit here! -------------------------------------------------------------------

rm(list=ls()) ## Clean environment
library(ggridges)
library(r4ss)
library(tidyverse)
library(icesAdvice)
library(icesTAF)

## Model path
run <- 'run base' ## *CHANGE name
mod_path <- paste0(getwd(), "/model/", run, sep="") 

## Create plots folder
plotdir<-paste(mod_path, '/additional plots', sep="")
mkdir(plotdir)

## Indicate dir
data.file <- 'data.ss'
data.file  <- file.path(mod_path, data.file)
run <- paste0("_", run)

## Read output
output <- SS_output(dir = mod_path, repfile = "Report.sso", 
                    compfile = "CompReport.sso",covarfile = "covar.sso", 
                    ncols = 200,forefile="forecast.ss",warn = TRUE,covar = TRUE, 
                    checkcor = TRUE, cormax = 0.95, cormin = 0.01,
                    printhighcor = 10, printlowcor = 10,  verbose = TRUE,
                    printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE,
                    aalmaxbinrange=0)
    
output$nforecastyears <- ifelse(is.na(output$nforecastyears), 0, 
                                output$nforecastyears)

## Set correct number of years
forecast <- T
nyearsaveragesel     <- 3
dtyr <- output$endyr
years     <- output$startyr:dtyr
yearsfore <- output$startyr:(dtyr+output$N_forecast_yrs) 
nyears    <- length(years)
fltnms <- setNames(output$definitions$Fleet_name,1:8)

## Model datafile
start <- r4ss::SS_readstarter(file = file.path(mod_path, "starter.ss"), 
                              verbose = FALSE)
ss3Dat <- r4ss::SS_readdat(file = file.path(mod_path, start$datfile),
                        verbose = FALSE)

# SS plot ----------------------------------------------------------------------

## SS plot
pdf(paste(plotdir, "/SS multiplot.pdf", sep=""), height = 6.5, width = 11)
par(mfrow=c(3,3))
SSplotCatch(output, subplots=10); title("Landings")
SSplotSummaryF(output); title("F")
SSplotBiology(output,subplots = 1)
SSplotTimeseries(output, subplot = 3, minyr = 1982); title("Recruits")
SSplotRecdevs(output, subplots=1);title("recdevs")
SSplotBiology(output,subplots = 6); title("Mat")
SSplotSelex(output, subplot = 1)
SSplotTimeseries(output, subplot = 7); title("Biomass")
SSplotTimeseries(output, subplot = 7, minyr = 1982, maxyr = 2021); title("Biomass recent")
dev.off()

# Catch ------------------------------------------------------------------------

## Prepare objects and plot observed and Fitted landings & discards by fleet 
## N for numbers, B for Biomass

## Catch
catch <- as_tibble(output$timeseries) %>% filter(Era == 'TIME') %>% 
  select("Yr", "Seas", starts_with("obs_cat"), starts_with("retain(N)"), starts_with("dead(N)")) 
names(catch) <- c('year', 'season', paste('LanObs', fltnms[1:output$nfishfleets], sep = "_"), paste('LanEst', fltnms[1:output$nfishfleets], sep = "_"),
                  paste('CatEst', fltnms[1:output$nfishfleets], sep = "_"))

aux1 <- catch %>% select(starts_with('CatEst')) - catch %>% select(starts_with('LanEst'))
names(aux1) <- paste('DisEst', fltnms[1:output$nfishfleets], sep = "_")
catch <- catch %>% bind_cols(aux1) 
catch <- catch %>% pivot_longer(cols = names(catch)[-(1:2)], names_to = 'id', values_to = 'value') %>% 
  mutate(indicator = substr(id,1,6), fleet = substr(id, 8, nchar(id))) %>% 
  select('year', 'season', 'fleet', 'indicator', 'value')  

catch <- catch %>% 
  mutate(category = ifelse(substr(indicator,1,3) == 'Lan', 'Landings', ifelse(substr(indicator,1,3) == 'Dis', 'Discards', 'Catch')),
         type = ifelse(substr(indicator,4,6) == 'Est', 'Estimated', 'Observed'),
         time = year + 1.125 - 1/season)  %>% 
  select('year', 'season', 'time', 'fleet', 'category', 'type', 'indicator', 'value')  

## Total landings: Fitted vs Observed

pdf(paste(plotdir, "/Total landings.pdf", sep=""), height = 6.5, width = 11)

library(plyr)
conflict_prefer("summarize", "plyr")
catch1<-ddply(catch, .(year, category, type, indicator), summarize,  value=sum(value))

estLandDisc <- ggplot(catch1%>% filter(category == 'Landings')) +
                 geom_line(aes(year, value, group = indicator, color = type, linetype=type), size = 1) + 
                 geom_point(aes(year, value, group = indicator, color = type), size = 2) +
                 facet_grid(category~., scales = 'free') + scale_color_manual(values = c("dodgerblue3","black"))+ 
                theme_light() +theme(plot.title = element_text(size=11)) + ggtitle("Landings")
estLandDisc

dev.off()

## By Fleet: Landings: Fitted vs Observed.

pdf(paste(plotdir, "/Total landings by fleet.pdf", sep=""), height = 6.5, width = 11)

catch2<-ddply(catch, .(time,year, category, type, indicator,fleet), summarize,  value=sum(value))

estLand_fl <- ggplot(catch2%>% filter(category == 'Landings') ) +
               geom_line(aes(x=time, y=value, group = indicator, color = type), size = 0.5) + 
               geom_point(aes(time, value, group = indicator, color = type), size = 1) +
               facet_wrap(~fleet, scales = 'free', ncol = 2) + scale_color_manual(values = c("dodgerblue3","black")) +
               ggtitle('Landings by fleet')+ theme_light() +theme(plot.title = element_text(size=11))
estLand_fl

dev.off()

# LFD's ------------------------------------------------------------------------

## Length Frequencies: Distribution & bubble plots
## Note that CPUE indices have mirrored LFDs

LFD    <- as_tibble(output$lendbase[, c('Yr', 'Seas', 'Time', 'YrSeasName',  
                                        'Fleet', 'Part', 'Bin', 'Obs', 'Exp', 
                                        'Pearson','Sex')])
fltNms <- setNames(output$definitions[,'Fleet_name'], output$definitions[,1]) 
CaComp <- setNames(c('cat','dis','lan'), 0:2) 

LFD <- LFD %>% mutate(FleetNm = fltNms[Fleet], 
                      CatchComponent = CaComp[as.character(Part)]) %>% 
  pivot_longer(cols = c('Obs', 'Exp', 'Pearson'), names_to = 'variable', 
               values_to = 'value')

LFD_plots <- setNames(vector('list', output$nfleets), c(fltNms[1:output$nfleets]))

# Commercial fleets -----------------------------------------------------------

plot_list_LD = list()
plot_list_res = list()
#1:output$nfishfleets

for (i in c(1:14,16)) {

a<-ggplot(LFD %>% filter(Fleet %in% i, variable %in% c('Obs', 'Exp')), aes(x=Bin, height=value, y=factor(Yr), group = interaction(Yr, variable), fill = variable))+
  geom_density_ridges2(stat="identity", scale=1.2, alpha=0.3, size = 0.2) + 
  facet_wrap(~Seas, ncol = 4) + 
  ggtitle(paste("Commercial fleets -", i, sep=" "))+ 
  theme_light() +theme(plot.title = element_text(size=11))+ 
  xlab("LD by bin")

b<-ggplot(LFD %>% filter(Fleet %in% i, variable == "Pearson", CatchComponent == 'cat')) + 
  geom_point(aes(Time, Bin, size = abs(value),col= value<0),alpha=0.5,pch=16) +
  facet_wrap(~Seas, ncol = 4) + 
  scale_color_manual(values = c('blue','red')) +
  ggtitle(paste("Commercial fleets -", i, sep=" "))+ 
  theme_light() +theme(plot.title = element_text(size=11))

plot_list_LD[[i]] = a
plot_list_res[[i]] = b

}

# create pdfs
pdf(paste(plotdir,"/LDs by fleet.pdf",sep=""))
for (i in c(1:14,16)) {
  print(plot_list_LD[[i]])
}
dev.off()

# create pdfs
pdf(paste(plotdir,"/LDs residuals by fleet.pdf",sep=""))
for (i in c(1:14,16)) {
  print(plot_list_res[[i]])
}
dev.off()

# CPUE ---------------------------------------------------------------------

## Indices have no LFD, they are mirrored from other fleets
## CPUEs observed vs expected  

surveys <- as_tibble(output$cpue) %>% select('Fleet', 'Fleet_name', 'Yr', 'Month',
                                             'Time', 'Obs', 'Exp', 'SE') %>% 
    mutate(Obs = log(Obs), Exp = log(Exp), residuals = Obs-Exp, upp = Obs + 2*SE,
           low = Obs - 2*SE) 

plot_list = list()
plot_list_res = list()

cp<-split(surveys, surveys$Fleet_name)

for (i in 1:length(cp)) {
  
a<-ggplot(cp[[i]]) + geom_line(aes(Yr, Obs, group=Month), col = 'red') + 
      geom_ribbon(aes(x = Yr, ymin = low, ymax = upp), fill = 'red', alpha = 0.3) +
      geom_point(aes(Yr, Exp), col = 'blue') +
      facet_wrap(~Month, ncol = 2, scales = 'free_y') +
      ggtitle(paste("Survey exp. (blue) and obs. (red) - log scale ", i,sep=" ")) +
      theme_light() +theme(plot.title = element_text(size=11))
  
b<-ggplot(cp[[i]]) + geom_line(aes(Yr, residuals, group=Month)) + 
    geom_point(aes(Yr, residuals), col = 'blue') +
    facet_wrap(~Month, ncol = 2, scales = 'free_y')+
    geom_hline(yintercept = 0) +
    ggtitle(paste("Survey pearson residuals", i,sep=" "))+
    theme_light() +theme(plot.title = element_text(size=11))

plot_list[[i]] = a
plot_list_res[[i]] = b

}

# create pdf
pdf(paste(plotdir,"/CPUE.pdf",sep=""))
for (i in 1:length(cp)) {
  print(plot_list[[i]])
}
dev.off()

# create pdf
pdf(paste(plotdir,"/CPUE residuals.pdf",sep=""))
for (i in 1:length(cp)) {
  print(plot_list_res[[i]])
}
dev.off()
  
# Summary plot ------------------------------------------------------------------

## Function
summary_plot <- function(sumTab, file = NULL){
  x <- 75
  y <- (x/58)*27
  
  years <- unique(sumTab$year)
  dtyr <- max(years)
  
  if(!is.null(file)) pdf(file, width = 10.5, height = 6.5, pointsize = 12)
  par(mfrow = c(2,2), mar = rep(4,4))
  barplot(unlist(sumTab[,'CatObs'])/1000, main = 'Catches', ylab = 'Catches in 1000 t',
          xlab = '', names.arg = sumTab[,1], space = 1, col = 'grey')
  abline(h=0)
  
  
  a <- barplot(unlist(sumTab[,'rec'])/1000, main = 'Recruitment in millions', ylab = 'Recruitment (age 0)',
               xlab = '', names.arg = unlist(sumTab[,1]), space = 1, col = 'grey', ylim = c(0,max(unlist(sumTab[,'upperrec'])/1000)*1.2))
  arrows(a, unlist(sumTab[,'lowerrec'])/1000, a, unlist(sumTab[,'upperrec'])/1000, 
         angle = 90, code = 3, length = 0.025)

  
  plot(unlist(sumTab[,1]), unlist(sumTab[,'f']), ylab = 'F', main = 'Fishing mortality', 
       ylim = c(0,range(unlist(sumTab[,'f']))[2])*c(0.8,1.1), xlab = '', type = 'l', lwd = 2, axes = F)
  axis(1, at =  c(seq(years[1],dtyr,3),dtyr))
  polygon(c(years[1]:dtyr, dtyr:years[1]), c(unlist(sumTab[1:nyears,'lowerf']), unlist(sumTab[nyears:1,'upperf'])), col = 'gray')
  lines(unlist(sumTab[,1]), unlist(sumTab[,'f']), lwd = 2)
  axis(2)
  abline( h =  0.26 ,  col = 'green', lty = 2, lwd = 2)

  plot(unlist(sumTab[,1]), unlist(sumTab[,'ssb'])/1000, ylab = '1000 t', main = 'Spawning Stock Biomass', ylim = c(range(unlist(sumTab[,'ssb'])/1000))*c(0,1.2),
       xlab = '', type = 'l', lwd = 2, axes = F)
  polygon(c(years[1]:dtyr, dtyr:years[1]), c(unlist(sumTab[1:nyears,'lowerssb'])/1000, unlist(sumTab[nyears:1,'upperssb'])/1000), col = 'gray')
  lines(unlist(sumTab[,1]), unlist(sumTab[,'ssb'])/1000,col = 1, lty = 1, lwd = 2)
  axis(1, at =  c(seq(years[1],dtyr,3),dtyr))
  axis(2)

  if(!is.null(file))  dev.off()
}


## All to do with catches
library(conflicted)
conflict_prefer("summarise", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")

colnames(catch)<-c("year", "season","time","fleet","category","type","indicator","value")
cc <- catch %>% group_by(year,  indicator) %>% filter (year>0) %>% summarise(value = sum(value)) %>% 
            pivot_wider(id_cols = year, names_from = indicator)

## SSB, REC and F
ssbrecf <- output$derived_quants

## Bound of the CIs: ssb
spb <- ssbrecf[substr(ssbrecf$Label,1,3)=="SSB",]
ssb <- spb[3:(3+length(yearsfore)-1), 2:3]
upperssb <- ssb[(1:length(years)),1] + 1.96*ssb[(1:length(years)),2]
lowerssb <- ssb[(1:length(years)),1] - 1.96*ssb[(1:length(years)),2]

upperssb.proj <- ssb[length(years) + 1:3,1] + 1.96*ssb[length(years) + 1:3,2]
lowerssb.proj <- ssb[length(years) + 1:3,1] - 1.96*ssb[length(years) + 1:3,2]

## Bound of the CIs: recruitment
recr <- ssbrecf[substr(ssbrecf$Label,1,4)=="Recr",]
recr <- recr[(1:length(years))+2,2:3]

## 90% confidence interval for recruitment
recrCV <- recr[,2]/recr[,1]
recrSigma <- sqrt(log(1+recrCV^2))
upperrecr.90 <- exp(log(recr[,1]) + 1.645*recrSigma)
lowerrecr.90 <- exp(log(recr[,1]) - 1.645*recrSigma)

# 90% confidence interval for fishing mortality: We took the F at length calculated 
# in this code and apply the SD that comes up from SS3.
fratenum <- ssbrecf[substr(ssbrecf$Label,1,2)=="F_",]; 
fratenum <- fratenum[,2:3]
f <- fratenum[1:nyears,'Value']

upperf <- f + 1.96*((fratenum[1:nyears,'StdDev']/fratenum[1:nyears,'Value'])*f)
lowerf <- f - 1.96*((fratenum[1:nyears,'StdDev']/fratenum[1:nyears,'Value'])*f)

sumTab <- as_tibble(output$timeseries) %>% filter(Era == 'TIME') %>% 
              select("Yr", "Seas", 'Bio_all', 'SpawnBio') %>% 
              group_by(Yr) %>% summarise(ssb = sum(SpawnBio, na.rm = TRUE), biomass = sum(Bio_all)) %>% 
              mutate(rec = recr[,1], f = f) %>% bind_cols(cc[,-1]) %>% 
              mutate(CatObs =  LanObs, yield.ssb = CatObs/ssb, 
                     upperf = upperf, lowerf = lowerf, lowerrec = lowerrecr.90, upperrec = upperrecr.90,
                     upperssb = upperssb, lowerssb = lowerssb)
names(sumTab)[1] <- 'year'

## Summary plot
pdf(paste(plotdir, "/Summary plot.pdf", sep=""), height = 6.5, width = 11)
summary_plot(sumTab)
dev.off()

## SSB cut -------------------------------------------------------------------

## Maturity at length
## Length increments in population length bins (distance between bins)
increments <- output$lbinspop[-1]-output$lbinspop[-output$nlbinspop] 
increments <- c(increments, increments[length(increments)])

## Length at mid-point of population length bins
len <- output$lbinspop + increments/2
matslope <- as.numeric(output$MGparmAdj$"Mat_slope_Fem")[1]
matl50 <- as.numeric(output$MGparmAdj$"Mat50%_Fem")[1]  
matlen <- 1/( 1 + exp(matslope*(len - matl50 )) ) # Ogiva: % matures by length

## Weight at length 
wlena <- as.numeric(output$MGparmAdj$Wtlen_1_Fem)[1] 
wlenb <- as.numeric(output$MGparmAdj$Wtlen_2_Fem)[1] 
wlen <- wlena*(len^wlenb) # Weight at length for each one of the lengths 

library(dplyr)
natlen<-output$natlen
names(natlen)
natlen<-natlen %>% filter(Sex==1, `Beg/Mid`=="B", Seas==1)
library(plyr)
vec=colnames(natlen)
ind=which(vec=="10"); lvec=length(vec)
vec=vec[ind:lvec]

N=unique(natlen$Yr)
for (i in 1:length(vec)){
  
  aux=aggregate(natlen[,ind+i-1], by=list(Category=natlen$Yr), FUN=sum)
  
  N=cbind(N,aux[,2])
}
colnames(N)=c("Yr",vec)

## Total
laux=length(unique(natlen$Yr))
year=unique(natlen$Yr)[1:(laux-3)] # 3 forecast years
l_l=length(unique(wlen))
l_y=length((year))
SSB=matrix(0,ncol=l_l,nrow=l_y)
B=matrix(0,ncol=l_l,nrow=l_y)
for (i in 1:l_y){
  for (j in 1:l_l){
    SSB[i,j]=N[i,j+1]*wlen[j]*matlen[j]
    B[i,j]=N[i,j+1]*wlen[j]
  }
}

ssb_t=apply(SSB, 1,sum)
b_t=apply(B, 1,sum)

## Define cut! -----------------------------------------------------------------

vec ## select a value from here
cut_len=150

ind=which(vec==cut_len)
l_l=length(vec[1:ind])

SSB=matrix(0,ncol=l_l,nrow=l_y)
B=matrix(0,ncol=l_l,nrow=l_y)
for (i in 1:l_y){
  for (j in 1:l_l){
    SSB[i,j]=N[i,j+1]*wlen[j]*matlen[j]
    B[i,j]=N[i,j+1]*wlen[j]
  }
}

ssb_t_cut=apply(SSB, 1,sum)
b_t_cut=apply(B, 1,sum)

pdf(paste(plotdir, "/SSB cut.pdf", sep=""), height = 6.5, width = 11)
par(mfcol=c(2,2))
plot(year, b_t, type="l", ylab="B",xlab="Year",lty=2, main="Bio",col="black", lwd=1.8) 
lines( year, b_t_cut, col="orange",lty=1)
legend(x = "topright",  cex = 0.7,        # Position
       legend = c("B", paste("B<", cut_len, sep="")),  # Legend texts
       lty = c(2, 1),           # Line types
       col = c("black", "orange"),           # Line colors
       lwd = 2)

ylab=paste(paste("1-(B<", cut_len, sep=""),"/B)")
main=paste(paste("B proportion above ",cut_len,sep=""),"cm")

plot(year, 1-b_t_cut/b_t, type="l",ylim=c(0,1), 
     ylab=ylab,xlab="Year",lty=2, 
     main=main,col="black", lwd=1.8) 
max_b=max(b_t)
min_b=min(b_t_cut)

plot(year, ssb_t, type="l", ylim=c(min_b,max_b),
     ylab="SSB",xlab="Year",lty=2, 
     main="SSB",col="black", lwd=1.8) 
lines( year, ssb_t_cut, col="orange",lty=1)
legend(x = "topright",  cex = 0.7,        # Position
       legend = c("SSB", paste("SSB<", cut_len, sep="")),  # Legend texts
       lty = c(2, 1),           # Line types
       col = c("black", "orange"),           # Line colors
       lwd = 2)
ylab=paste(paste("1-(SSB<", cut_len, sep=""),"/SSB)")
main=paste(paste("SSB proportion above ",cut_len,sep=""),"cm")

plot(year, 1-ssb_t_cut/ssb_t, type="l",ylim=c(0,1), 
     ylab=ylab,xlab="Year",lty=2, main=main,col="black", lwd=1.8) 
dev.off()
