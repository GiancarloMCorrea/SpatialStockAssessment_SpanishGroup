# Clean workspace:
rm(list = ls())

# Set working directory:

# directory to save models (local disk)
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'

# Libraries:
require(r4ss)
require(ss3diags)
library(icesAdvice)

### Model path
type_model <- '4A_25_ageS_PY_CPUEst_tags_R124_moveType2_RDTime' ## *CHANGE name
subfolder = 'dat_4A_1'
mod_path = paste0(saveDir, '/', file.path(type_model, subfolder))

## Retros for directories
plotdir_retro<- paste(mod_path, "/retros", sep="")
dir.create(plotdir_retro)

# Do retros --------------------------------------------------------------------

## Retros analysis by creating new directories, copying model files, and 
## changing the starter file to set the number of years of data to exclude

## SS.exe must be in the model folder

yper = -1:-5 ## years period for retros

r4ss::retro(dir=mod_path, oldsubdir="", newsubdir = "retros", extras="-nohess",
           subdirstart = "retro",years = yper, overwrite = TRUE, exe = "ss")

retroModels <- SSgetoutput(dirvec=file.path(mod_path, "retros",
                                            paste("retro",yper,sep="")))

save(retroModels, file=paste0(mod_path, 
                              "/retros/retroModels.RData", sep=""))

retroSummary <- SSsummarize(retroModels) # retro 0 is replist 1


endyrvec <- retroSummary$endyrs + yper

# R4ss plots ------------------------------------------------------------------

## Comparison plot pdf
SSplotComparisons(retroSummary, endyrvec=endyrvec, xlim=c(1121,1256), 
                  legendlabels=paste("Data",yper,"years"), print=FALSE, pdf=TRUE, 
                  plotdir = plotdir_retro)
## Retro recruits
SSplotRetroRecruits(retroSummary, endyrvec=endyrvec, cohorts=1125:1256,
                    relative=TRUE, legend=FALSE)
dev.off()

# Plot biomass:
plot_name = '4A_25_retro'
SSplotComparisons(retroSummary, subplot = 1, legendlabels=paste("Data",yper,"years"), png = TRUE, 
                  plotdir = 'figures', filenameprefix = plot_name)

## Summarize the list of retroModels
retroSummary <- r4ss::SSsummarize(retroModels)
# 
# ## Now Check retros Analysis with one-step ahead Forecasts
sspar(mfrow=c(2,1),plot.cex = 0.9)
SSplotRetro(retroSummary,forecast = F,add=T)
SSplotRetro(retroSummary,forecast = F,add=T, xmin=1985)
dev.print(jpeg,paste0(mod_path,"/Retroforecast_.jpg"), width = 8,
          height = 9, res = 300, units = "in")

## Do Hindcast with Cross-Validation of CPUE observations
png(paste0(mod_path,"/HCxvalIndex.png"), height = 140, width = 190, units = 'mm', res = 500)
sspar(mfrow=c(2,2),plot.cex = 0.9)
SSplotHCxval(retroSummary,xmin=1972,add=T)
dev.off()
## Also test new feature of Hindcast with Cross-Validation for mean length

## Use new converter fuction SSretroComps()
hccomps = SSretroComps(retroModels)
png(paste0(mod_path,"/HCxvalLen.png"), height = 140, width = 190, units = 'mm', res = 500)
sspar(mfrow=c(2,2),plot.cex = 0.7)
SSplotHCxval(hccomps,add=T,subplots = "len",legendloc="topleft")
#dev.print(jpeg,paste0(dirplot,"/HCxvalLen_.jpg"), width = 8,
#         height = 4, res = 300, units = "in")
dev.off()
