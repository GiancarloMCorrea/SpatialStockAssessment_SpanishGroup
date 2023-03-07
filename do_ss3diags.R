# Clean workspace:
rm(list = ls())

# Set working directory:

# directory to save models (local disk)
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'

# Libraries:
require(r4ss)
require(ss3diags)

### Model path
type_model <- '1A_25_ageS_PY_CPUEst_tags' ## *CHANGE name
subfolder = 'dat_1A_1'
mod_path = paste0(saveDir, '/', file.path(type_model, subfolder))

# Read model:
ss3rep = SS_output(dir = mod_path, covar=TRUE, verbose = FALSE, printstats = FALSE)

# ss3diags ---------------------------------------------------------------------

## Check Data
sspar()
SSplotData(ss3rep,subplot = 2)
dev.print(jpeg, paste0("figures/", type_model, "_diags1.jpg"), width = 8, 
          height = 6, res = 300, units = "in")

## For cpue
if(subfolder == 'dat_1A_1') sspar(mfrow=c(1,1),plot.cex = 0.8)
if(subfolder == 'dat_4A_1') sspar(mfrow=c(2,2),plot.cex = 0.8)
SSplotRunstest(ss3rep,subplots="cpue",add=T)
dev.print(jpeg, paste0("figures/", type_model, "_diags2.jpg"), 
          width = 8, height = 7, res = 300, units = "in")
dev.off()

## For length
sspar(mfrow=c(4,4),plot.cex = 0.8)
SSplotRunstest(ss3rep,subplots="len",add=T)
dev.print(jpeg,paste0("figures/", type_model, "_diags3.jpg"), 
          width = 8, height = 7, res = 300, units = "in")

## Check conflict between mean lengths
sspar(mfrow=c(1,2),plot.cex = 0.8)
SSplotJABBAres(ss3rep,subplots="cpue",add=T)
SSplotJABBAres(ss3rep,subplots="len",add=T)
dev.print(jpeg,paste0("figures/", type_model, "_diags4.jpg"), 
          width = 8, height = 3.5, res = 300, units = "in")
dev.off()


# -------------------------------------------------------------------------

## Check starter file
starter = SSsettingsBratioF(ss3rep)

## Get uncertainty from MVLN for F/F_Btrg with original F setting F_abs
sspar(mfrow=c(1,1),plot.cex = 0.9)
mvn = SSdeltaMVLN(ss3rep,plot = T,years=1081:1256)
mvn$labels # the out put is SB/SBtrg
dev.print(jpeg, paste0("figures/", type_model, "_kobe.jpg"), 
          width = 6.5, height = 6.5, res = 300, units = "in")

sspar(mfrow=c(3,2),plot.cex = 0.9)
SSplotEnsemble(mvn$kb,ylabs = mvn$labels,add=T)
dev.print(jpeg,paste0("figures/", type_model, "_MVLN.jpg"), width = 8, 
          height = 6.5, res = 300, units = "in")


