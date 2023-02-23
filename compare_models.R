# Clean workspace:
rm(list = ls())

# Set working directory:

# directory to save models (local disk)
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'

# Libraries:
require(r4ss)
require(ss3diags)

# Model information:
type_model = '4A_25_ageS_PY_CPUEst_tags_moveType1'
subfolder = 'dat_4A_4'
selex_type = 2 #1 = length, 2 = age

# Path name:
mod_path = paste0(saveDir, '/', file.path(type_model, subfolder))
  
# Read report:
replist = SS_output(dir = mod_path, covar=TRUE, verbose = FALSE, printstats = FALSE)

## Plot
SS_plots(replist) ## html output

## Summary plot
png(file=paste(mod_path,"/a_SSplot.png",sep=""), width = 300, height = 200, units = 'mm', res = 500)
par(mfrow=c(3,3))
SSplotCatch(replist = replist, subplots=1) ; title("Landings")
SSplotSummaryF(replist); title("F")
SSplotBiology(replist,subplots = 1)
SSplotRecdevs(replist, subplots=1);title("recdevs")
SSplotBiology(replist,subplots = 6); title("Mat")
SSplotSelex(replist, subplot = selex_type)
SSplotTimeseries(replist, subplot = 7); title("Biomass")
SSplotTimeseries(replist, subplot = 7, minyr = 1121, maxyr = 1256); title("Biomass recent")
dev.off()

## Typical summary tables
replist$likelihoods_used
replist$RunTime
replist$likelihoods_by_fleet
write.csv(replist$likelihoods_used, file=paste0(mod_path, "/likelihood-", subfolder, ".csv", sep=""))

## Create output tables
SSbiologytables(replist)
SSexecutivesummary(replist)


# -------------------------------------------------------------------------
# Compare several models:

# Models to compare (age selex 1 Area): only models that converged
#type_model = c('1A_25_ageS_PY', '1A_25_ageS_PY_CPUEst')
#type_model = c('1A_25_lenS_PY', '1A_25_lenS_PY_CPUEst', '1A_25_lenS_PY_CPUEst_tags', '1A_25_lenS_PY_tags')
type_model = c('4A_25_ageS_PY_CPUEst_tags_moveType2', '4A_25_ageS_PY_CPUEst_tags_R123_moveType2', '4A_25_ageS_PY_CPUEst_tags_R124_moveType2')
subfolder = rep('dat_4A_2', times = length(type_model))
plot_name = '4A_25'

all_models = SSgetoutput(dirvec = paste0(saveDir, '/', file.path(type_model, subfolder)))
summary_models = SSsummarize(biglist = all_models)
summary_models$maxgrad
summary_models$likelihoods
# Make plots:
SSplotComparisons(summaryoutput = summary_models, subplots = 1, legendlabels = type_model, print = TRUE, 
                  plotdir = 'figures', filenameprefix = plot_name)

SSplotComparisons(summaryoutput = summary_models, subplots = 7, legendlabels = type_model, print = TRUE, 
                  plotdir = 'figures', filenameprefix = plot_name)

SSplotComparisons(summaryoutput = summary_models, subplots = 9, legendlabels = type_model, print = TRUE, 
                  plotdir = 'figures', filenameprefix = plot_name)

