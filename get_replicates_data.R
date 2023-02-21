
## Libraries ---------------------------------------------------
rm(list = ls())
library(r4ss)
require(stringr)
require(dplyr)

# Set working directory to source file location

# This is the directory where you have your models:
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
saveFolder = 'replicates_data' # folder to save produced RData from this script

# -------------------------------------------------------------------------
# Main directories (model configurations to read):
dir_mods = c('1A_25_ageS_PY_CPUEst_tags', '4A_25_ageS_PY_CPUEst_tags_moveType1')
label_mods = c('1A_25_PY', '4A_25_PY') # labels shown in plots

# -------------------------------------------------------------------------
# Loop to read results from all replicates:
dq_all = list()
ts_all = list()
fish_all = list()
recD_all = list()
mov_all = list()

cList = 1
for(j in seq_along(dir_mods)) { 
  
  mod_path = paste0(saveDir, '/', dir_mods[j])
  all_iter = list.files(path = mod_path)  
  
  for(k in seq_along(all_iter)) {
    iter_name = as.numeric(str_split(string = all_iter[k] , pattern = '_', simplify = TRUE)[1,3])
    tmp_mod = SS_output(dir = file.path(mod_path, all_iter[k]), covar = FALSE, verbose = FALSE,
                        printstats = FALSE) 
    # derived quantities --------------------
    B0 = mean(tmp_mod$timeseries$Bio_all[tmp_mod$timeseries$Era == 'VIRG'])
    Bstatus = mean(tmp_mod$timeseries$Bio_all[tmp_mod$timeseries$Yr == tmp_mod$endyr])/B0
    R0 = mean(tmp_mod$timeseries$Recruit_0[tmp_mod$timeseries$Era == 'VIRG'])
    # Another reference points?
    dq_df = data.frame(iter = iter_name, em = label_mods[j], B0 = B0, Bstatus = Bstatus, 
                       R0 = R0, grad = tmp_mod$maximum_gradient_component)
    # Time series --------------------------
    thisYear = tmp_mod$timeseries$Yr %in% tmp_mod$startyr:tmp_mod$endyr
    SSB = tmp_mod$timeseries$SpawnBio[thisYear]
    TotB = tmp_mod$timeseries$Bio_all[thisYear]
    Rec = tmp_mod$timeseries$Recruit_0[thisYear]
    ts_df = data.frame(iter = iter_name, em = label_mods[j], Area = tmp_mod$timeseries$Area[thisYear],
                       Yr = tmp_mod$timeseries$Yr[thisYear], Seas = tmp_mod$timeseries$Seas[thisYear],
                       SSB = SSB, TotB = TotB, Rec = Rec)
    # Fishing mortality and catch ----------
    prevF = tmp_mod$timeseries[thisYear, c(2, 4, grep(pattern = 'F:_', x = colnames(tmp_mod$timeseries)))]
    fish_df = tidyr::gather(prevF, 'Fleet', 'FishM', 3:ncol(prevF))
    prevCatch = tmp_mod$timeseries[thisYear, grep(pattern = 'dead\\(N\\)', x = colnames(tmp_mod$timeseries))]
    fish_df = cbind(fish_df, tidyr::gather(prevCatch, 'Fleet2', 'Catch', 1:ncol(prevCatch)))
    fish_df$iter = iter_name
    fish_df$em = label_mods[j]
    
    # Save results:
    dq_all[[cList]] = dq_df
    ts_all[[cList]] = ts_df
    fish_all[[cList]] = fish_df
    
    # Recruitment app and movement rates (only for 4A model):
    if(tmp_mod$nareas > 1) {
      recD_df = tmp_mod$recruitment_dist$recruit_dist[,c(4,6,9)] # needs to change when R dist is time varying
      recD_df$iter = iter_name
      recD_df$em = label_mods[j]
      recD_all[[cList]] = recD_df
    }
    if(!is.null(tmp_mod$movement)) {
      prevMov = tmp_mod$movement[tmp_mod$movement$age0 == 0, ] # age0 == 0 also in year configuration?
      movDef = paste0(prevMov$Source_area,' to ',prevMov$Dest_area)
      inmatureRate = prevMov[,grep(pattern = paste0('age', prevMov$minage[1]), x = colnames(prevMov))]
      matureRate = prevMov[,grep(pattern = paste0('age', prevMov$maxage[1]), x = colnames(prevMov))]
      mov_df = data.frame(iter = iter_name, em = label_mods[j], Seas = prevMov$Seas,
                          movDef = movDef, inmatureRate = inmatureRate, matureRate = matureRate)
      mov_all[[cList]] = mov_df
    }
    
    cList = cList+1
    print(k)
  }
  
}

# Merge lists
save_data = list()
save_data$dq = dplyr::bind_rows(dq_all)
save_data$ts = dplyr::bind_rows(ts_all)
save_data$fish = dplyr::bind_rows(fish_all)
save_data$recD = dplyr::bind_rows(recD_all)
save_data$mov = dplyr::bind_rows(mov_all)

# Save RData
save(save_data, file = file.path(saveFolder, 'PY_all.RData'))
