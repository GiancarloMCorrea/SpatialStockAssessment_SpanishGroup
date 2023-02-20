
## Libraries ---------------------------------------------------
rm(list = ls())
library(r4ss)
require(stringr)
require(dplyr)

# Set working directory to source file location
# This is the directory where you have your models:
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
saveFolder = 'replicates_data' # folder to save produced RData

# -------------------------------------------------------------------------
# Main directories (model configurations to read):
dir_mods = c('1A_25_ageS_PY_CPUEst_tags', '4A_25_ageS_PY_CPUEst_tags_move')
label_mods = c('1A_25_PY', '4A_25_PY') # labels shown in plots

# -------------------------------------------------------------------------
# Loop to read results from all replicates:
ts_tmp = list()
cList = 1
for(j in seq_along(dir_mods)) { 
  
  mod_path = paste0(saveDir, '/', dir_mods[j])
  all_iter = list.files(path = mod_path)  
  
  for(k in seq_along(all_iter)) {
    iter_name = as.numeric(str_split(string = all_iter[k] , pattern = '_', simplify = TRUE)[1,3])
    tmp_mod = SS_output(dir = file.path(mod_path, all_iter[k]), covar = FALSE, verbose = FALSE,
                        printstats = FALSE) 
    if(tmp_mod$nareas > 1) rec_dist_vec = tmp_mod$recruitment_dist$recruit_dist$`Frac/sex` # needs to change when R dist is time varying
    SSB_info = tmp_mod$derived_quants[grep(pattern = 'SSB_', x = tmp_mod$derived_quants$Label)[3:258], ]
    F_info = tmp_mod$derived_quants[grep(pattern = 'F_', x = tmp_mod$derived_quants$Label)[1:256], ]
    R_info = tmp_mod$derived_quants[grep(pattern = 'Recr_', x = tmp_mod$derived_quants$Label)[3:258], ]
    tmp_df = data.frame(iter = iter_name, em = dir_mods[j],
                        year = 1001:1256, SSB = SSB_info$Value, R_val = R_info$Value,
                        Fval = F_info$Value, 
                        grad = tmp_mod$maximum_gradient_component)
    if(tmp_mod$nareas > 1) { tmp_df = cbind(tmp_df, data.frame(R1=rec_dist_vec[1], R2=rec_dist_vec[2],
                                                             R3=rec_dist_vec[3], R4=rec_dist_vec[4])) }
    ts_tmp[[cList]] = tmp_df
    cList = cList+1
    print(k)
  }
  
}

ts_data0 = dplyr::bind_rows(ts_tmp)
ts_data0$model_name = factor(ts_data0$em, levels = dir_mods, labels = label_mods)
save(ts_data0, file = file.path(saveFolder, 'PY_all.RData'))
