# Clean workspace:
rm(list = ls())

# Set working directory
mainDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
setwd(mainDir)

# Libraries:
require(r4ss)

# -------------------------------------------------------------------------
# Main directory (model):
main_dir = '1area_25'
all_iter = list.files(path = main_dir)  


# -------------------------------------------------------------------------
# Loop to read results from all replicates:
for(j in seq_along(all_iter)) { 

  tmp_mod = SS_output(dir = file.path(main_dir, all_iter[1]), covar = FALSE) 
  # TODO: What variables should we analyze?
  
}

