# Read data:
# directory where your models are:
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
saveFolder = 'replicates_data'
load(file.path(saveFolder, 'PY_all.RData'))

# Create output file for workshop -----------------------------------------

SS_ICES_wkshp = list()

SS_ICES_team = 'SS_ICES'
SS_ICES_status = 'prelim'
SS_ICES_model_types = 'pan and spatial'
SS_ICES_notes = ''
SS_ICES_ssb_units = 'weight'
SS_ICES_recr_units = '1000s of fish'
SS_ICES_F_units = 'instant apical F'
SS_ICES_catch_units = 'numbers'
SS_ICES_BRPS_calc = 'yes'
SS_ICES_b0_calc = 'yes'
SS_ICES_BRPs_type = ''


SS_ICES_pan_nsims = 100
SS_ICES_pan_rds_num = 1
SS_ICES_pan_nyrs = c(64, 256)
SS_ICES_pan_flts = 7
SS_ICES_pan_flts_names = c('fishing_gi', 'fishing_hd', 'fishing_ll', 'fishing_other', 
                       'fishing_bb', 'fishing_ps', 'fishing_trol')
SS_ICES_pan_b0 = 








