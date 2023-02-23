# Read data:
# directory where your models are:
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
saveFolder = 'replicates_data'
load(file.path(saveFolder, 'PY_all.RData'))

# Create output file for workshop -----------------------------------------


# Background info ---------------------------------------------------------

SS_ICES_team = 'SS_ICES'
SS_ICES_status = 'prelim'
SS_ICES_model_types = 'pan and spatial'
SS_ICES_notes = 'two configurations (pan and spatial): year and pseudoyear. Runs without hessian so CV not reported. Only convergent runs are reported. '
SS_ICES_ssb_units = 'weight'
SS_ICES_recr_units = '1000s of fish'
SS_ICES_F_units = 'instant apical F'
SS_ICES_catch_units = 'numbers'
SS_ICES_BRPS_calc = 'yes'
SS_ICES_b0_calc = 'yes'
SS_ICES_BRPs_type = 'depletion, Bmsy, Fmsy, MSY'


# 1-area model results ----------------------------------------------------

SS_ICES_pan_nsims = 100
SS_ICES_pan_rds_num = 1
SS_ICES_pan_nyrs = 256
SS_ICES_pan_flts = 7
SS_ICES_pan_flts_names = c('fishing_gi', 'fishing_hd', 'fishing_ll', 'fishing_other', 
                           'fishing_bb', 'fishing_ps', 'fishing_trol')
SS_ICES_pan_b0 = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_pan_status_bio = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_pan_R0 = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_pan_brp = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_pan_ssb = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_ssb_CV = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_bio = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_bio_CV = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_rec = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_rec_CV = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_F = array(data = NA, dim = c(1,SS_ICES_pan_flts,1,SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_F_CV = array(data = NA, dim = c(1,SS_ICES_pan_flts,1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_pan_catch = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))


# Spatial model results ---------------------------------------------------

SS_ICES_spat_nsims = 100
SS_ICES_spat_rds_num = 1
SS_ICES_spat_nyrs = 256
SS_ICES_spat_nareas = 4
SS_ICES_spat_flts = array(data = NA, dim = c(1, SS_ICES_spat_nareas))
SS_ICES_spat_flts_names = c('fishing_gi', 'fishing_hd', 'fishing_ll', 'fishing_other', 
                           'fishing_bb', 'fishing_ps', 'fishing_trol')
SS_ICES_spat_b0 = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_spat_status_bio = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_spat_R0 = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_spat_brp = array(data = NA, dim = c(1, SS_ICES_pan_nsims))
SS_ICES_spat_ssb = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_ssb_CV = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_bio = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_bio_CV = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_rec = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_rec_CV = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_recr_apport = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims, 1, SS_ICES_spat_nareas))
SS_ICES_spat_move = array(data = NA, dim = c(1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims, 1, SS_ICES_spat_nareas, 1, SS_ICES_spat_nareas))
SS_ICES_spat_F = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1,SS_ICES_pan_flts,1,SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_F_CV = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1,SS_ICES_pan_flts,1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))
SS_ICES_spat_catch = array(data = NA, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_pan_nyrs, 1, SS_ICES_pan_nsims))







