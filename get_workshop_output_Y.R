require(dplyr)
# Read data:
# directory where your models are:


# For 1A model: -----------------------------------------------------------
saveFolder = 'replicates_data'
load(file.path(saveFolder, 'Y_1A_25_1982_all_replicates.RData'))
max_grad = 5
all_years = 1952:2015

# Background info ---------------------------------------------------------

SS_ICES_team = 'SS_ICES'
SS_ICES_status = 'prelim'
SS_ICES_model_types = 'pan and spatial'
SS_ICES_notes = c('Configurations (pan and spatial): year.', 
                  'Runs without hessian so CV not reported.', 
                  'Movement rates reported for inmature and mature fish.')
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
SS_ICES_pan_nyrs = 64
SS_ICES_pan_flts = 7
SS_ICES_pan_flts_names = c('fishing_gi', 'fishing_hd', 'fishing_ll', 'fishing_other', 
                           'fishing_bb', 'fishing_ps', 'fishing_trol')

# -------------------------------------------------------------------------

# Process data for reporting 1A:
this_model = '1A_25_PY'
df_dq = save_data$dq[save_data$dq$em == this_model, ]
df_dq = df_dq[order(df_dq$iter),]
non_conv_runs = df_dq$iter[df_dq$grad > max_grad]

SS_ICES_pan_b0 = array(df_dq$B0, dim = c(1,SS_ICES_pan_nsims))
SS_ICES_pan_status_bio = array(df_dq$Bstatus, dim = c(1,SS_ICES_pan_nsims))
SS_ICES_pan_R0 = array(df_dq$R0, dim = c(1,SS_ICES_pan_nsims))
SS_ICES_pan_SSBmsy = array(df_dq$SSBmsy, dim = c(1,SS_ICES_pan_nsims))
SS_ICES_pan_Fmsy = array(df_dq$Fmsy, dim = c(1,SS_ICES_pan_nsims))
SS_ICES_pan_MSY = array(df_dq$MSY, dim = c(1,SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_b0[,non_conv_runs] = NA
SS_ICES_pan_status_bio[,non_conv_runs] = NA
SS_ICES_pan_R0[,non_conv_runs] = NA
SS_ICES_pan_SSBmsy[,non_conv_runs] = NA
SS_ICES_pan_Fmsy[,non_conv_runs] = NA
SS_ICES_pan_MSY[,non_conv_runs] = NA

df_ts = save_data$ts[save_data$ts$em == this_model, ]
df_ts = df_ts[order(df_ts$iter, df_ts$Yr),]

SS_ICES_pan_ssb = array(df_ts$SSB, dim = c(1,SS_ICES_pan_nyrs,1,SS_ICES_pan_nsims))
SS_ICES_pan_bio = array(df_ts$TotB, dim = c(1,SS_ICES_pan_nyrs,1,SS_ICES_pan_nsims))
SS_ICES_pan_rec = array(df_ts$Rec, dim = c(1,SS_ICES_pan_nyrs,1,SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_ssb[,,,non_conv_runs] = NA
SS_ICES_pan_bio[,,,non_conv_runs] = NA
SS_ICES_pan_rec[,,,non_conv_runs] = NA

df_fish = save_data$fish[save_data$fish$em == this_model, ]
df_fish = df_fish[order(df_fish$iter, df_fish$Yr),]
df_fish$Fleet = as.numeric(gsub(pattern = 'F:_', replacement = '', x = df_fish$Fleet))

SS_ICES_pan_F = array(df_fish$FishM, dim = c(1,SS_ICES_pan_flts,1,SS_ICES_pan_nyrs,1,SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_F[,,,,,non_conv_runs] = NA

df_catch = df_fish %>% dplyr::group_by(iter, Yr) %>% dplyr::summarise(Catch = sum(Catch))
SS_ICES_pan_catch = array(df_catch$Catch, dim = c(1,SS_ICES_pan_nyrs,1,SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_catch[,,,non_conv_runs] = NA

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Spatial model results ---------------------------------------------------

SS_ICES_spat_nsims = 3
SS_ICES_spat_rds_num = 1
SS_ICES_spat_nyrs = 256
SS_ICES_spat_nareas = 4
SS_ICES_spat_flts = array(c(7,3,1,5), dim = c(1, SS_ICES_spat_nareas))
SS_ICES_spat_flts_names = c('fishing_gi', 'fishing_hd', 'fishing_ll', 'fishing_other', 
                            'fishing_bb', 'fishing_ps', 'fishing_trol')
SS_ICES_spat_nstate = 2 # mature or inmature (movement rates)

# Process data for reporting 4A:
this_model = '4A_25_PY'
df_dq = save_data$dq[save_data$dq$em == this_model, ]
df_dq = df_dq[order(df_dq$iter, df_dq$Area),]
non_conv_runs = unique(df_dq$iter[df_dq$grad > max_grad])

SS_ICES_spat_b0 = array(df_dq$B0, dim = c(1,SS_ICES_spat_nareas,1, SS_ICES_spat_nsims))
SS_ICES_spat_status_bio = array(df_dq$Bstatus, dim = c(1,SS_ICES_spat_nareas,1, SS_ICES_spat_nsims))
SS_ICES_spat_R0 = array(df_dq$R0, dim = c(1,SS_ICES_spat_nareas,1, SS_ICES_spat_nsims))
SS_ICES_spat_SSBmsy = array(df_dq$SSBmsy, dim = c(1,SS_ICES_spat_nareas,1, SS_ICES_spat_nsims))
SS_ICES_spat_Fmsy = array(df_dq$Fmsy, dim = c(1,SS_ICES_spat_nareas,1, SS_ICES_spat_nsims))
SS_ICES_spat_MSY = array(df_dq$MSY, dim = c(1,SS_ICES_spat_nareas,1, SS_ICES_spat_nsims))
# NA for non convergent replicates:
SS_ICES_spat_b0[,,,non_conv_runs] = NA
SS_ICES_spat_status_bio[,,,non_conv_runs] = NA
SS_ICES_spat_R0[,,,non_conv_runs] = NA
SS_ICES_spat_SSBmsy[,,,non_conv_runs] = NA
SS_ICES_spat_Fmsy[,,,non_conv_runs] = NA
SS_ICES_spat_MSY[,,,non_conv_runs] = NA

df_ts = save_data$ts[save_data$ts$em == this_model, ]
df_ts = df_ts[order(df_ts$iter, df_ts$Yr),]

SS_ICES_spat_ssb = array(df_ts$SSB, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_spat_nyrs, 1, SS_ICES_spat_nsims))
SS_ICES_spat_bio = array(df_ts$TotB, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_spat_nyrs, 1, SS_ICES_spat_nsims))
SS_ICES_spat_rec = array(df_ts$Rec, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_spat_nyrs, 1, SS_ICES_spat_nsims))
# NA for non convergent replicates:
SS_ICES_spat_ssb[,,,,,non_conv_runs] = NA
SS_ICES_spat_bio[,,,,,non_conv_runs] = NA
SS_ICES_spat_rec[,,,,,non_conv_runs] = NA

df_recD = save_data$recD[save_data$recD$em == this_model, ]

SS_ICES_spat_recr_apport = array(0, dim = c(1,SS_ICES_spat_nsims,1,SS_ICES_spat_nareas))
for(k in 1:nrow(df_recD)) SS_ICES_spat_recr_apport[1,df_recD$iter[k],1,df_recD$Area[k]] = df_recD$`Frac/sex`[k]
# NA for non convergent replicates:
SS_ICES_spat_recr_apport[,non_conv_runs,,] = NA

df_mov = save_data$mov[save_data$mov$em == this_model, ]
df_mov = tidyr::gather(df_mov, 'state', 'mov', 5:6)
df_mov$state = ifelse(test = df_mov$state == 'inmatureRate', yes = 1, no = 2)
df_mov$area1 = as.numeric(substr(x = df_mov$movDef, start = 1, stop = 1))
df_mov$area2 = as.numeric(substr(x = df_mov$movDef, start = 6, stop = 6))

SS_ICES_spat_move = array(0, dim = c(1,SS_ICES_spat_nstate,1,SS_ICES_spat_nsims,1,SS_ICES_spat_nareas,1,SS_ICES_spat_nareas))
for(k in 1:nrow(df_mov)) SS_ICES_spat_move[1,df_mov$state[k],1,df_mov$iter[k],1,df_mov$area1[k],1,df_mov$area2[k]] = df_mov$mov[k]
# NA for non convergent replicates:
SS_ICES_spat_move[,,,non_conv_runs,,,,] = NA

df_fish = save_data$fish[save_data$fish$em == this_model, ]
df_fish = df_fish[order(df_fish$iter, df_fish$Yr),]
df_fish$Fleet = as.numeric(gsub(pattern = 'F:_', replacement = '', x = df_fish$Fleet))

SS_ICES_spat_F = array(df_fish$FishM, dim = c(1, SS_ICES_spat_nareas, 1,sum(SS_ICES_spat_flts),1,SS_ICES_spat_nyrs, 1, SS_ICES_spat_nsims))
# NA for non convergent replicates:
SS_ICES_spat_F[,,,,,,,non_conv_runs] = NA

df_catch = df_fish %>% dplyr::group_by(iter, Yr, Area) %>% dplyr::summarise(Catch = sum(Catch))
SS_ICES_spat_catch = array(df_catch$Catch, dim = c(1, SS_ICES_spat_nareas, 1, SS_ICES_spat_nyrs, 1, SS_ICES_spat_nsims))
# NA for non convergent replicates:
SS_ICES_spat_catch[,,,,,non_conv_runs] = NA
