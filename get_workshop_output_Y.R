rm(list = ls())
require(dplyr)

# Read data:
# directory where your models are:
saveFolder = 'replicates_data'
max_grad = 5
all_years = 1952:2015

# Background info ---------------------------------------------------------

SS_ICES_team = 'SS_ICES'
SS_ICES_status = 'final'
SS_ICES_model_types = 'pan and spatial'
SS_ICES_notes = c('Configuration (pan and spatial): year.', 
                  'Runs without hessian so CV not reported.', 
                  'Movement rates reported for inmature and mature fish.')
SS_ICES_ssb_units = 'weight'
SS_ICES_recr_units = '1000s of fish'
SS_ICES_F_units = 'instant apical F'
SS_ICES_catch_units = 'biomass'
SS_ICES_BRPS_calc = 'yes'
SS_ICES_b0_calc = 'yes'
SS_ICES_BRPs_type = 'Bmsy, Fmsy, MSY (global)'


# 1-area model results ----------------------------------------------------

SS_ICES_pan_nsims = 100
SS_ICES_pan_rds_num = 1
SS_ICES_pan_nyrs = 64
SS_ICES_pan_flts = 7
SS_ICES_pan_flts_names = c('fishing_gi', 'fishing_hd', 'fishing_ll', 'fishing_other', 
                           'fishing_bb', 'fishing_ps', 'fishing_trol')

# Spatial model results ---------------------------------------------------

SS_ICES_spat_nsims = 100
SS_ICES_spat_rds_num = 1
SS_ICES_spat_nyrs = 64
SS_ICES_spat_nareas = 4
SS_ICES_spat_flts = array(c(7,3,1,5), dim = c(SS_ICES_spat_nareas))
SS_ICES_spat_flts_names = c('fishing_gi', 'fishing_hd', 'fishing_ll', 'fishing_other', 
                           'fishing_bb', 'fishing_ps', 'fishing_trol')
SS_ICES_spat_nstate = 2 # mature/immature (movement rates)
  
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

load(file.path(saveFolder, 'Y_1A_25_1982_all_replicates.RData'))

# Process data for reporting 1A:
df_dq = save_data$dq
df_dq = df_dq %>% group_by(iter, Area) %>% summarise(B0 = mean(B0), Bstatus = mean(Bstatus), R0 = mean(R0), 
                                                     SSBmsy = mean(SSBmsy), Fmsy = mean(Fmsy), MSY = mean(MSY),
                                                     grad = mean(grad))
df_dq = df_dq[order(df_dq$iter),]
non_conv_runs = df_dq$iter[df_dq$grad > max_grad]

SS_ICES_pan_b0 = array(df_dq$B0, dim = c(SS_ICES_pan_nsims))
SS_ICES_pan_status_bio = array(df_dq$Bstatus, dim = c(SS_ICES_pan_nsims))
SS_ICES_pan_R0 = array(df_dq$R0, dim = c(SS_ICES_pan_nsims))
SS_ICES_pan_SSBmsy = array(df_dq$SSBmsy, dim = c(SS_ICES_pan_nsims))
SS_ICES_pan_Fmsy = array(df_dq$Fmsy, dim = c(SS_ICES_pan_nsims))
SS_ICES_pan_MSY = array(df_dq$MSY, dim = c(SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_b0[non_conv_runs] = NA
SS_ICES_pan_status_bio[non_conv_runs] = NA
SS_ICES_pan_R0[non_conv_runs] = NA
SS_ICES_pan_SSBmsy[non_conv_runs] = NA
SS_ICES_pan_Fmsy[non_conv_runs] = NA
SS_ICES_pan_MSY[non_conv_runs] = NA

df_ts = save_data$ts
df_ts = df_ts %>% group_by(iter, Area, Yr) %>% summarise(SSB = mean(SSB,na.rm=TRUE), TotB = mean(TotB), Rec = mean(Rec))
df_ts = df_ts[order(df_ts$iter, df_ts$Yr),]

SS_ICES_pan_ssb = array(df_ts$SSB, dim = c(SS_ICES_pan_nyrs,SS_ICES_pan_nsims))
SS_ICES_pan_bio = array(df_ts$TotB, dim = c(SS_ICES_pan_nyrs,SS_ICES_pan_nsims))
SS_ICES_pan_rec = array(df_ts$Rec, dim = c(SS_ICES_pan_nyrs,SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_ssb[,non_conv_runs] = NA
SS_ICES_pan_bio[,non_conv_runs] = NA
SS_ICES_pan_rec[,non_conv_runs] = NA

df_fish = save_data$fish
df_fish = df_fish %>% group_by(iter, Yr, Fleet) %>% summarise(FishM = mean(FishM), Catch = sum(Catch))
df_fish = df_fish[order(df_fish$iter, df_fish$Yr),]
df_fish$Fleet = as.numeric(gsub(pattern = 'F:_', replacement = '', x = df_fish$Fleet))

SS_ICES_pan_F = array(df_fish$FishM, dim = c(SS_ICES_pan_flts,SS_ICES_pan_nyrs,SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_F[,,non_conv_runs] = NA

df_catch = df_fish %>% dplyr::group_by(iter, Yr) %>% dplyr::summarise(Catch = sum(Catch))
SS_ICES_pan_catch = array(df_catch$Catch, dim = c(SS_ICES_pan_nyrs,SS_ICES_pan_nsims))
# NA for non convergent replicates:
SS_ICES_pan_catch[,non_conv_runs] = NA

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
load(file.path(saveFolder, 'Y_4A_25_base_all_replicates.RData'))

# Process data for reporting 4A:
df_dq = save_data$dq
df_dq = df_dq %>% group_by(iter, Area) %>% summarise(B0 = mean(B0), Bstatus = mean(Bstatus), R0 = mean(R0), 
                                                     SSBmsy = mean(SSBmsy), Fmsy = mean(Fmsy), MSY = mean(MSY),
                                                     grad = mean(grad))
df_dq = df_dq[order(df_dq$iter, df_dq$Area),]
non_conv_runs = unique(df_dq$iter[df_dq$grad > max_grad])

SS_ICES_spat_b0 = array(df_dq$B0, dim = c(SS_ICES_spat_nareas,SS_ICES_spat_nsims))
SS_ICES_spat_status_bio = array(df_dq$Bstatus, dim = c(SS_ICES_spat_nareas,SS_ICES_spat_nsims))
SS_ICES_spat_R0 = array(df_dq$R0, dim = c(SS_ICES_spat_nareas,SS_ICES_spat_nsims))
tmp = df_dq %>% group_by(iter) %>% summarise(SSBmsy = mean(SSBmsy))
SS_ICES_spat_SSBmsy = array(tmp$SSBmsy, dim = c(SS_ICES_spat_nsims))
tmp = df_dq %>% group_by(iter) %>% summarise(Fmsy = mean(Fmsy))
SS_ICES_spat_Fmsy = array(tmp$Fmsy, dim = c(SS_ICES_spat_nsims))
tmp = df_dq %>% group_by(iter) %>% summarise(MSY = mean(MSY))
SS_ICES_spat_MSY = array(tmp$MSY, dim = c(SS_ICES_spat_nsims))
# NA for non convergent replicates:
SS_ICES_spat_b0[,non_conv_runs] = NA
SS_ICES_spat_status_bio[,non_conv_runs] = NA
SS_ICES_spat_R0[,non_conv_runs] = NA
SS_ICES_spat_SSBmsy[non_conv_runs] = NA
SS_ICES_spat_Fmsy[non_conv_runs] = NA
SS_ICES_spat_MSY[non_conv_runs] = NA

df_ts = save_data$ts
df_ts = df_ts %>% group_by(iter, Area, Yr) %>% summarise(SSB = mean(SSB,na.rm=TRUE), TotB = mean(TotB), Rec = mean(Rec))
df_ts = df_ts[order(df_ts$iter, df_ts$Yr,df_ts$Area),]

SS_ICES_spat_ssb = array(df_ts$SSB, dim = c(SS_ICES_spat_nareas,SS_ICES_spat_nyrs,SS_ICES_spat_nsims))
SS_ICES_spat_bio = array(df_ts$TotB, dim = c(SS_ICES_spat_nareas,SS_ICES_spat_nyrs,SS_ICES_spat_nsims))
SS_ICES_spat_rec = array(df_ts$Rec, dim = c(SS_ICES_spat_nareas,SS_ICES_spat_nyrs,SS_ICES_spat_nsims))
# NA for non convergent replicates:
SS_ICES_spat_ssb[,,non_conv_runs] = NA
SS_ICES_spat_bio[,,non_conv_runs] = NA
SS_ICES_spat_rec[,,non_conv_runs] = NA

df_recD = save_data$recD
df_recD = df_recD[order(df_recD$Area, df_recD$iter),]

SS_ICES_spat_recr_apport = array(df_recD$`Frac/sex`, dim = c(SS_ICES_spat_nsims,SS_ICES_spat_nareas))
# NA for non convergent replicates:
SS_ICES_spat_recr_apport[non_conv_runs,] = NA

df_mov = save_data$mov
df_mov = tidyr::gather(df_mov, 'state', 'mov', 6:7)
df_mov$state = ifelse(test = df_mov$state == 'inmatureRate', yes = 1, no = 2)

SS_ICES_spat_move = array(0, dim = c(SS_ICES_spat_nstate,SS_ICES_spat_nsims,SS_ICES_spat_nareas,SS_ICES_spat_nareas))
for(k in 1:nrow(df_mov)) SS_ICES_spat_move[df_mov$state[k],df_mov$iter[k],df_mov$area1[k],df_mov$area2[k]] = df_mov$mov[k]
# NA for non convergent replicates:
SS_ICES_spat_move[,non_conv_runs,,] = NA

df_fish = save_data$fish
df_fish = df_fish %>% group_by(iter, Yr, Fleet, Area) %>% summarise(FishM = mean(FishM), Catch = sum(Catch))
df_fish$Fleet = as.numeric(gsub(pattern = 'F:_', replacement = '', x = df_fish$Fleet))

SS_ICES_spat_F = array(0, dim = c(SS_ICES_spat_nareas,sum(SS_ICES_spat_flts),SS_ICES_spat_nyrs, SS_ICES_spat_nsims))
for(k in 1:nrow(df_fish)) SS_ICES_spat_F[df_fish$Area[k],df_fish$Fleet[k],match(df_fish$Yr[k], all_years),df_fish$iter[k]] = df_fish$FishM[k]
# NA for non convergent replicates:
SS_ICES_spat_F[,,,non_conv_runs] = NA

df_catch = df_fish %>% dplyr::group_by(iter, Yr, Area) %>% dplyr::summarise(Catch = sum(Catch))
df_catch = df_catch[order(df_catch$iter, df_catch$Yr, df_catch$Area),]

SS_ICES_spat_catch = array(df_catch$Catch, dim = c(SS_ICES_spat_nareas,SS_ICES_spat_nyrs,SS_ICES_spat_nsims))
# NA for non convergent replicates:
SS_ICES_spat_catch[,,non_conv_runs] = NA


# -------------------------------------------------------------------------
# SAVE DATA:

save(SS_ICES_team,SS_ICES_status,SS_ICES_model_types,SS_ICES_notes,SS_ICES_ssb_units,SS_ICES_recr_units,
     SS_ICES_F_units,SS_ICES_catch_units,SS_ICES_BRPS_calc,SS_ICES_b0_calc,SS_ICES_BRPs_type,
     SS_ICES_pan_nsims,SS_ICES_pan_rds_num,SS_ICES_pan_nyrs,SS_ICES_pan_flts,SS_ICES_pan_flts_names,
     SS_ICES_spat_nsims,SS_ICES_spat_rds_num,SS_ICES_spat_nyrs,SS_ICES_spat_nareas,SS_ICES_spat_flts,
     SS_ICES_spat_flts_names,SS_ICES_spat_nstate,SS_ICES_pan_b0,SS_ICES_pan_status_bio,SS_ICES_pan_R0,
     SS_ICES_pan_SSBmsy,SS_ICES_pan_Fmsy,SS_ICES_pan_MSY,SS_ICES_pan_ssb,SS_ICES_pan_bio,SS_ICES_pan_rec,
     SS_ICES_pan_F,SS_ICES_pan_catch,SS_ICES_spat_b0,SS_ICES_spat_status_bio,SS_ICES_spat_R0,SS_ICES_spat_SSBmsy,
     SS_ICES_spat_Fmsy,SS_ICES_spat_MSY,SS_ICES_spat_ssb,SS_ICES_spat_bio,SS_ICES_spat_rec,
     SS_ICES_spat_recr_apport,SS_ICES_spat_move,SS_ICES_spat_F,SS_ICES_spat_catch, file = 'SS_ICES_Y_wkshp.RData')

