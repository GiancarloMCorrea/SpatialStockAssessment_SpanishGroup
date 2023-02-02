
get_initial_files = function(sim_dat, ctl, dat, selex_len, selex_age, selex_df, Qphase = 3, use_tags = TRUE,
                              N_moveDef = 0, RecDistPhase = -4, RecDistInit = 0, MovPhase = -4, 
                              source_area = NULL, dest_area = NULL, RecDistTemp = FALSE) {

  # INIT INFO:
  
  OutDataFile = dat
  OutControlFile = ctl
  myDataFile = sim_dat
  
  # DATA FILE :
  
  OutDataFile$N_areas = myDataFile$N_areas
  OutDataFile$Nfleets = myDataFile$Nfleet + myDataFile$Nsurveys
  OutDataFile$fleetinfo = data.frame(type = c(rep(1, times = myDataFile$Nfleet), rep(3, times = myDataFile$Nsurveys)), 
                                     surveytiming = c(rep(-1, times = myDataFile$Nfleet), rep(7, times = myDataFile$Nsurveys)),
                                     area = as.vector(as.matrix(myDataFile$fleetinfo1[2,])),
                                     units = 2, 
                                     need_catch_mult = 0,
                                     fleetname = myDataFile$fleetnames)
  OutDataFile$fleetnames = myDataFile$fleetnames
  OutDataFile$surveytiming = c(rep(-1, times = myDataFile$Nfleet), rep(7, times = myDataFile$Nsurveys))
  OutDataFile$units_of_catch = rep(2, times = OutDataFile$Nfleets)
  OutDataFile$areas = as.vector(as.matrix(myDataFile$fleetinfo1[2,]))
  #Catch data:
  catch_330 = data.frame(year = 1000 + rep(myDataFile$styr:myDataFile$endyr, times = myDataFile$Nfleet), 
                         seas = 1, 
                         fleet = rep(1:myDataFile$Nfleet, each = myDataFile$endyr))
  CatchVec = NULL
  for(i in 1:myDataFile$Nfleet) {
    CatchVec = c(CatchVec, myDataFile$catch[,i])
  }
  catch_330$catch = CatchVec
  catch_330$catch_se = rep(myDataFile$se_log_catch, each =  myDataFile$endyr)
  catch_330 = rbind(data.frame(year = -999, seas = 1, fleet = 1:myDataFile$Nfleet, catch = 0, catch_se = myDataFile$se_log_catch),
                    catch_330)
  OutDataFile$catch = catch_330
  #continue..
  OutDataFile$CPUEinfo = data.frame(Fleet = myDataFile$CPUEinfo$Fleet, Units = 0, Errtype = myDataFile$CPUEinfo$Errtype,
                                    SD_Report = 0)
  rownames(OutDataFile$CPUEinfo) = myDataFile$fleetnames
  #CPUE data:
  index_vec = myDataFile$CPUE$index
  if(myDataFile$N_areas == 1) index_vec = 8 # one area
  cpue_330 = data.frame(year = 1000 + as.numeric(as.character(myDataFile$CPUE$year)), 
                        seas = 7, 
                        index = index_vec,
                        obs = as.numeric(as.character(myDataFile$CPUE$cpu)), 
                        se_log = as.numeric(as.character(myDataFile$CPUE$cv)))
  OutDataFile$CPUE = cpue_330
  #continue...
  OutDataFile$len_info = data.frame(mintailcomp = -0.001, addtocomp = myDataFile$add_to_comp, 
                                    combine_M_F = 0, CompressBins = 0, CompError = 0, ParmSelect = 0, #1:OutDataFile$Nfleets, # Dirichlet
                                    minsamplesize = rep(0.01, times = OutDataFile$Nfleets))
  rownames(OutDataFile$len_info) = OutDataFile$fleetnames
  OutDataFile$lencomp = myDataFile$lencomp
  OutDataFile$lencomp$Yr = 1000 + OutDataFile$lencomp$Yr
  OutDataFile$lencomp$Seas = 7 # should be 7 or 1?
  OutDataFile$Nfleet = myDataFile$Nfleet
  OutDataFile$Nsurveys = myDataFile$Nsurveys
  OutDataFile$fleetinfo1 = t(OutDataFile$fleetinfo[,c('surveytiming', 'area', 'type')])
  OutDataFile$fleetinfo2 = t(data.frame(units = OutDataFile$units_of_catch, need_catch_mult = 0))
  OutDataFile$max_combined_lbin = rep(myDataFile$max_combined_lbin, times = OutDataFile$Nfleets)
  
  #if(myDataFile$do_tags == 1 & myDataFile$N_areas > 1) {
  if(use_tags) {
    
    OutDataFile$do_tags = myDataFile$do_tags
    OutDataFile$N_tag_groups = myDataFile$N_tag_groups
    OutDataFile$N_recap_events = myDataFile$N_recap_events
    OutDataFile$mixing_latency_period = myDataFile$mixing_latency_period
    OutDataFile$max_periods = myDataFile$max_periods
    OutDataFile$tag_releases = myDataFile$tag_releases
    OutDataFile$tag_recaps = myDataFile$tag_recaps
    OutDataFile$tag_releases$yr = 1000 + OutDataFile$tag_releases$yr
    OutDataFile$tag_releases$tfill = 1000 + OutDataFile$tag_releases$tfill
    OutDataFile$tag_releases$age = OutDataFile$tag_releases$age # SS age
    OutDataFile$tag_recaps$yr = 1000 + OutDataFile$tag_recaps$yr
    if(myDataFile$N_areas == 1) OutDataFile$tag_recaps$fleet = 6 # purse seine
    
  }
  
  # CONTROL FILE
  
  OutControlFile$N_areas = myDataFile$N_areas
  OutControlFile$Nfleets = myDataFile$Nfleet + myDataFile$Nsurveys
  OutControlFile$fleetnames = myDataFile$fleetnames
  OutControlFile$recr_dist_read = myDataFile$N_areas
  OutControlFile$recr_dist_pattern = data.frame(GPattern = 1, month = 1, area = 1:myDataFile$N_areas, 
                                                age = 1)
  
  # Movment information
  if(myDataFile$N_areas > 1) {
    OutControlFile$N_moveDef = N_moveDef
    if(N_moveDef > 0) {
      if(is.null(source_area)) stop("'source_area' is required.")
      if(is.null(dest_area)) stop("'dest_area' is required.")
      OutControlFile$firstAgeMove = 1
      OutControlFile$moveDef = data.frame(seas = 1, morph = 1, source = source_area,
                                          dest = dest_area, age = 1, age2 = 28)
    }
  }
  
  x_rep = c(rep(1, times = 22), myDataFile$N_areas, 1,1)
  MGdf = OutControlFile$MG_parms
  MGdf_new = MGdf %>% 
              slice(rep(1:nrow(MGdf), times= x_rep))
  OutControlFile$MG_parms = MGdf_new
  OutControlFile$MG_parms[23:(22+myDataFile$N_areas),3] = RecDistInit # Init values
  OutControlFile$MG_parms[23:(22+myDataFile$N_areas),7] = RecDistPhase # Phase
  if(RecDistTemp) { # Time variability in recruitment distribution
    OutControlFile$MG_parms[23:(22+myDataFile$N_areas),9] = c(1,1,0,1) 
    OutControlFile$MG_parms[23:(22+myDataFile$N_areas),10] = c(1100,1100,0,1100) 
    OutControlFile$MG_parms[23:(22+myDataFile$N_areas),11] = c(1256,1256,0,1256) 
    OutControlFile$MG_parms[23:(22+myDataFile$N_areas),12] = c(3,3,0,3) 
  }

  if(N_moveDef > 0) { # adding movement parameters
    movement_DF = data.frame(LO = -20, HI = 20, INIT = rep(0, times = N_moveDef*2), PRIOR = 0, PR_SD = 0,
                             PR_type = 0, PHASE = MovPhase, env_var = 0,
                             dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
    part1_MG = OutControlFile$MG_parms[1:(nrow(OutControlFile$MG_parms)-1),]
    part2_MG = OutControlFile$MG_parms[nrow(OutControlFile$MG_parms),]
    colnames(movement_DF) = colnames(part1_MG)
    OutControlFile$MG_parms = rbind(part1_MG, movement_DF, part2_MG)
  }

  if(RecDistTemp) {
    OutControlFile$MG_parms_tv = data.frame(LO = -99, HI = 99, INIT = c(1,0,1,0,1,0), 
                                            PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(3,-3,3,-3,3,-3))
  }

  # Catchability
  link_value = 0
  link_info_value = 0
  if(myDataFile$N_areas > 1) {
    link_value = c(0,2,2,2)
    link_info_value = c(0,17,17,17)
  }
  OutControlFile$Q_options = data.frame(fleet = (myDataFile$Nfleet+1):(myDataFile$Nfleet+myDataFile$Nsurveys), 
                                        link = link_value, link_info = link_info_value, extra_se = 0, biasadj = 0, float = 0)
  rownames(OutControlFile$Q_options) = myDataFile$fleetnames[(myDataFile$Nfleet+1):(myDataFile$Nfleet+myDataFile$Nsurveys)]
  OutControlFile$Q_parms = data.frame(LO = -30, HI = 15, INIT = rep(-3, times = myDataFile$Nsurveys), PRIOR = 0, PR_SD = 0,
                                      PR_type = 0, PHASE = Qphase, env_var = 0,
                                      dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
  rownames(OutControlFile$Q_parms) = paste0('LnQ', myDataFile$fleetnames[(myDataFile$Nfleet+1):(myDataFile$Nfleet+myDataFile$Nsurveys)])
  
  
  OutControlFile$size_selex_types = selex_len
  OutControlFile$age_selex_types = selex_age
  OutControlFile$size_selex_parms = selex_df
  OutControlFile$age_selex_parms = NULL
  
  if(use_tags) {
    OutControlFile$TG_custom = 1
    OutControlFile$TG_Loss_init = data.frame(LO = -10, HI = 10, INIT = rep(-2, times = myDataFile$N_tag_groups), PRIOR = -7, PR_SD = 0.001,
                                      PR_type = 1, PHASE = -4, env_var = 0,
                                      dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
    OutControlFile$TG_Loss_chronic = data.frame(LO = -10, HI = 10, INIT = rep(-4, times = myDataFile$N_tag_groups), PRIOR = -7, PR_SD = 0.001,
                                      PR_type = 1, PHASE = -4, env_var = 0,
                                      dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
    OutControlFile$TG_overdispersion = data.frame(LO = 1, HI = 10, INIT = rep(7, times = myDataFile$N_tag_groups), PRIOR = 2, PR_SD = 0.001,
                                      PR_type = 1, PHASE = -4, env_var = 0,
                                      dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
    OutControlFile$TG_Report_fleet = data.frame(LO = -10, HI = 10, INIT = rep(-7, times = myDataFile$Nfleet), PRIOR = -7, PR_SD = 0.001,
                                      PR_type = 1, PHASE = -4, env_var = 0,
                                      dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
    if(myDataFile$N_areas == 1) {
      OutControlFile$TG_Report_fleet$INIT[6] = 7
      OutControlFile$TG_Report_fleet$PHASE[6] = 7
    }
    if(myDataFile$N_areas == 4) {
      OutControlFile$TG_Report_fleet$INIT[11:13] = 7
      OutControlFile$TG_Report_fleet$PHASE[11:13] = 8
    }
    OutControlFile$TG_Report_fleet_decay = data.frame(LO = -4, HI = 0, INIT = rep(-3, times = myDataFile$Nfleet), PRIOR = 0, PR_SD = 2,
                                      PR_type = 6, PHASE = -4, env_var = 0,
                                      dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
    if(myDataFile$N_areas == 1) {
      OutControlFile$TG_Report_fleet_decay$INIT[6] = 0
      OutControlFile$TG_Report_fleet_decay$PHASE[6] = 7
    }
    if(myDataFile$N_areas == 4) {
      OutControlFile$TG_Report_fleet_decay$INIT[11:13] = 0
      OutControlFile$TG_Report_fleet_decay$PHASE[11:13] = 8
    }
  }

  # For Dirichlet parameters:
  # OutControlFile$dirichlet_parms = data.frame(LO = -15, HI = 15, INIT = rep(x = 0, times = OutDataFile$Nfleets), PRIOR = 0, PR_SD = 99,
  #                                     PR_type = 0, PHASE = 6, env_var = 0,
  #                                     dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)

  # OUT FILES
  myFiles = list(dat = OutDataFile, ctl = OutControlFile)
  
  return(myFiles)

}