
get_initial_files = function(sim_dat, ctl, dat, selex_len, selex_age, selex_df) {

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
  index_vec = as.numeric(as.character(myDataFile$CPUE$index))
  if(any(is.na(index_vec))) index_vec = 8 # one area
  cpue_330 = data.frame(year = 1000 + as.numeric(as.character(myDataFile$CPUE$year)), 
                        seas = 7, 
                        index = index_vec,
                        obs = as.numeric(as.character(myDataFile$CPUE$cpu)), 
                        se_log = as.numeric(as.character(myDataFile$CPUE$cv)))
  OutDataFile$CPUE = cpue_330
  #continue...
  OutDataFile$len_info = data.frame(mintailcomp = -0.001, addtocomp = myDataFile$add_to_comp, 
                                    combine_M_F = 0, CompressBins = 0, CompError = 0, ParmSelect = 0,
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
  
  if(myDataFile$do_tags == 1 & myDataFile$N_areas > 1) {
    
    OutDataFile$do_tags = myDataFile$do_tags
    OutDataFile$N_tag_groups = myDataFile$N_tag_groups
    OutDataFile$N_recap_events = myDataFile$N_recap_events
    OutDataFile$mixing_latency_period = myDataFile$mixing_latency_period
    OutDataFile$max_periods = myDataFile$max_periods
    OutDataFile$tag_releases = myDataFile$tag_releases
    OutDataFile$tag_recaps = myDataFile$tag_recaps
    OutDataFile$tag_releases$yr = 1000 + OutDataFile$tag_releases$yr
    OutDataFile$tag_releases$tfill = 1000 + OutDataFile$tag_releases$tfill
    # OutDataFile$tag_releases$age = OutDataFile$tag_releases$age - 1 # SS age
    OutDataFile$tag_recaps$yr = 1000 + OutDataFile$tag_recaps$yr
    
  }
  
  # CONTROL FILE
  
  OutControlFile$N_areas = myDataFile$N_areas
  OutControlFile$Nfleets = myDataFile$Nfleet + myDataFile$Nsurveys
  OutControlFile$fleetnames = myDataFile$fleetnames
  OutControlFile$recr_dist_read = myDataFile$N_areas
  OutControlFile$recr_dist_pattern = data.frame(GPattern = 1, month = 1, area = 1:myDataFile$N_areas, 
                                                age = 0)
  
  if(myDataFile$N_areas > 1) OutControlFile$N_moveDef = 0
  # firstAgeMove (substract 1)
  # moveDef is df
  
  x_rep = c(rep(1, times = 23), myDataFile$N_areas, 1,1)
  MGdf = OutControlFile$MG_parms
  MGdf_new = MGdf %>% 
                slice(rep(1:nrow(MGdf), times= x_rep))
  OutControlFile$MG_parms = MGdf_new
  
  # Movement parameters after cohort growth deviation
  
  OutControlFile$Q_options = data.frame(fleet = (myDataFile$Nfleet+1):(myDataFile$Nfleet+myDataFile$Nsurveys), 
                                        link = 0, link_info = 0, extra_se = 0, biasadj = 0, float = 0)
  rownames(OutControlFile$Q_options) = myDataFile$fleetnames[(myDataFile$Nfleet+1):(myDataFile$Nfleet+myDataFile$Nsurveys)]
  OutControlFile$Q_parms = data.frame(LO = -30, HI = 15, INIT = rep(-8, times = myDataFile$Nsurveys), PRIOR = 0, PR_SD = 0,
                                      PR_type = 0, PHASE = 1, env_var = 0,
                                      dev_link = 0, dev_minyr = 0, dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
  rownames(OutControlFile$Q_parms) = paste0('LnQ', myDataFile$fleetnames[(myDataFile$Nfleet+1):(myDataFile$Nfleet+myDataFile$Nsurveys)])
  
  
  OutControlFile$size_selex_types = selex_len
  OutControlFile$age_selex_types = selex_age
  OutControlFile$size_selex_parms = selex_df
  OutControlFile$age_selex_parms = NULL
  
  # OUT FILES

  myFiles = list(dat = OutDataFile, ctl = OutControlFile)
  
  return(myFiles)

}