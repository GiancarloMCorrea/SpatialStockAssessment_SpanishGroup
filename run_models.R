# Clean workspace:
rm(list = ls())

# Set working directory:

# directory to save models (local disk)
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'

# Libraries:
require(r4ss)
require(ss3diags)
require(Rfssa)
require(doSNOW)
require(parallel)
source('get_initial_files.R')
nCores = 5
nSim = 1:5 # sequence of replicates to run

# Important parameters to run models:
Nsamp = '25'
n_areas = 4
selType = 'age' # len or age
use_CPUEst = TRUE
use_tags = TRUE
use_recdist_time = FALSE
N_moveDef = 4 
# N_moveDef: 
# - 0 for no movement or 1 area model. 
# - 8 for 4 area model with movement: 1-2, 1-4, 3-4, and 2-3
# - 6 for 4 area model with movement: 1-2, 1-4, 3-4 or 2-3
# - 4 for 4 area model with movement: 1-2, 1-4
# Movement definitions (only used if N_moveDef > 0):
typeMov = 4 # 1: N_moveDef = 8, 2: N_moveDef = 6 and 4-3, 3: N_moveDef = 6 and 2-3, 4: N_moveDef = 4 and no mov to 3.


# -------------------------------------------------------------------------
# Read data from Github:
# this_url = paste0("https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/blob/main/data/YFT_", n_areas,"area_observations_1_100_ESS_", Nsamp,".RData")
# mydata = Rfssa::load_github_data(github_data_url = this_url)
# Read downloaded data:
this_file = paste0("sim_data/YFT_", n_areas,"area_observations_1_100_ESS_", Nsamp,".RData")
mydata = load(this_file)


# -------------------------------------------------------------------------
# Continue running:
base_folder = paste0('base_', selType,'Selex')
extra_name = ''
if(use_CPUEst) extra_name = paste0(extra_name, '_CPUEst')
if(use_tags) extra_name = paste0(extra_name, '_tags')
if(N_moveDef > 0) extra_name = paste0(extra_name, '_move')
if(N_moveDef > 0) extra_name = paste0(extra_name, 'Type', typeMov)
if(use_recdist_time > 0) extra_name = paste0(extra_name, '_RDTime')
type_model = paste0(n_areas, 'A_', Nsamp,'_', selType,'S_PY', extra_name)
dir.create(path = file.path(saveDir, type_model))
cpue_folder = 'CPUE_standardization/output/03 besag st'

# Information for recruitment distribution:
if(n_areas == 1){
  RecDistInit = 0
  RecDistPhase = -7
}
if(n_areas == 4){
  RecDistInit = c(0,0,0,0)
  RecDistPhase = c(3,-3,3,3)
}
# Information for movement:
MovePhase = 8
if(typeMov == 1) {
  source_area = c(1,2,4,1,4,3,2,3)
  dest_area =   c(2,1,1,4,3,4,3,2)
}
if(typeMov == 2) {
  source_area = c(1,2,4,1,4,3)
  dest_area =   c(2,1,1,4,3,4)
}
if(typeMov == 3) {
  source_area = c(1,2,4,1,2,3)
  dest_area =   c(2,1,1,4,3,2)
}
if(typeMov == 4) {
  source_area = c(1,2,4,1)
  dest_area =   c(2,1,1,4)
}

# -------------------------------------------------------------------------
# Define selex parameters:

if(selType == 'len') {
  # Length Selectivity parameters:
  selex_params_double = data.frame(LO   = c(15,   -10,  -20, -20,-20,-20),
                                   HI   = c(150,   10,   15,  20, 15, 15),
                                   INIT = c(50,    -2,   -1,   1, -4, -4),
                                   PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,4,5,5,6,6),
                                   env_var = 0, dev_link = 0, dev_minyr = 0,
                                   dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
  selex_params_logistic = data.frame(LO =   c(10,  0),
                                     HI   = c(150, 30),
                                     INIT = c(50,  10),
                                     PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,5),
                                     env_var = 0, dev_link = 0, dev_minyr = 0,
                                     dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
  # # For 1 Area:
  if(n_areas == 1) {
    selex_age = data.frame(Pattern = rep(0, times = 8),
                           Discard = 0, Male = 0, Special = 0)
    selex_len = data.frame(Pattern = c(24,24,1,24,24,24,24,15),
                           Discard = 0, Male = 0, Special = c(rep(0, times= 7), 3))
    selex_df = rbind(selex_params_double,selex_params_double,selex_params_logistic,
                     selex_params_double,selex_params_double,selex_params_double,selex_params_double)
  }
  # For 4 Areas:
  if(n_areas == 4) {
    selex_age = data.frame(Pattern = rep(0, times = 20),
                           Discard = 0, Male = 0, Special = 0)
    selex_len = data.frame(Pattern = c(24,15,24,1,15,15,15,24,24,24,24,15,15,24,15,15,15,15,15,15),
                           Discard = 0, Male = 0,
                           Special = c(0,1,0,0,4,4,4,0,0,0,0,11,11,0,14,14,4,4,4,4))
    selex_df = rbind(selex_params_double,selex_params_double,selex_params_logistic,
                     selex_params_double,selex_params_double,selex_params_double,
                     selex_params_double,selex_params_double)
  }
}

if(selType == 'age') {
  
  # Age Selectivity parameters:
  selex_params_double = data.frame(LO   = c(0,   -40, -40,-40,-40,-40),
                                   HI   = c(25,    15, 15,  15, 15,   15),
                                   INIT = c(5,   2,  -6,   2, -4, -4),
                                   PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,4,5,5,6,6),
                                   env_var = 0, dev_link = 0, dev_minyr = 0,
                                   dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
  selex_params_logistic = data.frame(LO =   c(0, 0),
                                     HI   = c(25,25),
                                     INIT = c(10, 2),
                                     PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,5),
                                     env_var = 0, dev_link = 0, dev_minyr = 0,
                                     dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
  # #For 1 Area:
  if(n_areas == 1) {
    selex_len = data.frame(Pattern = rep(0, times = 8),
                           Discard = 0, Male = 0, Special = 0)
    selex_age = data.frame(Pattern = c(20,20,12,20,20,20,20,15),
                           Discard = 0, Male = 0, Special = c(rep(0, times= 7), 3))
    selex_df = rbind(selex_params_double,selex_params_double,selex_params_logistic,
                     selex_params_double,selex_params_double,selex_params_double,selex_params_double)
  }
  
  # # For 4 Areas:
  if(n_areas == 4) {
    selex_len = data.frame(Pattern = rep(0, times = 20),
                           Discard = 0, Male = 0, Special = 0)
    selex_age = data.frame(Pattern = c(20,15,20,12,15,15,15,20,20,20,20,15,15,20,15,15,15,15,15,15),
                           Discard = 0, Male = 0,
                           Special = c(0,1,0,0,4,4,4,0,0,0,0,11,11,0,14,14,4,4,4,4))
    selex_df = rbind(selex_params_double,selex_params_double,selex_params_logistic,
                     selex_params_double,selex_params_double,selex_params_double,
                     selex_params_double,selex_params_double)
  }
}

# -------------------------------------------------------------------------
# Read template:
template_dat = r4ss::SS_readdat_3.30(file = paste0(base_folder, '/data_ss.dat'))
template_ctl = r4ss::SS_readctl_3.30(file = paste0(base_folder, '/control_ss.ctl'), datlist = paste0(base_folder, '/data.ss_new'))
template_start = r4ss::SS_readstarter(file = paste0(base_folder, '/starter.ss'))
template_fore = r4ss::SS_readforecast(file = paste0(base_folder, '/forecast.ss'))


# -------------------------------------------------------------------------
# BEGIN LOOP ONLY TO CREATE THE INPUT SS FILES:
base_name = substr(x = mydata[1], start = 1, stop = 7)
for(i in nSim) {
  
  dir.create(path = file.path(saveDir, type_model, paste0(base_name, i)))
  this_data = get(paste0(base_name, i))
  
  # -------------------------------------------------------------------------
  # Create input files DAT y CTL
  
  outFiles = get_initial_files(sim_dat = this_data, ctl = template_ctl, dat = template_dat, 
                               selex_len = selex_len, selex_age = selex_age, selex_df = selex_df,
                               use_tags = use_tags, RecDistPhase = RecDistPhase, MovPhase = MovePhase,
                               N_moveDef = N_moveDef, source_area = source_area, dest_area = dest_area,
                               RecDistTemp = use_recdist_time)
  
  # Update standardized CPUE:
  if(use_CPUEst) {
    load(file.path(cpue_folder, paste0('input_SS_', n_areas,'A_', i, '.RData')))
    if(n_areas == 1) {
      this_cpue = input_SS_1A # change name for 4 areas
      outFiles$dat$CPUE$obs = this_cpue$sumX50
      outFiles$dat$CPUE$se_log = 0.1 # this_cpue$sumSd/this_cpue$sumX50
    }
    if(n_areas == 4) {
      this_cpue = input_SS_4A # change name for 4 areas
      this_cpue = this_cpue[order(this_cpue$regionID),]
      outFiles$dat$CPUE = data.frame(year = rep(x = 1081:1256, times = 4), seas = 7, 
                                         index = rep(x = c(17,18,19,20), each = 176), 
                                         obs = this_cpue$sumX50, se_log = 0.1)
    }
  }
  
  # -------------------------------------------------------------------------
  # Write SS files:
  
  r4ss::SS_writedat_3.30(datlist = outFiles$dat, 
                         outfile = paste0(file.path(saveDir, type_model, paste0(base_name, i)), '/data_ss.dat'), 
                         overwrite = TRUE)
  r4ss::SS_writestarter(mylist = template_start, 
                        dir = paste0(file.path(saveDir, type_model, paste0(base_name, i))), 
                        file = '/starter.ss', overwrite = TRUE)
  r4ss::SS_writeforecast(mylist = template_fore, 
                         dir = paste0(file.path(saveDir, type_model, paste0(base_name, i))), 
                         file = 'forecast.ss', overwrite = TRUE)
  r4ss::SS_writectl_3.30(ctllist = outFiles$ctl, 
                         outfile = paste0(file.path(saveDir, type_model, paste0(base_name, i)), '/control_ss.ctl'),
                         overwrite = TRUE)
  
}
 
# # # -------------------------------------------------------------------------
# # # Detect number of cores:
cl = makeCluster(nCores)
registerDoSNOW(cl)


# # -------------------------------------------------------------------------
# # BEGIN LOOP to RUN ss IN PARALLEL:
#
foreach(ix = nSim) %dopar% {

  # -------------------------------------------------------------------------
  # Run SS3:
  dir = paste0(saveDir, '/', file.path(type_model, paste0(base_name, ix)))
  command = paste("cd", dir, "& ss -nohess", sep = " ")
  ss = shell(cmd = command, intern = T, wait = T)

  # # Find Francis weight:
  # my_report = SS_output(dir = paste0(saveDir, '/', file.path(type_model, mydata[ix])),
  #                       covar=FALSE, verbose = FALSE, printstats = FALSE)
  #
  # # Read data:
  # template_dat = r4ss::SS_readdat_3.30(file = paste0(saveDir, '/', file.path(type_model, mydata[ix]), '/data_ss.dat'))
  #
  # # Update Nsamp
  # for(j in 1:my_report$nfishfleets) {
  #   tmp_wgt = SSMethod.TA1.8(fit = my_report, type = "len", fleet = j, plotit = FALSE)
  #   factor_francis = round(tmp_wgt[1],2)
  #   pos_fleet = template_dat$lencomp$FltSvy == j
  #   template_dat$lencomp$Nsamp[pos_fleet] = factor_francis*template_dat$lencomp$Nsamp[pos_fleet]
  # }
  #
  # # Write updated .dat:
  # r4ss::SS_writedat_3.30(datlist = template_dat,
  #                        outfile = paste0(file.path(saveDir, type_model, mydata[ix]), '/data_ss.dat'),
  #                        overwrite = TRUE)
  #
  # # Run model again:
  # command = paste("cd", dir, "& ss -nohess", sep = " ")
  # ss = shell(cmd = command, intern = T, wait = T)
  #
  # # Read report:
  # my_report = SS_output(dir = paste0(saveDir, '/', file.path(type_model, mydata[ix])),
  #                      covar=TRUE, verbose = FALSE, printstats = FALSE)
  #
  # # Make plots:
  # SS_plots(replist = my_report)

}

stopCluster(cl)


