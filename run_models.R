# Clean workspace:
rm(list = ls())

# TODO: Add movement parameters

# Set working directory
mainDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
setwd(mainDir)

# Libraries:
require(r4ss)
require(Rfssa)
require(doSNOW)
require(parallel)
source('get_initial_files.R')
nCoresRemain = 4
type_model = '1A_25_AgeS_PY'
base_folder = 'base_ageSelex'

# -------------------------------------------------------------------------
# Read data from Github:
this_url = "https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/blob/main/data/YFT_1area_observations_1_100_ESS_25.RData"
mydata = Rfssa::load_github_data(github_data_url = this_url)
dir.create(path = type_model)

# -------------------------------------------------------------------------
# Len Selectivity parameters:
# selex_params_double = data.frame(LO   = c(15,   -10,  -20, -20,-1000,-10),
#                                  HI   = c(150,   10,   15,  20, 15,   5),
#                                  INIT = c(50,    -2,   -1,   1, -999, -4),
#                                  PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,4,5,5,-6,-6), 
#                                  env_var = 0, dev_link = 0, dev_minyr = 0,
#                                  dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
# selex_params_logistic = data.frame(LO =   c(15,  0),
#                                    HI   = c(150, 50),
#                                    INIT = c(50,  10),
#                                    PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,4), 
#                                    env_var = 0, dev_link = 0, dev_minyr = 0,
#                                    dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
# # For 1 Area:
# selex_age = data.frame(Pattern = rep(0, times = 8),
#                        Discard = 0, Male = 0, Special = 0)
# selex_len = data.frame(Pattern = c(24,24,1,24,24,24,24,15),
#                        Discard = 0, Male = 0, Special = c(rep(0, times= 7), 3))
# selex_df = rbind(selex_params_double,selex_params_double,selex_params_logistic,
#                  selex_params_double,selex_params_double,selex_params_double,selex_params_double)

# Age Selectivity parameters:
selex_params_double = data.frame(LO   = c(0,   -40, -40,-40,-1000,-40),
                                 HI   = c(25,    15, 15,  15, 15,   15),
                                 INIT = c(5,   2,  -6,   2, -999, -3),
                                 PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,4,5,5,-6,-6),
                                 env_var = 0, dev_link = 0, dev_minyr = 0,
                                 dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
selex_params_logistic = data.frame(LO =   c(0, 0),
                                   HI   = c(25,25),
                                   INIT = c(10, 2),
                                   PRIOR = 0, PR_SD = 0, PR_type = 0, PHASE = c(4,4),
                                   env_var = 0, dev_link = 0, dev_minyr = 0,
                                   dev_maxyr = 0, dev_PH = 0, Block = 0, Block_Fxn = 0)
# For 1 Area:
selex_len = data.frame(Pattern = rep(0, times = 8),
                       Discard = 0, Male = 0, Special = 0)
selex_age = data.frame(Pattern = c(20,20,12,20,20,20,20,15),
                       Discard = 0, Male = 0, Special = c(rep(0, times= 7), 3))
selex_df = rbind(selex_params_double,selex_params_double,selex_params_logistic,
                 selex_params_double,selex_params_double,selex_params_double,selex_params_double)


# For 4 Areas:
# selex_len = data.frame(Pattern = rep(0, times = 20),
#                        Discard = 0, Male = 0, Special = 0)
# selex_age = data.frame(Pattern = c(rep(20,times = 3),12,12,12,12,rep(20,times = 9),15,15,15,15),
#                              Discard = 0,
#                              Male = 0,
#                              Special = c(rep(0, times= 16), 4,5,6,7))
# selex_df = rbind(selex_params_double,selex_params_double,selex_params_double,selex_params_logistic,
#                  selex_params_logistic,selex_params_logistic,selex_params_logistic,selex_params_double,
#                  selex_params_double,selex_params_double,selex_params_double,selex_params_double,
#                  selex_params_double,selex_params_double,selex_params_double,selex_params_double)


# -------------------------------------------------------------------------
# Read template:
template_dat = r4ss::SS_readdat_3.30(file = paste0(base_folder, '/data_ss.dat'))
template_ctl = r4ss::SS_readctl_3.30(file = paste0(base_folder, '/control_ss.ctl'), datlist = paste0(base_folder, '/data.ss_new'))
template_start = r4ss::SS_readstarter(file = paste0(base_folder, '/starter.ss'))
template_fore = r4ss::SS_readforecast(file = paste0(base_folder, '/forecast.ss'))


# -------------------------------------------------------------------------
# BEGIN LOOP ONLY TO CREATE THE INPUT SS FILES:

for(i in seq_along(mydata)) {
  
  dir.create(path = file.path(type_model, mydata[i]))
  this_data = get(mydata[i])
  
  # -------------------------------------------------------------------------
  # Create input files DAT y CTL
  
  outFiles = get_initial_files(sim_dat = this_data, ctl = template_ctl, dat = template_dat, 
                               selex_len = selex_len, selex_age = selex_age, selex_df = selex_df)
  
  # -------------------------------------------------------------------------
  # Write SS files:
  
  r4ss::SS_writedat_3.30(datlist = outFiles$dat, outfile = paste0(file.path(type_model, mydata[i]), '/data_ss.dat'), overwrite = TRUE)
  r4ss::SS_writestarter(mylist = template_start, file = paste0(file.path(type_model, mydata[i]), '/starter.ss'), overwrite = TRUE)
  r4ss::SS_writeforecast(mylist = template_fore, file = paste0(file.path(type_model, mydata[i]), '/forecast.ss'), overwrite = TRUE)
  r4ss::SS_writectl_3.30(ctllist = outFiles$ctl, outfile = paste0(file.path(type_model, mydata[i]), '/control_ss.ctl'), overwrite = TRUE)
  
  print(i)
  
}


# -------------------------------------------------------------------------
# Detect number of cores:
cores = detectCores()
cl = makeCluster(cores[1] - nCoresRemain)
registerDoSNOW(cl)


# -------------------------------------------------------------------------
# BEGIN LOOP to RUN ss IN PARALLEL:

foreach(ix = seq_along(mydata)) %dopar% {
  
  # -------------------------------------------------------------------------
  # Run SS3:
  dir = paste0(mainDir, '/', file.path(type_model, mydata[ix])) 
  command = paste("cd", dir, "& ss -nohess", sep = " ")
  ss = shell(cmd = command, intern = T, wait = T)
  
}

stopCluster(cl)
