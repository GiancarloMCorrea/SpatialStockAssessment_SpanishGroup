#V3.30
#C file created using the SS_writectl function in the R package r4ss
#C file write time: 2021-01-27 10:37:57
#
0 # 0 means do not read wtatage.ss; 1 means read and usewtatage.ss and also read and use growth parameters
1 #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern
3 # recr_dist_method for parameters
1 # not yet implemented; Future usage:Spawner-Recruitment; 1=global; 2=by area
3 # number of recruitment settlement assignments 
0 # unused option
# for each settlement assignment:
#_GPattern	month	area	age
1	1	1	0	#_1
1	4	1	0	#_2
1	7	1	0	#_3
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern
# begin and end years of blocks
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement
#
3 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
#_ #_Age_natmort_by sex x growthpattern
#_Age_0	Age_1	Age_2	Age_3	Age_4	Age_5	Age_6	Age_7
1.2223	0.6581	0.5439	0.7295	0.7579	0.5995	0.5425	0.5376	#_natM1
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0.5 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0 #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
1 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
# Age Maturity or Age fecundity:
#_Age_0	Age_1	Age_2	Age_3	Age_4	Age_5	Age_6	Age_7
0	0.1125	0.6	1	1	1	1	1	#_Age_Maturity1
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
 2.0e+00	 24.000000	 2.200e+01	 12.00000000	 3.00	0	 -2	0	0	0	0	0	0	0	#_L_at_Amin_Fem_GP_1          
 5.0e+01	180.000000	 1.450e+02	100.17000000	18.62	6	 -3	0	0	0	0	0	0	0	#_L_at_Amax_Fem_GP_1          
 6.0e-03	  0.600000	 4.550e-01	 -1.27200000	 2.30	4	 -3	0	0	0	0	0	0	0	#_VonBert_K_Fem_GP_1          
 5.0e-03	  0.500000	 3.000e-01	  0.20000000	 0.80	0	 -6	0	0	0	0	0	0	0	#_CV_young_Fem_GP_1           
 5.0e-03	  0.500000	 1.000e-01	  0.10000000	 0.80	0	 -6	0	0	0	0	0	0	0	#_CV_old_Fem_GP_1             
-1.0e+00	  1.000000	 2.460e-05	  0.00000659	 0.05	0	 -3	0	0	0	0	0	0	0	#_Wtlen_1_Fem_GP_1            
 2.0e+00	  4.000000	 2.966e+00	  3.01221000	 0.05	0	 -3	0	0	0	0	0	0	0	#_Wtlen_2_Fem_GP_1            
 3.0e+01	 55.000000	 3.650e+01	 36.50000000	 5.00	0	 -3	0	0	0	0	0	0	0	#_Mat50%_Fem_GP_1             
-1.0e+00	  1.000000	-2.000e-01	 -0.20000000	 0.50	0	 -3	0	0	0	0	0	0	0	#_Mat_slope_Fem_GP_1          
-3.0e+00	  3.000000	 1.000e+00	  1.00000000	 0.80	0	 -3	0	0	0	0	0	0	0	#_Eggs_scalar_Fem_GP_1        
-3.0e+00	  3.000000	 0.000e+00	  0.00000000	 0.80	0	 -3	0	0	0	0	0	0	0	#_Eggs_exp_wt_Fem_GP_1        
-1.2e+01	 15.000000	 0.000e+00	  0.00000000	 0.00	0	 -4	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_1_month_1
-1.2e+01	 15.000000	 0.000e+00	  0.00000000	 0.00	0	  4	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_1_month_4
-1.2e+01	 15.000000	 0.000e+00	  0.00000000	 0.00	0	  4	0	0	0	0	0	0	0	#_RecrDist_GP_1_area_1_month_7
 0.0e+00	  2.000000	 1.000e+00	  1.00000000	 0.00	0	 -3	0	0	0	0	0	0	0	#_CohortGrowDev               
 1.0e-06	  0.999999	 5.000e-01	  0.50000000	 0.50	0	-99	0	0	0	0	0	0	0	#_FracFemale_GP_1             
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; 2=Ricker; 3=std_B-H; 4=SCAA;5=Hockey; 6=B-H_flattop; 7=survival_3Parm;8=Shepard_3Parm
0 # 0/1 to use steepness in initial equ recruitment calculation
0 # future feature: 0/1 to make realized sigmaR a function of SR curvature
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn # parm_name
 12	22.000	 18.39	11.48	1.000	0	  1	0	0	0	0	0	0	0	#_SR_LN(R0)  
 0.2	 0.999	 0.800	 0.777	0.113	2	 -4	0	0	0	0	0	0	0	#_SR_BH_steep
 0.1	 2.000	 0.600	 0.400	0.200	0	 -1	0	0	0	0	0	0	0	#_SR_sigmaR  
-5.0	 6.000	 0.000	-0.700	2.000	0	 -1	0	0	0	0	0	0	0	#_SR_regime  
 0.0	 0.000	 0.000	 0.000	0.000	0	-99	0	0	0	0	0	0	0	#_SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1972 # first year of main recr_devs; early devs can preceed this era
2014 # last year of main recr_devs; forecast devs start in following year
2 #_recdev phase
1 # (0/1) to read 13 advanced options
-10 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
3 #_recdev_early_phase
-99 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 #_lambda for Fcast_recr_like occurring before endyr+1
1970 #_last_yr_nobias_adj_in_MPD; begin of ramp
1990 #_first_yr_fullbias_adj_in_MPD; begin of plateau
2014 #_last_yr_fullbias_adj_in_MPD
2015 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
0.9618 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
0 #_period of cycles in recruitment (N parms read below)
-5 #min rec_dev
5 #max rec_dev
0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
#Fishing Mortality info
0.3 # F ballpark
-2001 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
5 # max F or harvest rate, depends on F_Method
5 # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#
#_Q_setup for fleets with cpue or survey data
#_fleet	link	link_info	extra_se	biasadj	float  #  fleetname
    8	1	0	0	0	0	#_llcpue    
-9999	0	0	0	0	0	#_terminator
#_Q_parms(if_any);Qunits_are_ln(q)
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
-15	15	0	0	0	0	3	0	0	0	0	0	0	0	#_LnQllcpue
#_no timevary Q parameters
#
#_size_selex_patterns
#_Pattern	Discard	Male	Special
24	0	0	0	#_1 1
24	0	0	0	#_2 2
 1	0	0	0	#_3 3
24	0	0	0	#_4 4
24	0	0	0	#_5 5
24	0	0	0	#_6 6
24	0	0	0	#_7 7
15	0	0	3	#_8 8
#
#_age_selex_patterns
#_Pattern	Discard	Male	Special
0	0	0	0	#_1 1
0	0	0	0	#_2 2
0	0	0	0	#_3 3
0	0	0	0	#_4 4
0	0	0	0	#_5 5
0	0	0	0	#_6 6
0	0	0	0	#_7 7
0	0	0	0	#_8 8
#
#_SizeSelex
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
  13.00	  70	  19	  15	 0.01	0	 5	0	0	0	0	0.0	0	0	#_1 
 -16.00	   2	 -16	  -2	 0.01	0	 6	0	0	0	0	0.0	0	0	#_2 
  -6.00	  14	   3	   4	 0.01	0	 6	0	0	0	0	0.0	0	0	#_3 
   1.00	  40	  40	  10	 0.01	0	 6	0	0	0	0	0.0	0	0	#_4 
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_5 
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_6 
  13.00	  70	  19	  15	 0.01	0	 5	0	0	0	0	0.0	0	0	#_7 
 -16.00	   2	 -16	  -2	 0.01	0	 6	0	0	0	0	0.0	0	0	#_8 
  -6.00	  14	   3	   4	 0.01	0	 6	0	0	0	0	0.0	0	0	#_9 
   1.00	  40	  40	  10	 0.01	0	 6	0	0	0	0	0.0	0	0	#_10
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_11
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_12
  19.00	  70	  45	  50	99.00	0	 5	0	0	0	0	0.5	0	0	#_13
   0.01	  60	  20	  15	99.00	0	 5	0	0	0	0	0.5	0	0	#_14
  13.00	  70	  19	  15	 0.01	0	 5	0	0	0	0	0.0	0	0	#_15
 -16.00	   2	 -16	  -2	 0.01	0	 6	0	0	0	0	0.0	0	0	#_16
  -6.00	  14	   3	   4	 0.01	0	 6	0	0	0	0	0.0	0	0	#_17
   1.00	  40	  40	  10	 0.01	0	 6	0	0	0	0	0.0	0	0	#_18
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_19
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_20
  13.00	  70	  19	  15	 0.01	0	 5	0	0	0	0	0.0	0	0	#_21
 -16.00	   2	 -16	  -2	 0.01	0	 6	0	0	0	0	0.0	0	0	#_22
  -6.00	  14	   3	   4	 0.01	0	 6	0	0	0	0	0.0	0	0	#_23
   1.00	  40	  40	  10	 0.01	0	 6	0	0	0	0	0.0	0	0	#_24
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_25
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_26
  13.00	  70	  19	  15	 0.01	0	 5	0	0	0	0	0.0	0	0	#_27
 -16.00	   2	 -16	  -2	 0.01	0	 6	0	0	0	0	0.0	0	0	#_28
  -6.00	  14	   3	   4	 0.01	0	 6	0	0	0	0	0.0	0	0	#_29
   1.00	  40	  40	  10	 0.01	0	 6	0	0	0	0	0.0	0	0	#_30
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_31
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_32
  13.00	  70	  19	  15	 0.01	0	 5	0	0	0	0	0.0	0	0	#_33
 -16.00	   2	 -16	  -2	 0.01	0	 6	0	0	0	0	0.0	0	0	#_34
  -6.00	  14	   3	   4	 0.01	0	 6	0	0	0	0	0.0	0	0	#_35
   1.00	  40	  40	  10	 0.01	0	 6	0	0	0	0	0.0	0	0	#_36
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_37
-999.00	-999	-999	-999	 0.01	0	-2	0	0	0	0	0.0	0	0	#_38
#_AgeSelex
#_No age_selex_parm
#_Dirichlet parameters
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_1
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_2
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_3
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_4
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_5
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_6
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_7
-5	5	5	0	99	0	-7	0	0	0	0	0	0	0	#_ln(DM_theta)_8
#_no timevary selex parameters
#
0 #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
# Tag loss and Tag reporting parameters go next
0 # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# Input variance adjustments factors: 
#_Factor Fleet Value
-9999 1 0 # terminator
#
1 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 0 changes to default Lambdas (default value is 1.0)
-9999 0 0 0 0 # terminator
#
0 # 0/1 read specs for more stddev reporting
#
999