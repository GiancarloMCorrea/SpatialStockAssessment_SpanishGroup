
## Bio trend from base model ---------------------------------------------------
rm(list = ls())
library(r4ss)
require(stringr)
require(dplyr)
require(ggplot2)
saveDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'

# Read model outputs for 1A model:
load('Year_runs/20 assessments year CPUE st 1A.RData')
Y1A = ggdat
Y1A$model_name = '1A_25_Y'
load('Year_runs/20 assessments year CPUE st 4A.RData')
Y4A = ggdat
Y4A$model_name = '4A_25_Y'

# Information Year model:
Year_config = rbind(Y1A[,c('years', 'ssb_val', 'F_val', 'run', 'model_name')], 
                    Y4A[,c('years', 'ssb_val', 'F_val', 'run', 'model_name')])
Year_config$run  = as.numeric(as.character(Year_config$run ))

# Info:
yrvector = 1001:1256 # could be any vector (decimals to include seasons)
min_grad = 0.1

# -------------------------------------------------------------------------
# Main directories (model types):
dir_mods = c('1A_25_ageS_PY_CPUEst_tags', '4A_25_ageS_PY_CPUEst_tags_move')

# -------------------------------------------------------------------------
# Loop to read results from all replicates:
ts_tmp = list()
cList = 1
for(j in seq_along(dir_mods)) { 
  
  mod_path = paste0(saveDir, '/', dir_mods[j])
  all_iter = list.files(path = mod_path)  
  
  for(k in seq_along(all_iter)) {
    iter_name = as.numeric(str_split(string = all_iter[k] , pattern = '_', simplify = TRUE)[1,3])
    tmp_mod = SS_output(dir = file.path(mod_path, all_iter[k]), covar = FALSE, verbose = FALSE,
                        printstats = FALSE) 
    SSB_info = tmp_mod$derived_quants[grep(pattern = 'SSB_', x = tmp_mod$derived_quants$Label)[3:258], ]
    F_info = tmp_mod$derived_quants[grep(pattern = 'F_', x = tmp_mod$derived_quants$Label)[1:256], ]
    R_info = tmp_mod$derived_quants[grep(pattern = 'Recr_', x = tmp_mod$derived_quants$Label)[3:258], ]
    ts_tmp[[cList]] = data.frame(iter = iter_name, em = dir_mods[j],
                                 year = 1001:1256, SSB = SSB_info$Value, R_val = R_info$Value,
                                 Fval = F_info$Value, grad = tmp_mod$maximum_gradient_component)
    cList = cList+1
    print(k)
  }
  
}


# Organize data for plot ---------------------------------------------------

# Clean data (no convergence):
ts_data0 = dplyr::bind_rows(ts_tmp)
ts_data0$model_name = ifelse(test = ts_data0$em == '4A_25_ageS_PY_CPUEst_tags_move', yes = '4A_25_PY', no = '1A_25_PY')
ts_data2 = ts_data0[ts_data0$grad < min_grad, c(3,4,5,6,1,8)]
ts_data1 = ts_data0[ts_data0$grad < min_grad, c(3,4,6,1,8)]
colnames(ts_data1) = c('years', 'ssb_val', 'F_val', 'run', 'model_name')

# Get values for table
ts_data3 = ts_data2 %>% 
              dplyr::group_by(model_name, year) %>%
              dplyr::summarise(CVSSB = sd(SSB)/mean(SSB), CVR = sd(R_val)/mean(R_val), CVF = sd(Fval)/mean(Fval))
ts_data3 %>% 
  dplyr::group_by(model_name) %>%
  dplyr::summarise(meanSSB = mean(CVSSB), meanR = mean(CVR), meanF = mean(CVF),
                   sdSSB = sd(CVSSB), sdR = sd(CVR), sdF = sd(CVF))

# Convergence rate:
conv_rate = ts_data0 %>%
  dplyr::group_by(em, iter) %>%
  dplyr::summarise(grad = mean(grad))
#View(conv_rate)

# Replace years:
ts_data1$years = rep(1952:2015, each = 4)
ts_data1 = ts_data1 %>%
  dplyr::group_by(years, run, model_name) %>%
  dplyr::summarise(ssb_val = mean(ssb_val), F_val = mean(F_val))

# Merge data sets:
plot_data = rbind(Year_config, ts_data1)
plot_data2 = tidyr::gather(data = plot_data, 'variable', 'value', 2:3)
plot_data3 = plot_data2 %>%
  dplyr::group_by(years, model_name, variable) %>% 
  dplyr::summarise(mean = mean(value), p1 = quantile(value, p = 0.025), 
                   p2 = quantile(value, p = 0.975))
plot_data3$variable[plot_data3$variable == 'F_val'] = 'F'
plot_data3$variable[plot_data3$variable == 'ssb_val'] = 'SSB'

save(plot_data3, file = 'Year_runs/replicates_all.RData')

# Plots -------------------------------------------------------------------

plot_data4 = plot_data3[plot_data3$model_name == '1A_25_Y' | plot_data3$model_name == '1A_25_PY', ]
ggplot(plot_data4, aes(x = years, y = mean, ymin=p1, ymax=p2)) +
  labs(y = '') +
  geom_ribbon(alpha=.3) + 
  geom_line(lwd=1) +
  theme_bw() +
  facet_grid(variable ~ model_name, scales = 'free_y')
ggsave(filename = 'figures/compare_ts_1A.png', width = 190, height = 130, units = 'mm', dpi = 500)

plot_data4 = plot_data3[plot_data3$model_name == '4A_25_Y' | plot_data3$model_name == '4A_25_PY', ]
ggplot(plot_data4, aes(x = years, y = mean, ymin=p1, ymax=p2)) +
  labs(y = '') +
  geom_ribbon(alpha=.3) + 
  geom_line(lwd=1) +
  theme_bw() +
  facet_grid(variable ~ model_name, scales = 'free_y')
ggsave(filename = 'figures/compare_ts_4A.png', width = 190, height = 130, units = 'mm', dpi = 500)



