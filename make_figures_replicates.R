
## Libraries ---------------------------------------------------
rm(list = ls())
library(r4ss)
require(stringr)
require(dplyr)
require(ggplot2)

# Set working directory to source file location
saveFolder = 'replicates_data' # folder to save produced RData

# Read model outputs for 1A model:
load('replicates_data/100 assessments year CPUE st 1A.RData')
Y1A = ggdat
Y1A$model_name = '1A_25_Y'
load('replicates_data/20 assessments year CPUE st 4A.RData')
Y4A = ggdat
Y4A$model_name = '4A_25_Y'

# Information Year model:
Year_config = rbind(Y1A[,c('years', 'ssb_val', 'F_val', 'run', 'model_name')], 
                    Y4A[,c('years', 'ssb_val', 'F_val', 'run', 'model_name')])
Year_config$run  = as.numeric(as.character(Year_config$run ))

# Now read PY configuration:
load('replicates_data/PY_all.RData') # 'ts_data0'

# Info:
yrvector = 1001:1256 # could be any vector (decimals to include seasons)
max_grad = 5

# Convergence -------------------------------------------------------------

ts_data2 = ts_data0[ts_data0$grad < max_grad, c('year','SSB','R_val','Fval','iter','model_name')]
ts_data1 = ts_data0[ts_data0$grad < max_grad, c('year','SSB','Fval','iter','model_name')]
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
conv_rate2 = conv_rate %>%
  dplyr::group_by(em) %>%
  dplyr::summarise(convrate = sum(grad < max_grad)/n())
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

save(plot_data3, file = file.path(saveFolder, 'replicates_merged.RData'))

# Plots time series -------------------------------------------------------------------

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


# Plots recruitment by area -----------------------------------------------

plot_data5 = ts_data0[ts_data0$grad < max_grad & ts_data0$em == '4A_25_ageS_PY_CPUEst_tags_move', c('iter', 'R1', 'R2', 'R3', 'R4')]
plot_data5 = plot_data5 %>%
  dplyr::group_by(iter) %>% 
  dplyr::summarise(R1 = mean(R1),R2 = mean(R2),R3 = mean(R3),R4 = mean(R4))
plot_data5 = tidyr::gather(plot_data5, "area", "prop", 2:5)
ggplot(plot_data5, aes(x = area, y = prop, fill = factor(area))) +
  geom_boxplot() +
  theme_bw()
ggsave(filename = 'figures/R_prop_4A.png', width = 190, height = 130, units = 'mm', dpi = 500)


