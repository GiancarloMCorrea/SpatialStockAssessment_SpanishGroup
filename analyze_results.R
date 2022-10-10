# Clean workspace:
rm(list = ls())

# Set working directory
mainDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
setwd(mainDir)

# Libraries:
require(r4ss)
require(stringr)
require(dplyr)
require(ggplot2)
yrvector = 1001:1256 # could be any vector (decimals to include seasons)
min_grad = 0.1

# -------------------------------------------------------------------------
# Main directories (model types):
dir_mods = c('1A_25_LenS_PY', '1A_25_AgeS_PY')

# -------------------------------------------------------------------------
# Loop to read results from all replicates:
cList = 1
ts_tmp = list()
quant_tmp = list()
for(j in seq_along(dir_mods)) { 

  all_iter = list.files(path = dir_mods[j])  
            
  for(k in seq_along(all_iter)) {
    iter_name = as.numeric(str_split(string = all_iter[k] , pattern = '_', simplify = TRUE)[1,3])
    tmp_mod = SS_output(dir = file.path(dir_mods[j], all_iter[k]), covar = FALSE, verbose = FALSE) 
    SSB0 = tmp_mod$timeseries$SpawnBio[tmp_mod$timeseries$Era == 'VIRG']*1e-06
    ts_data = tmp_mod$timeseries[tmp_mod$timeseries$Yr >= min(yrvector) & 
                                 tmp_mod$timeseries$Yr <= max(yrvector), ]
    ts_catch = tmp_mod$catch[tmp_mod$catch$Yr >= min(yrvector) & 
                             tmp_mod$catch$Yr <= max(yrvector), ]
    ts_tmp[[cList]] = data.frame(iter = iter_name, em = dir_mods[j],
                                 year = yrvector, SSB = ts_data$SpawnBio/1e+06, 
                                 R = ts_data$Recruit_0/1e+06, HR = ts_catch$ret_bio/ts_data$Bio_all)
    quant_tmp[[cList]] = data.frame(iter = iter_name, em = dir_mods[j],
                                    Ref_SSB = tail(ts_tmp[[cList]]$SSB, 1)/SSB0, 
                                    grad = tmp_mod$maximum_gradient_component)
    print(cList)
    cList = cList + 1
  }
  
}


# Organize data for plot ---------------------------------------------------

# Clean data (no convergence):
quant_data0 = dplyr::bind_rows(quant_tmp)
ts_data0 = dplyr::bind_rows(ts_tmp)
ts_data0 = left_join(quant_data0, ts_data0)

delete_data = quant_data0[quant_data0$grad > min_grad, ]
freq1 = as.data.frame(table(delete_data$em))
freq2 = as.data.frame(table(quant_data0$em))
freq3 = merge(freq1, freq2, by = 'Var1')
freq3$conv_rate = (1 - freq3$Freq.x/freq3$Freq.y)*100 # convergence rate (%)
# delete rows:
quant_data1 = quant_data0[quant_data0$grad <= min_grad, ]
ts_data1 = ts_data0[ts_data0$grad <= min_grad, ]
quant_data2 = tidyr::gather(data = quant_data1, key = 'variable', 'value', 3:4)
ts_data2 = tidyr::gather(data = ts_data1, key = 'variable', 'value', 6:8)
ts_data3 = ts_data2 %>% 
              group_by(year, em, variable) %>%
              summarise(mean = mean(value), p1 = quantile(value, p = 0.025), 
                        p2 = quantile(value, p = 0.975))


# Plots -------------------------------------------------------------------

ggplot(ts_data3, aes(x = year, y = mean, ymin=p1, ymax=p2)) +
  labs(y = '') +
  geom_ribbon(alpha=.3) + 
  geom_line(lwd=1) +
  theme_bw() +
  facet_grid(variable ~ em, scales = 'free_y')
ggsave(filename = 'compare_ts.png', width = 190, height = 220, units = 'mm', dpi = 500)

