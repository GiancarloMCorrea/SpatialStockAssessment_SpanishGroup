
## Libraries ---------------------------------------------------
rm(list = ls())
library(r4ss)
require(stringr)
require(dplyr)
require(ggplot2)

# Set working directory to source file location
saveFolder = 'replicates_data' # folder to save produced RData

# Read model outputs for Y 1A model:
load('replicates_data/Y_1A_25_final_all_replicates.RData')
Y_1A = save_data
Y_1A$dq$em = '1A_25_Y'
Y_1A$ts$em = '1A_25_Y'
Y_1A$fish$em = '1A_25_Y'
# Read model outputs for Y 4A model:
load('replicates_data/Y_4A_25_final_all_replicates.RData')
Y_4A = save_data
Y_4A$dq$em = '4A_25_Y'
Y_4A$ts$em = '4A_25_Y'
Y_4A$fish$em = '4A_25_Y'
Y_4A$mov$em = '4A_25_Y'
# Read model outputs for PY 1A and 4A model:
load('replicates_data/PY_all.RData')
PY_all = save_data

# Read F data
data_fish = rbind(Y_1A$fish, Y_4A$fish, PY_all$fish)

# Info:
PYvector = 1001:1256 # could be any vector (decimals to include seasons)
Yvector = rep(x = seq(from = 1952, to = 2015, by=1), each = 4)
max_grad = 5

# Convergence -------------------------------------------------------------
# Merge data in one dataset:
data_dq = rbind(Y_1A$dq, Y_4A$dq, PY_all$dq)
data_conv = data_dq[data_dq$Season == 1 & data_dq$Area == 1, ] # only values for season/area = 1 (only to find conv rates)

# Non-Convergence rate:
nonconv_rate = data_conv %>% dplyr::group_by(em) %>% dplyr::summarise(nonconvrate = sum((grad > max_grad) | is.nan(R0))/max(iter))
nonconv_rate$conv_rate = 1 - nonconv_rate$nonconvrate

# Save nonconvergent replicates:
data_conv = data_conv %>% mutate(replicate = paste0(em, '_', iter))
nonconv_rep = data_conv$replicate[which((data_conv$grad > max_grad) | is.nan(data_conv$R0))]

# Plot DQ -----------------------------------------------------------------

temp_dq = data_dq[!(data_dq$grad > max_grad | is.nan(data_dq$R0)), ] # remove nonconvergent replicates

# By season:
# temp_dq = temp_dq %>% dplyr::group_by(em, iter, Area) %>% dplyr::summarise(B0 = mean(B0), Bstatus = mean(Bstatus), R0 = sum(R0),
#                                                                          SSBmsy = mean(SSBmsy), Fmsy = mean(Fmsy), MSY = mean(MSY))
temp_dq = temp_dq[temp_dq$Season == 1, ]
# By area:
temp_dq = temp_dq %>% dplyr::group_by(em, iter) %>% dplyr::summarise(B0 = sum(B0), Bstatus = mean(Bstatus), R0 = sum(R0),
                                                             SSBmsy = mean(SSBmsy), Fmsy = mean(Fmsy), MSY = mean(MSY))
temp_dq = tidyr::gather(temp_dq, 'variable', 'value', 3:8)
data_plot = temp_dq %>% mutate(Type1 = gsub("\\_.*","",em), Type2 = gsub(".*_","",em))
data_plot$Type2 = factor(data_plot$Type2, levels = c('PY', 'Y'), labels = c('Pseudo-year', 'Year'))

# Make plot:
ggplot(data_plot, aes(x = factor(Type1), y = value)) +
  geom_boxplot(aes(fill = Type2)) +
  xlab('') + ylab('Value') +
  theme_bw() +
  guides(fill=guide_legend(title="Type")) +
  scale_fill_brewer(palette = 'Set1') +
  theme(legend.position = 'top') +
  facet_wrap(.~ variable, nrow = 2, scales = 'free_y') 

ggsave(filename = 'figures/replicates_dq.png', width = 190, height = 140, dpi = 500, units = 'mm')

# Plot time series -------------------------------------------------------------------

# Merge data in one dataset:
data_ts = rbind(Y_1A$ts, Y_4A$ts, PY_all$ts)
data_ts = data_ts %>% mutate(replicate = paste0(em, '_', iter))
data_ts = data_ts[!(data_ts$replicate %in% nonconv_rep), ] # remove nonconvergent replicates
data_ts$Yr = ifelse(test = data_ts$Yr < 1900, yes = Yvector[match(data_ts$Yr, PYvector)], no = data_ts$Yr) # from PY to Y

# By season
temp_ts = data_ts %>% dplyr::group_by(em, iter, Yr, Area) %>% dplyr::summarise(SSB = mean(SSB, na.rm = TRUE), TotB = mean(TotB), Rec = mean(Rec))
data_rec_area = temp_ts

# By Area
temp_ts = temp_ts %>% dplyr::group_by(em, iter, Yr) %>% dplyr::summarise(SSB = sum(SSB), TotB = sum(TotB), Rec = sum(Rec))
temp_ts = tidyr::gather(temp_ts, 'variable', 'value', 4:6)
temp_ts = temp_ts %>% dplyr::group_by(em, Yr, variable) %>% dplyr::summarise(q025 = quantile(value, probs = 0.025),
                                                           q50 = quantile(value, probs = 0.5),
                                                           q975 = quantile(value, probs = 0.975))
temp_ts = temp_ts %>% mutate(Type1 = gsub("\\_.*","",em), Type2 = gsub(".*_","",em))
data_plot = temp_ts[temp_ts$variable != 'TotB', ] # not show total biomass
spict_plot = temp_ts[temp_ts$variable == 'TotB', ] # for later
data_plot$Type2 = factor(data_plot$Type2, levels = c('PY', 'Y'), labels = c('Pseudo-year', 'Year'))

# Make plot:
ggplot(data_plot, aes(x = Yr, y = q50, ymin=q025, ymax=q975)) +
  xlab('') + ylab('') +
  geom_ribbon(aes(fill = Type2, color = Type2), alpha=.3) + 
  geom_line(aes(color = Type2), lwd=1) +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1') +
  theme_bw() +
  theme(legend.position = 'top') +
  facet_grid(variable~Type1, scales = 'free_y')

ggsave(filename = 'figures/replicates_ts.png', width = 190, height = 140, dpi = 500, units = 'mm')

# Make plot recruitment by area:
data_rec_area = data_rec_area %>% mutate(Type1 = gsub("\\_.*","",em), Type2 = gsub(".*_","",em))
data_rec_area = data_rec_area[data_rec_area$Type1 == '4A', ]

data_plot = data_rec_area %>% dplyr::group_by(Type1, Type2, Yr, Area) %>% dplyr::summarise(q025 = quantile(Rec, probs = 0.025),
                                                                         q50 = quantile(Rec, probs = 0.5),
                                                                         q975 = quantile(Rec, probs = 0.975))
data_plot$Type2 = factor(data_plot$Type2, levels = c('PY', 'Y'), labels = c('Pseudo-year', 'Year'))

# Make plot:
ggplot(data_plot, aes(x = Yr, y = q50, ymin=q025, ymax=q975)) +
  xlab('') + ylab('') +
  geom_ribbon(aes(fill = Type2, color = Type2), alpha=.3) + 
  geom_line(aes(color = Type2), lwd=1) +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1') +
  theme_bw() +
  theme(legend.position = 'top') +
  facet_wrap(.~Area, ncol = 2)

ggsave(filename = 'figures/replicates_recarea.png', width = 190, height = 140, dpi = 500, units = 'mm')

# Plots movement -----------------------------------------------

data_mov = rbind(Y_4A$mov, PY_all$mov)
data_mov = data_mov %>% mutate(replicate = paste0(em, '_', iter))
data_mov = data_mov[!(data_mov$replicate %in% nonconv_rep), ] # remove nonconvergent replicates

data_mov = data_mov %>% mutate(Type1 = gsub("\\_.*","",em), Type2 = gsub(".*_","",em))
data_mov = data_mov %>% mutate(Mov = paste0('From ', area1, ' to ', area2))

data_plot = tidyr::gather(data_mov, 'state', 'value', 6:7)
data_plot$Type2 = factor(data_plot$Type2, levels = c('PY', 'Y'), labels = c('Pseudo-year', 'Year'))

# Make plot:
ggplot(data_plot, aes(x = factor(state), y = value)) +
  geom_boxplot(aes(fill = factor(Type2))) +
  xlab('') + ylab('Rate') +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  guides(fill=guide_legend(title="Type")) +
  theme(legend.position = 'top') +
  facet_wrap(.~ Mov, nrow = 2, scales = 'free_y') 

ggsave(filename = 'figures/replicates_mov.png', width = 190, height = 140, dpi = 500, units = 'mm')

# Plot SPiCT --------------------------------------------------------------

load("SPiCT results/Bio_spict.RData")
colnames(Bio_spict_cpue_original) = 1:100
rownames(Bio_spict_cpue_original) = 1972:2015

spict_df = reshape2::melt(Bio_spict_cpue_original, varnames = c('Yr', 'iter'), value.name = 'TotB')
spict_df = spict_df %>% dplyr::group_by(Yr) %>% dplyr::summarise(q025 = quantile(TotB, probs = 0.025),
                                                             q50 = quantile(TotB, probs = 0.5),
                                                             q975 = quantile(TotB, probs = 0.975))
spict_df$Type1 = '1A'
spict_1A = spict_df
spict_df$Type1 = '4A'
spict_4A = spict_df

spict_all = rbind(spict_1A, spict_4A)
spict_all$Type2 = 'SPiCT'

data_plot = rbind(spict_all, spict_plot[,c('Yr', 'q025', 'q50', 'q975', 'Type1', 'Type2')])
data_plot$Type2 = factor(data_plot$Type2, levels = c('PY', 'Y', 'SPiCT'), 
                         labels = c('Pseudo-year', 'Year', 'SPiCT'))

# Make plot:
ggplot(data_plot, aes(x = Yr, y = q50, ymin=q025, ymax=q975)) +
  xlab('') + ylab('Biomass (mt)') +
  geom_ribbon(aes(fill = Type2, color = Type2), alpha=.3) + 
  geom_line(aes(color = Type2), lwd=1) +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1') +
  theme_bw() +
  theme(legend.position = 'top') +
  facet_wrap(.~Type1, scales = 'free_y')

ggsave(filename = 'figures/replicates_spict.png', width = 190, height = 140, dpi = 500, units = 'mm')
