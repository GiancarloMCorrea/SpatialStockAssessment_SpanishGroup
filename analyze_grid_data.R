# Clean workspace:
rm(list = ls())

# Set working directory
mainDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
setwd(mainDir)

# Libraries:
require(dplyr)
require(tidyr)
require(reshape2)
# -------------------------------------------------------------------------
# Read data from Github:
this_url = "https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/blob/main/data/YFT_221cell_observations_1-100_ESS_00x.RData"
mydata = Rfssa::load_github_data(github_data_url = this_url)


# CPUE data ---------------------------------------------------------------
save_total = list()
for(i in seq_along(mydata)) {
  
  this_data = get(mydata[i])

  Lat_grid = data.frame("lat" = paste0("r",as.character(seq(1,13))),
                        "Lat" = as.numeric(this_data$layers$`layer[latitude]`$data[,1]))
  Lon_grid = data.frame("lon" = paste0("c",as.character(seq(1,17))),
                        "Lon" = as.numeric(this_data$layers$`layer[longitude]`$data[1,]))
  region_matrix = this_data$layers$`layer[region]`$data
  rownames(region_matrix) = paste0("r",as.character(seq(1,13)))
  colnames(region_matrix) = paste0("c",as.character(seq(1,17)))
  region_info = melt(region_matrix)
  region_info$index = paste0(region_info$Var1, '-', region_info$Var2)
  
  # CPUE data:
  save_tmp = list()
  pos_cpue = grep(pattern = 'simulated_cpue_ll_jpn_', x = names(this_data$obs))
  for (y in seq_along(pos_cpue)) {
    
    tmp_data = this_data$obs[[pos_cpue[y]]]
    this_cpue_data = data.frame(year = as.numeric(tmp_data$data$year), 
                                LatLon = tmp_data$data$obs[,1],
                                cpue = as.numeric(tmp_data$data$obs[,2]))
    save_tmp[[y]] = this_cpue_data
    
    
  }
  
  all_cpue = dplyr::bind_rows(save_tmp)
  all_cpue$region = region_info$value[match(all_cpue$LatLon, region_info$index)]
  
  all_cpue2 = all_cpue %>%
                separate(LatLon, c("lat", "lon"), "-")
  all_cpue2$lat2 = Lat_grid$Lat[match(all_cpue2$lat, Lat_grid$lat)]
  all_cpue2$lon2 = Lon_grid$Lon[match(all_cpue2$lon, Lon_grid$lon)]
  all_cpue2$sim = mydata[i]
  
  save_total[[i]] = all_cpue2
}

out_cpue = dplyr::bind_rows(save_total)
write.csv(out_cpue, 'CPUE_grid_data.csv', row.names = FALSE)

# Catch data ---------------------------------------------------------------
save_total = list()
for(i in seq_along(mydata)) {
  
  this_data = get(mydata[i])
  
  lat_matrix = melt(this_data$layers$`layer[latitude]`$data)
  lon_matrix = melt(this_data$layers$`layer[longitude]`$data)
  region_matrix = this_data$layers$`layer[region]`$data
  rownames(region_matrix) = paste0("r",as.character(seq(1,13)))
  colnames(region_matrix) = paste0("c",as.character(seq(1,17)))
  region_info = melt(region_matrix)
  region_info$index = paste0(region_info$Var1, '-', region_info$Var2)
  
  # Catch data:
  save_tmp = list()
  pos_cpue = grep(pattern = 'fishing_', x = names(this_data$layers))
  for (k in seq_along(pos_cpue)) {
    
    this_name = names(this_data$layers)[pos_cpue[k]]
    fishery_name = strsplit(x = this_name, split = '_')[[1]][2]
    year = as.numeric(gsub(pattern = ']', replacement = '', x = strsplit(x = this_name, split = '_')[[1]][3]))
    tmp_data = this_data$layers[[pos_cpue[k]]]
    catch_data = melt(tmp_data$data)
    this_cpue_data = data.frame(catch = catch_data$value,
                                lon = lon_matrix$value,
                                lat = lat_matrix$value,
                                year = year,
                                fishery = fishery_name,
                                region = region_info$value)
    save_tmp[[k]] = this_cpue_data
    
  }
  
  all_cpue = dplyr::bind_rows(save_tmp)
  all_cpue$sim = mydata[i]
  
  save_total[[i]] = all_cpue
  print(i)
}

out_catch = dplyr::bind_rows(save_total)

write.csv(out_catch, 'Catch_grid_data.csv', row.names = FALSE)

