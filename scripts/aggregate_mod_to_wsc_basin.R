# this script takes the gridded data set and aggregates watershed average based on input polygon

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(plotly)

path = 'D:/transboundary_hakai/Scratch/unreg_Gauged_Data'
setwd(path)


# # get pts not including fraser or cus
# dcwbm_grid_new <- st_read("D:/transboundary_hakai/amrit/HUC_Runoff_Gridded.csv", 
#                           options=c("AUTODETECT_TYPE=YES", 
#                                     "X_POSSIBLE_NAMES=Long",
#                                     "Y_POSSIBLE_NAMES=Lat")) 
# # set to wgs84
# st_crs(dcwbm_grid_new) <- 4326

# downsampled to 50 m 
dcwbm_grid_new <- st_read("dcwbm_pts_50m_6gauges.shp")

saveRDS(dcwbm_grid_new, "dcbwmPoints_50m.rds")

# points only have IDs join to flow depth later
dcwbm_grid_new <- readRDS("dcbwmPoints_50m.rds")

# get wsc basins that are unregulated from joels original dataset 
# wsc_bsn_q <- st_read("../scratch/unreg_Gauged_Data/wsc_data/WSC_polygons_qGaged_IP_IDs.shp") %>% 
#   st_transform(st_crs(4326)) 

# updated basins 2021
# nhc_meta <- read.csv("D:/transboundary_hakai/scratch/WSC_Basins_NHC_2021/Appendix 5.3 GIS Data/20200817_3004476_NHC_station_metadata.R0.csv") %>% 
#   dplyr::rename(ID = Gauge.ID)
# 
# nhc_bsns <- st_read("D:/transboundary_hakai/scratch/WSC_Basins_NHC_2021/Appendix 5.3 GIS Data/20200811_3004476_NHC_all_watersheds.R0.shp") %>% 
#   left_join(nhc_meta) %>% 
#   st_transform(4326)

# combo of nhc, foundry and env can
wsc_bsns1 <- sf::st_read("D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_basins.shp")
wsc_bsns2 <- sf::st_read("D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_basins.shp") %>% slice(50:n())

# join basin ID to points 
dcwbm_grid_new <- st_join(dcwbm_grid_new, wsc_bsns1, join = st_intersects) %>%
  st_drop_geometry()

# runoff data to join to points
flow_depths <- read_csv("D:/transboundary_hakai/amrit/HUC_Runoff_Gridded.csv")

colnames(join)[5:16] <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

dcwbm_grid_new <- NULL

pts.avg <- join %>%
  filter(is.na(ID.y) == F) %>%
  group_by(ID.y) %>%
  summarize(Jan = mean(Jan, na.rm = T),
            Feb = mean(Feb, na.rm = T),
            Mar = mean(Mar, na.rm = T),
            Apr = mean(Apr, na.rm = T),
            May = mean(May, na.rm = T),
            Jun = mean(Jun, na.rm = T),
            Jul = mean(Jul, na.rm = T),
            Aug = mean(Aug, na.rm = T),
            Sep = mean(Sep, na.rm = T),
            Oct = mean(Oct, na.rm = T),
            Nov = mean(Nov, na.rm = T),
            Dec = mean(Dec, na.rm = T)) %>%
  mutate(ann_mm = rowSums(.[,2:13, drop=TRUE], na.rm = TRUE)) %>% 
  rename(ID = ID.y)

write.csv(pts.avg, "2021_dcwbm_nhc_foundry_EC_wsc_usgs.csv", row.names = F)

pts.avg <- read.csv("Gauged Compare/dcwbm2020_wscPolyUnreg.csv") %>% select(WSC:ann_mm)


# ###### convert to m3/s##### 
# # get wsc area in km3 
# wsc_area <- wsc_bsn_q %>% 
#   st_drop_geometry() %>% 
#   select(WSC, area)
# 
# dcwbm_mm <- pts.avg %>% 
#   left_join(wsc_area, by = "WSC")
# 
# colnames(dcwbm_mm)[2:13] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
# 
# # days in a month
# mdays = data.frame(Month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), mdays = c(31,28.25,31,30,31,30,31,31,30,31,30,31))
# 
# dcwbm_long <- dcwbm_mm %>% 
#   pivot_longer(`1`:`12`, names_to = "Month", values_to = "q") %>% 
#   mutate(ID = paste0(WSC, Month))
# 
# # coherce months to numeric
# dcwbm_long$Month <- as.numeric(dcwbm_long$Month)
# 
# # convert mm to cms 
# dcwbm_cms <- dcwbm_long %>% 
#   left_join(mdays, by = 'Month') %>% 
#   mutate(q_cms =  (((q/1000) * (1000000 * area)) * (1/(mdays*24*60*60))),
#          q_cky = (q/1000000) * (area))
# 
# write.csv(dcwbm_cms, "Gauged Compare/bcak_dcwbm_long_mm_cms_cky.csv")
# 
# 
# # wsc convert
# wsc_mm <- wsc_bsn_q %>% 
#   st_drop_geometry() %>% 
#   select(WSC, Jan:Dec, area, regime)
# 
# colnames(wsc_mm)[2:13] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
# 
# wsc_long <- wsc_mm %>% 
#   pivot_longer(`1`:`12`, names_to = "Month", values_to = "q") %>% 
#   mutate(ID = paste0(WSC, Month))
# 
# # coherce months to numeric
# wsc_long$Month <- as.numeric(wsc_long$Month)
# 
# # convert mm to cms 
# wsc_cms <- wsc_long %>% 
#   left_join(mdays, by = 'Month') %>% 
#   mutate(q_cms =  (((q/1000) * (1000000 * area)) * (1/(mdays*24*60*60))),
#          q_cky = (q/1000000) * (area))
# 
# write.csv(wsc_cms, "Gauged Compare/wsc_unreg_long_mm_cms_cky.csv")

##### plot #####

# plot cms
# combine_cms <- left_join(wsc_cms, dcwbm_cms, suffix = c("_obs", "_mod"), by = "ID")
# 
# ggplot(combine_cms, aes(x= q_cms_obs, y = q_cms_mod)) +
#   geom_point() +
#   labs(x='Observed (m3/s)', y = 'Modelled (m3/s)') +
#   geom_abline(intercept = 0, slope = 1) +
#   xlim(0, 700) +
#   ylim(0, 700)
# 


# bind wsc and dcwbm to plot mm

mod <- read.csv("2021_dcwbm_nhc_foundry_EC_wsc_usgs.csv")
gauge <- read.csv("2021_UPDATED_wsc_1981-2010_monthly_normals_long.csv")


# monthly plots

month_num <- data.frame(num = c(1:12), month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

dcwbm_long <- mod %>% 
  pivot_longer(Jan:Dec, names_to = "month", values_to = "dcwbm_q") %>% 
  left_join(month_num, by = "month") %>% 
  select(ID, month = num, dcwbm_q)


wsc_long <- gauge %>% 
  select(ID = STATION_NUMBER, month = Month, gauge_q = runoff)

dat <- left_join(wsc_long, dcwbm_long, by = c("ID", "month"))  

ggplot(dat, aes(x= gauge_q, y = dcwbm_q)) +
  geom_point() +
  labs(x='Observed (mm)', y = 'Modelled (mm)') +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 700) +
  ylim(0, 700) +
  scale_colour_brewer(palette="Dark2") +
  theme_bw() 

ggsave('Gauged Compare/bcak_monthly_amrit2020.png', width = 8, height = 6, units = "in")


# plot annual mm
wsc_ann <- read.csv("D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_wide.csv") %>% 
  select(ID = STATION_NUMBER, ann_mm)

dcbwm_ann <- mod %>% 
  select(ID, ann_mm_mod = ann_mm)

combine_ann <- left_join(wsc_ann, dcbwm_ann, by = "ID") %>% 
  mutate(diff = (ann_mm - ann_mm_mod)) %>% 
  mutate(perc_diff = (ann_mm - ann_mm_mod)/ann_mm_mod)

ggplot(combine_ann, aes(x= ann_mm, y = ann_mm_mod)) +
  geom_point() +
  labs(x='Observed (mm)', y = 'Modelled (mm)', colour = "Regime:") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 4500) +
  ylim(0, 4500) +
  scale_colour_brewer(palette="Dark2") +
  theme_bw() 


plot_ly(
  data = combine_ann, 
  x = ~ann_mm, y = ~ann_mm_mod, 
  type = "scatter", 
  mode = "markers", 
  color = ~regime,
  text = ~Name,
  hovertemplate = paste(
    "<b>%{text}</b><br>",
    "%{yaxis.title.text}: %{y:,.0f}<br>",
    "%{xaxis.title.text}: %{x:,.0f}")) %>% 
  layout(
    xaxis = list(title = "Observed (mm)"),
    yaxis = list(title = "Modelled (mm)")
  )

# group by regime 
combine_cms <- left_join(wsc_cms, dcwbm_cms, suffix = c("_obs", "_mod"), by = "ID")

summarizeRegime <- combine_cms %>% 
  group_by(regime) %>% 
  summarise(across(c(q_cms_obs, q_cms_mod), mean))

ggplot(summarizeRegime, aes(x= q_cms_obs, y = q_cms_mod)) +
  geom_point(aes(colour = factor(regime))) +
  labs(x='Observed (mm)', y = 'Modelled (mm)', colour = "Regime:") +
  geom_abline(intercept = 0, slope = 1) +
  # xlim(0, 4500) +
  # ylim(0, 4500) +
  scale_colour_brewer(palette="Dark2") +
  theme_bw() 

