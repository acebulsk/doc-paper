# this script takes the gridded data set and aggregates watershed average based on input polygon

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(plotly)

# get cus 
dcwbm_grid_new <- st_read('data/modelled/raw from amrit/DCWBM/DCWBM/CUS_Runoff_Gridded.csv', 
                          options=c("AUTODETECT_TYPE=YES", 
                                    "X_POSSIBLE_NAMES=Long",
                                    "Y_POSSIBLE_NAMES=Lat")) 
# set to wgs84
st_crs(dcwbm_grid_new) <- 4326

# get usgs gauged basins
usgs_bsn_q <- read.csv('data/gauged/usgs/usgs_1981_2010_monthly_flows_long.csv') 

usgs_bsn_sptl <- st_read('data/gis/usgs_gauge_basins/usgs_unregulated_basins_1981-2010_dataRetrieval_2022_update.shp') |> 
  st_transform(st_crs(4326)) |> 
  select(GAGE_ID, AREA) #just keep spatial for join purposes

usgs_bsn_area <- usgs_bsn_sptl |>
  select(GAGE_ID, AREA) |>  #just keep spatial for join purposes
  st_drop_geometry()

usgs_bsn_df <- usgs_bsn_q |> 
  st_drop_geometry()

# write.csv(usgs_bsn_df, "Gauged Compare/usgs_gaugeDat.csv")

join <- st_join(dcwbm_grid_new, usgs_bsn_sptl, join = st_intersects) |> 
  st_drop_geometry()

colnames(join)[5:16] <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# dcwbm_grid_new <- NULL

pts.avg <- join |> 
  filter(is.na(GAGE_ID) == F) |> 
  group_by(GAGE_ID) |> 
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
            Dec = mean(Dec, na.rm = T)) |>
  mutate(ann_mm = rowSums(.[,2:13, drop=TRUE], na.rm = TRUE)) 

write.csv(pts.avg, "data/modelled/dcwbm_raw_from_amrit_usgs_unreg_1981_2010.csv", row.names = F)

pts.avg <- read.csv("Gauged Compare/dcwbm2020_usgsPolyUnreg.csv")

###### convert to m3/s##### 

dcwbm_mm <- pts.avg |> 
  left_join(usgs_bsn_area, by = "GAGE_ID")

colnames(dcwbm_mm)[2:13] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

# days in a month
mdays = data.frame(Month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), mdays = c(31,28.25,31,30,31,30,31,31,30,31,30,31))

dcwbm_long <- dcwbm_mm |> 
  pivot_longer(`1`:`12`, names_to = "Month", values_to = "q") |> 
  mutate(ID = paste0(GAGE_ID, Month))

# coherce months to numeric
dcwbm_long$Month <- as.numeric(dcwbm_long$Month)

# convert mm to cms 
dcwbm_cms <- dcwbm_long |> 
  left_join(mdays, by = 'Month') |> 
  mutate(q_cms =  (((q/1000) * (AREA)) * (1/(mdays*24*60*60))),
         q_cky = (q/1000000) * (AREA/1000000))

write.csv(dcwbm_cms, "data/modelled/dcwbm_raw_from_amrit_usgs_unreg_1981_2010_cky.csv")

# gauge data
# usgs_bsn_df <- usgs_bsn_q |> 
#   st_drop_geometry() |> 
#   select(GAGE_ID, Jan:Dec, area = Shape_Area, regime, Name = Station_Na)
# 
# colnames(usgs_bsn_df)[2:13] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
# 
# usgs_long <- usgs_bsn_df |> 
#   pivot_longer(`1`:`12`, names_to = "Month", values_to = "q") |> 
#   mutate(ID = paste0(GAGE_ID, Month))
# 
# # coherce months to numeric
# usgs_long$Month <- as.numeric(usgs_long$Month)
# 
# # convert mm to cms 
# usgs_cms <- usgs_long |> 
#   left_join(mdays, by = 'Month') |> 
#   mutate(q_cms =  (((q/1000) * (1000000 * area)) * (1/(mdays*24*60*60))),
#          q_cky = (q/1000000) * (area/1000000))
# 
# write.csv(usgs_cms, "Gauged Compare/usgs_unreg_long_mm_cms_cky.csv")
# 
# # get usgs area in km3 
# wsc_area <- wsc_bsn_q |> 
#   st_drop_geometry() |> 
#   select(WSC, area)
# 
# #### plot ####
# usgs_mod <- read.csv("Gauged Compare/dcwbm2020_usgsPolyUnreg.csv")
# usgs_gauge <- read.csv("Gauged Compare/usgs_gaugeDat.csv") |>
#   filter(NDAMS_2009 == 0, GAGE_ID != 24) # set to unregulated - and remove clark basin because some sort of damming must be going on for those high summer flows. 
# 
# 
# # monthly
# mod_long <- usgs_mod |> 
#   pivot_longer(Jan:Dec, names_to = "Month", values_to = "mod_q") |> 
#   mutate(ID = paste0(GAGE_ID, Month))  
# 
# usgs_long <- usgs_gauge |> 
#   #st_drop_geometry() |> 
#   select(GAGE_ID, Station_Na, Jan:Dec, regime) |> 
#   pivot_longer(Jan:Dec, names_to = "Month", values_to = "wsc_q")|> 
#   mutate(ID = paste0(GAGE_ID, Month))  
# 
# combine <- left_join(usgs_long, mod_long, suffix = c("_obs", "_mod"), by = "ID")  
# 
# ggplot(combine, aes(x= wsc_q, y = mod_q)) +
#   geom_point(aes(colour = factor(regime))) +
#   labs(x='Observed (mm)', y = 'Modelled (mm)', colour = "Regime:") +
#   geom_abline(intercept = 0, slope = 1) +
#   xlim(0, 600) +
#   ylim(0, 600) +
#   scale_colour_brewer(palette="Dark2") +
#   theme_bw() 
# 
# ggplot(combine, aes(x= wsc_q, y = mod_q)) +
#   geom_point(aes(colour = factor(GAGE_ID_obs), shape = factor(regime))) +
#   labs(x='Observed (mm)', y = 'Modelled (mm)', colour = "Basin ID:", shape = "Regime:") +
#   geom_abline(intercept = 0, slope = 1) +
#   xlim(0, 600) +
#   ylim(0, 600) +
#   scale_colour_brewer(palette="Dark2") +
#   theme_bw() 
# 
# ggsave('Gauged Compare/cus_monthly_unregulated_amrit2020_2.png', width = 8, height = 6, units = "in")
# 
# plot_ly(
#   data = combine, 
#   x = ~wsc_q, y = ~mod_q, 
#   type = "scatter", 
#   mode = "markers", 
#   color = ~regime,
#   text = ~Station_Na,
#   hovertemplate = paste(
#     "<b>%{text}</b><br>",
#     "%{yaxis.title.text}: %{y:,.0f}<br>",
#     "%{xaxis.title.text}: %{x:,.0f}")) |> 
#   layout(
#     xaxis = list(title = "Observed (mm)"),
#     yaxis = list(title = "Modelled (mm)")
#   )
# 
# 
# # annual 
# 
# # plot annual mm
# usgs_ann <- usgs_gauge |> 
#   #st_drop_geometry() |> 
#   select(Station_Na, GAGE_ID, ann_mm, regime, NDAMS_2009)
# 
# mod_ann <- usgs_mod |> 
#   select(GAGE_ID, ann_mm_mod = ann_mm)
# 
# cus_combine_ann <- left_join(usgs_ann, mod_ann, by = "GAGE_ID") |> 
#   filter(NDAMS_2009 == 0, GAGE_ID != 24) |> # set to unregulated - and remove clark basin because some sort of damming must be going on for those high summer flows.
#   mutate(diff = (ann_mm - ann_mm_mod)) |> 
#   mutate(perc_diff = (ann_mm - ann_mm_mod)/ann_mm_mod)  
# 
# ggplot(combine_ann, aes(x= ann_mm, y = ann_mm_mod)) +
#   geom_point(aes(colour = factor(regime))) +
#   labs(x='Observed (mm)', y = 'Modelled (mm)', colour = "Regime:") +
#   geom_abline(intercept = 0, slope = 1) +
#   xlim(0, 4500) +
#   ylim(0, 4500) +
#   scale_colour_brewer(palette="Dark2") +
#   theme_bw() 
# 
# ggsave('Gauged Compare/cus_annual_unregulated_amrit2020.png', width = 8, height = 6, units = "in")
# 
# 
# plot_ly(
#   data = combine_ann, 
#   x = ~ann_mm, y = ~ann_mm_mod, 
#   type = "scatter", 
#   mode = "markers", 
#   color = ~regime,
#   text = ~Station_Na,
#   hovertemplate = paste(
#     "<b>%{text}</b><br>",
#     "%{yaxis.title.text}: %{y:,.0f}<br>",
#     "%{xaxis.title.text}: %{x:,.0f}")) |> 
#   layout(
#     xaxis = list(title = "Observed (mm)"),
#     yaxis = list(title = "Modelled (mm)")
#   )
# 
# 
# 
