# script to average dcwbm flows to npctr basins 

library(tidyverse)
library(sf)

# bring in old dataset to show here is the same 

old_q <- read.csv('data/modelled/for_comparison/dcwbm_v2.0_bcak_cus_allMod_hakaiBsns.csv')

npctr_bsn <- st_read('data/gis/npctr_basins/npctr_basins.shp')

mod_bcak <- st_read("data/modelled/HUC_Runoff_Gridded.csv",
               options=c("AUTODETECT_TYPE=YES",
                         "X_POSSIBLE_NAMES=Long",
                         "Y_POSSIBLE_NAMES=Lat")) |> 
  st_set_crs(4326) |> 
  st_transform(st_crs(npctr_bsn)) |> 
  st_join(npctr_bsn |> select(IP_ID)) |> 
  st_drop_geometry()

mod_cus <- st_read("data/modelled/CUS_Runoff_Gridded.csv",
               options=c("AUTODETECT_TYPE=YES",
                         "X_POSSIBLE_NAMES=Long",
                         "Y_POSSIBLE_NAMES=Lat")) |> 
  st_set_crs(4326) |> 
  st_transform(st_crs(npctr_bsn)) |> 
  st_join(npctr_bsn |> select(IP_ID)) |> 
  st_drop_geometry()

mod <- rbind(mod_bcak, mod_cus)

names(mod) <- c('ID', 'Lat', 'Long', 'Elev', month.abb, 'IP_ID')

npctr_bsn_flows <- mod |> 
  group_by(IP_ID) |> 
  summarise(across(Jan:Dec, ~mean(.x, na.rm = TRUE))) |> 
  left_join(npctr_bsn |> st_drop_geometry(), by= 'IP_ID')

npctr_bsn_flows$ann_mm <- rowSums(npctr_bsn_flows[,2:13, drop=TRUE], na.rm = TRUE)
npctr_bsn_flows$q_km3yr <- npctr_bsn_flows$ann_mm * (1/1000000) * (npctr_bsn_flows$Are_km2)

write.csv(npctr_bsn_flows, 'data/modelled/npctr_bsn_1981_2010_dcwbm_sim_q.csv', row.names = F)


jn <- inner_join(old_q, npctr_bsn_flows, by = 'IP_ID')

ggplot(jn, aes(ann_mm.x, ann_mm.y)) + geom_point()
