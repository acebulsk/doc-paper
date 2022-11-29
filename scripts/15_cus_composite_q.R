# 1981-2020 Composite Flow Normal Calculation

# this script adds the gauged portion to each basin, to create a composite 
# (i.e. gauged + modelled) discharge for each basin, originally this was done
# using a mix of arcgis and R and here we will reproduce the same dataset using R in one 
# reproducible workflow. The original file is loaded in first so we can compare at the end.

# the original workflow is on the CHRL backup SSD at and output files to
# /media/alex/Seagate Backup Plus Drive/Q/archive/transboundary_hakai/scripts
# /media/alex/Seagate Backup Plus Drive/Q/archive/transboundary_hakai/scripts/cus_compute_monthlyGagedModelled_hybridBasins.R
# /media/alex/Seagate Backup Plus Drive/Q/archive/transboundary_hakai/scripts/bcak_compute_monthlyGagedModelled_hybridBasins.R

library(tidyverse)
library(sf)

# days in a month
mdays = data.frame(Month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), mdays = c(31,28.25,31,30,31,30,31,31,30,31,30,31))

# load data #### 

# original gauged data that was used before we found additional data

wsc_old <- read.csv('data/gauged/wsc/regulated/wsc_flowDat_withRegulated_old_file_from_JT_script.csv')

usgs_old <- st_read('data/gauged/usgs/usgs_data_croix/cus_usgs_basins_huc_IP_ID.shp') |> 
  st_drop_geometry() 

# npctr basins 
npctr_bsn <- st_read('data/gis/npctr_basins/npctr_basins.shp')

# get observed data with regulated stations included
wsc <- read.csv('data/gauged/wsc/regulated/wsc_regulated_1981_2010_flows_long.csv') |> 
  select(GAUGEID = STATION_NUMBER, Month, area = area_km2, runoff, Q, start_year, end_year, record_length)

usgs <- read.csv('data/gauged/usgs/regulated/usgs_regulated_1981-2010_long.csv') |> 
  select(GAUGEID = site_no, Month = month_nu, area, runoff, Q, start_year, end_year, record_length) |> 
  mutate(area = area / 1e6)

all_flows <- rbind(wsc, usgs)

# basin polygons including regulated gauges that have already been filtered

reg_bsn <- st_read('data/composite/all_regulated_basins_no_overlap_v4.shp')

reg_bsn_ipid <- read.csv('data/composite/reg_ip_id_filter.csv')

# get V2 modelled data which was calibrated by Amrit

mod <- st_read("data/modelled/CUS_Runoff_Gridded.csv",
               options=c("AUTODETECT_TYPE=YES",
                         "X_POSSIBLE_NAMES=Long",
                         "Y_POSSIBLE_NAMES=Lat")) |> 
  st_set_crs(4326) |> 
  st_transform(st_crs(npctr_bsn)) |> 
  st_join(npctr_bsn |> select(IP_ID))

names(mod) <- c('ID', 'Lat', 'Long', 'Elev', 1:12, 'IP_ID', 'geometry')

# join basin IDs to modelled grid ####

mod_gauge_ids <- mod |> 
  st_join(reg_bsn)

# get the modelled points that are gauged
pts_gaugeids <- mod_gauge_ids |>
  st_drop_geometry() |> 
  filter(is.na(GAUGEID) == F) 

# get the modelled points with no gauge
ungauge_pts <- mod |> 
  filter(!ID %in% pts_gaugeids$ID)

# we have some duplicate points but they are assigned different ip_ids so we do not double count
dup_check <- ungauge_pts |> 
  group_by(Lat, Long) |> 
  filter(n()>1)

ungauge_ipid <- ungauge_pts  |> 
  # slice(1:1000) |>
  group_by(IP_ID) |> 
  summarise(
    area = n() * 400 * 400, # area below gauge + some pts above maybe
    across(`1`:`12`, ~mean(.x, na.rm = TRUE))) |> 
  pivot_longer(`1`:`12`) |> 
  mutate(name = as.numeric(name)) |> 
  left_join(mdays, by = c('name' = 'Month')) |> 
  mutate(q_mod_cms = (value * (1e6 * area) * (1/1000000000) * (1/(mdays*24*60*60)))) |> # mm to m3s 
  select(IP_ID, Month = name, area, runoff = value, Q = q_mod_cms)

length(unique(ungauge_ipid$IP_ID)) # we have all 6690 dont need to hack in fraser columbia 

write.csv(ungauge_ipid |> st_drop_geometry(), 'data/composite/cus_flows_ungauged_npctr_bsns_chunks_NO_columbia.csv', row.names = F)

# join hakai ipids on the reg gauged data and then average all of the gauges within each ip_id

all_flows_ipid <- all_flows |> 
  inner_join(reg_bsn_ipid, by = 'GAUGEID') |> 
  group_by(IP_ID, Month) |> 
  summarise(
    area = sum(area),
    Q = sum(Q)) 

# composite final 
composite_raw <- left_join(ungauge_ipid, all_flows_ipid, by = c("IP_ID", "Month"), suffix = c("_sim", "_obs")) |> 
  mutate(area_sim = area_sim / 1e6)

composite_summ <- composite_raw |> 
  mutate(comp_area = area_sim + area_obs, 
         comp_q = Q_sim + Q_obs) |> 
  select(IP_ID, Month, comp_area, comp_q) |> 
  left_join(mdays) |> 
  mutate(comp_mm = comp_q * (1/(comp_area*1e6)) * (mdays * 24 * 60 * 60) * 1000) |> 
  filter(is.na(comp_mm) == F) |> 
  st_drop_geometry()

write.csv(composite_summ, 'data/composite/cus_composite_flows.csv', row.names = F)
