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

# load data #### 

# this file was from the DOC_REPORT file sent off for DOC modelling 
doc_q_original <- read.csv('data/composite/DCWBM_Runnoff_mm_composite_Master_DOC_IP_IDs_20200130.csv')

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

# get V2 modelled data which was calibrated by Amrit

mod <- st_read("data/modelled/HUC_Runoff_Gridded.csv",
                                 options=c("AUTODETECT_TYPE=YES",
                                           "X_POSSIBLE_NAMES=Long",
                                           "Y_POSSIBLE_NAMES=Lat")) |> 
  st_crs(4326) |> 
  st_transform(st_crs(npctr_bsn))

# join basin IDs to modelled grid ####

mod_gauge_ids <- mod |> 
  st_join(reg_bsn)




