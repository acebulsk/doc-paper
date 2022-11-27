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

# this file was from the DOC_REPORT file sent off for DOC modelling 
doc_q_original <- read.csv('data/composite/DCWBM_Runnoff_mm_composite_Master_DOC_IP_IDs_20200130.csv')

# npctr basins 
npctr_bsn <- st_read('../data/npctr_basins.shp')

# original gauged data that was used before we found additional data

wsc_old <- read.csv('data/gauged/wsc/wsc_flowDat_withRegulated_old_file_from_JT_script.csv')

usgs_old <- st_read('data/gauged/usgs/usgs_data_croix/cus_usgs_basins_huc_IP_ID.shp') |> 
  st_drop_geometry() 

# get observed data with regulated stations included
wsc <- read.csv('../data/regulated_gauge_flows/wsc_regulated_1991_2020_flows_long.csv') %>% 
  select(GAUGEID = STATION_NUMBER, Month, area = area_km2, runoff, Q, start_year, end_year, record_length)
usgs <- read.csv('../data/regulated_gauge_flows/usgs_regulated_1991-2020_long.csv') %>% 
  select(GAUGEID = site_no, Month = month_nu, area, runoff, Q, start_year, end_year, record_length) %>% 
  mutate(area = area / 1e6)

