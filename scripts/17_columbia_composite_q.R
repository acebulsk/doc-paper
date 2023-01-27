# this script calculates the 'composite' dischareg for the columbia basin as completed in early 2020
# see /media/alex/Seagate Backup Plus Drive/q-paper/archive/transboundary_hakai/dcwbmScripts_ac for original workflow
# this script was copied from the archive here for transparency and organization 

library(tidyverse)
library(sf)

columbia_id <- 14246900

usgs_meta <- readRDS('data/gauged/usgs/regulated/usgs_sites_metadata_all.rds') |> 
  select(site_no, station_nm) |> 
  mutate(site_no = as.numeric(site_no))

usgs_gauge_flows <- read.csv('data/gauged/usgs/regulated/usgs_regulated_1981-2010_long.csv') |> 
  filter(site_no == columbia_id)

mean(usgs_gauge_flows$Q) * (365*24*60*60) * (1/1e9)

# see /media/alex/Seagate Backup Plus Drive/q-paper/archive/transboundary_hakai/dcwbmScripts_ac for the rest... 
