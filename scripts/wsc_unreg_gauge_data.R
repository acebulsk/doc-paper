library(tidyhydat)
library(sf)
library(plyr)
library(dplyr)
library(tmap)
library(ggforce)
library(ggplot2)

# script to pull appropriate wsc stations from hydat for dcwbm comparison within the NCPTR. shapefiles for wsc gauges arent available as a single unit yet had to combine this layer here https://open.canada.ca/data/en/dataset/0c121878-ac23-46f5-95df-eb9960753375 with some sent by erika and some more by foundary spatial. 

stn_reg <- hy_stn_regulation() %>% 
  dplyr::rename(reg_year_from = Year_from,
                reg_year_to = Year_to)

stn_range <- hy_stn_data_range() 

stn_meta_all <- hy_stations() %>% 
  left_join(stn_reg) %>% 
  left_join(stn_range) 

stn_meta_fltr <- stn_meta_all %>% 
  left_join(stn_reg) %>% 
  left_join(stn_range) %>% 
  filter(
    PROV_TERR_STATE_LOC %in% c("BC", "YT", "AK"),
    # REGULATED == F, #this removed the englishman and zeballos which still have 10 yrs flow dat before reg. 
    Year_from <= 2000, # stations starting after 2000 will not have 10 yrs within 1981-2010
    RECORD_LENGTH >= 10, # need at least 10 yrs will constrain within 1981-2010 later  
    DATA_TYPE == "Q",
    DRAINAGE_AREA_GROSS > 0,
    is.na(DRAINAGE_AREA_GROSS)==F,
    is.na(STATION_NUMBER)==F,
    is.na(REGULATED) == F,
    REGULATED == F | (REGULATED == T & reg_year_from > 1990)) # keep stations that were regulated after 1990. Might have 10 years of data prior to regulation. Need to clip individually later by each stns reg from date. 

# these bsns need to be trimmed to their regulation start date 
stn_reg_fltr <- stn_meta_fltr %>% 
  filter(REGULATED == T & reg_year_from > 1990)

stns_for_later <- stn_reg_fltr$STATION_NUMBER

st.stn_meta_all <- stn_meta_all %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) 

st.stn_meta_fltr <- stn_meta_fltr %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) 

st_crs(st.stn_meta_all) <- 4326
st_crs(st.stn_meta_fltr) <- 4326

# transboundary basins

npctr_bsns <- read_sf("D:/transboundary_hakai/deliverables/paper-results/GIS/HUC_FinalBasins_AK_CAN_CUS.shp") %>% 
  st_transform(st_crs(st.stn_meta_fltr)) %>% 
  filter(Name != "Fraser River",
         Name != "Columbia River")

# get wsc gauges inside the NPCTR

st.filter <- st.stn_meta_fltr %>% 
  st_filter(npctr_bsns)


# map 

# tm_shape(npctr_bsns) +
#   tm_polygons() +
#   tm_shape(st.filter) +
#   tm_dots()

# filtered spatially

basins_fltr <- st.filter$STATION_NUMBER

# grab monthly flow normals but still need to filter by at least 10 yrs data
monthly_normals <- read_csv("D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_raw.csv")
monthly_normals <- ldply(basins_fltr, function(x) hy_monthly_flows(station_number = x)) 

# write.csv(monthly_normals, "D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_raw.csv")


# apply more filters

normal_fltr <- monthly_normals %>% 
  filter(
    Year >= 1981,
    Year <= 2010,
    Full_Month == T,
    Sum_stat == "MEAN"
  ) 

# remove regulated yrs for regulated stations 
zeballos <- normal_fltr %>% 
  filter(STATION_NUMBER == "08HE006", 
         Year < 2009)

englishman <- normal_fltr %>% 
  filter(STATION_NUMBER == "08HB002", 
         Year < 2000)

# add back filtered stations 
normal_fltr_unreg <- normal_fltr %>% 
  filter(!STATION_NUMBER %in% c("08HE006", "08HB002")) %>% # out w. old
  rbind(zeballos, englishman) # in w. new

# check number of months of data in each year and  total number of months for at least 10 yrs 

month_count <- normal_fltr_unreg %>% 
  group_by(STATION_NUMBER) %>% 
  tally() %>% 
  filter(n >= 120) # need at least 10 years of data


# its okay if most years have all months but notokay if all years are missing months 
month_in_yr_count <- normal_fltr_unreg %>% 
  filter(STATION_NUMBER %in% month_count$STATION_NUMBER) %>% 
  dplyr::group_by(STATION_NUMBER, Year) %>% 
  tally() %>% 
  dplyr::group_by(STATION_NUMBER) %>% 
  dplyr::summarise(n = mean(n)) %>% 
  filter(n > 10) # basins with an average of over 10 months in each yr; discludes 3 basins that do not have any full years of data

# filter out basins with missing months or removed via joels legacy script. mostly redundant from above
bad_basins <- c("08FC005", # looks like its regulated on sat imagery and has very low flows
                "08EE003", # no years with all months
                "08OA004", # no yrs with all months 
                "08OA005", # no yrs with all months 
                "08EE025" # joel removed previously - maybe bad basin boundary 
)


normal_fltr_updte <- normal_fltr_unreg %>% 
  filter(STATION_NUMBER %in% month_in_yr_count$STATION_NUMBER,
         !STATION_NUMBER %in% bad_basins)

# 101 basins
final_count <- length(unique(normal_fltr_updte$STATION_NUMBER))

# show start and end year for each station
start_end_meta <- normal_fltr_updte %>% 
  dplyr::group_by(STATION_NUMBER) %>% 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1)

published <- read_csv("D:/transboundary_hakai/deliverables/paper-results/dcwbm_observed_all_long_Basins_update_2021.csv") %>% 
  left_join(start_end_meta, by = c("ID" = "STATION_NUMBER")) %>% 
  mutate(start_year = coalesce(start_year.x, start_year.y),
         end_year = coalesce(end_year.x, end_year.y),
         record_length = coalesce(record_length.x, record_length.y)) %>% 
  select(-c(start_year.x:record_length.y))

write_csv(published, "D:/transboundary_hakai/deliverables/paper-results/dcwbm_observed_all_long_Basins_update_2021.csv")

test <- normal_fltr_updte %>% 
  filter(STATION_NUMBER == "08EE003")


# plot
ggplot(test, aes(Month, Value, group = Year, colour = factor(Year))) +
  #geom_point() +
  # geom_jitter()+
  geom_line()  

ggplot(normal_fltr_updte, aes(Month, Value, group = Year, colour = factor(Year))) +
  #geom_point() +
  # geom_jitter()+
  geom_line() +
  facet_wrap_paginate(~STATION_NUMBER, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE), ncol = 2, nrow = 6, page = 9)  

# map

st.filter_final <- st.filter %>% 
  filter(STATION_NUMBER %in% normal_fltr_updte$STATION_NUMBER) %>% 
  dplyr::rename(PROV = PROV_TERR_STATE_LOC,
                AREA = DRAINAGE_AREA_GROSS) %>% 
  select(-DRAINAGE_AREA_EFFECT)

tm_shape(st.filter_final) +
  tm_dots()

#st_write(st.filter_final, "D:/transboundary_hakai/deliverables/paper-results/GIS/wsc_unregulated_update_2021_3.shp", append = T)

wsc_gage_dat <- st_read("D:/transboundary_hakai/deliverables/paper-results/GIS/wsc_unregulated_update_2021_3.shp")

# meta filter
stn_meta_final <- stn_meta_fltr %>% 
  filter(STATION_NUMBER %in% normal_fltr_updte$STATION_NUMBER) 

# which basins arent included compared to before? It just BULKLEY RIVER NEAR HOUSTON which does not have data for a full yr ever
wsc_bsn_q <- st_read("D:/transboundary_hakai/scratch/unreg_Gauged_Data/wsc_data/WSC_polygons_qGaged_IP_IDs.shp") %>%
  filter(!WSC_cap %in% st.filter_final$STATION_NUMBER)

# get old wsc points at the outlet
wsc_bsn_q <- st_read("D:/transboundary_hakai/scratch/unreg_Gauged_Data/wsc_data/WSC_polygons_qGaged_IP_IDs.shp")

wsc_unreg_prior <- st.stn_meta_all %>%
  filter(STATION_NUMBER %in%  wsc_bsn_q$WSC_cap) %>%
  filter(DATA_TYPE == "Q") %>%
  dplyr::rename(PROV = PROV_TERR_STATE_LOC,
                AREA = DRAINAGE_AREA_GROSS) %>%
  select(-DRAINAGE_AREA_EFFECT)

# st_write(wsc_unreg_prior, "D:/transboundary_hakai/deliverables/paper-results/GIS/wsc_unregulated_prior_2020_2.shp", append = T)

# get largest list of basins we have 

wsc_bsns_all <- st_read("D:/transboundary_hakai/scratch/reg_Gauged_Data/wsc_all_basins.shp") 

new_wsc_gauges_no_bsn <- st.filter_final %>% filter(!STATION_NUMBER %in% wsc_bsns_all$ID) %>% select(STATION_NUMBER, STATION_NAME)

#st_write(new_wsc_gauges_no_bsn, "D:/transboundary_hakai/deliverables/paper-results/GIS/new_wsc_gauges_no_bsn.shp")

tmap_mode("view")

tm_shape(wsc_bsns_all) +
  tm_polygons() +
  tm_shape(new_wsc_gauges_no_bsn) +
  tm_dots()

# new basins from NHC 2021 missing seymour, mamquam, clayton 
nhc_meta <- read.csv("D:/transboundary_hakai/scratch/WSC_Basins_NHC_2021/Appendix 5.3 GIS Data/20200817_3004476_NHC_station_metadata.R0.csv") %>% 
  dplyr::rename(ID = Gauge.ID)

nhc_bsns <- st_read("D:/transboundary_hakai/scratch/WSC_Basins_NHC_2021/Appendix 5.3 GIS Data/20200811_3004476_NHC_all_watersheds.R0.shp")

nhc_bsns[nhc_bsns$ID == "15041200", "ID"] <- "08BB005" # conv usgs id to wgs id to match Q dat

nhc_bsns[nhc_bsns$ID == "15024800", "ID"] <- "08CF003" # conv usgs id to wgs id to match Q dat


nhc_ids <- nhc_bsns %>% st_drop_geometry()

# has seymout and mamquam
raw_ec_bsns <- st_read("D:/transboundary_hakai/deliverables/paper-results/GIS/raw_ec_basins_BC.shp") %>% 
  st_transform((st_crs(nhc_bsns))) %>% 
  select(ID = Station, Name = StatnNm) %>% 
  filter(!ID %in% nhc_bsns$ID)

# has clayton
foundry_sptl_bsns <- st_read("D:/transboundary_hakai/scratch/reg_Gauged_Data/basins_for_bill_floyd_from_FoundrySpatial/basins_for_bill_floyd/assigned_basins.shp") %>% 
  st_transform((st_crs(nhc_bsns))) %>% 
  select(ID = station_nu, Name = station_na) %>% 
  filter(!ID %in% nhc_bsns$ID)

# combine basins
all_bsns <- rbind(nhc_bsns, raw_ec_bsns, foundry_sptl_bsns) %>% 
  left_join(nhc_meta, by = "ID")

# missing PITMAN RIVER NEAR THE MOUTH, STIKINE RIVER ABOVE GRAND CANYON, STIKINE RIVER ABOVE BUTTERFLY CREEK

missing_basins <- wsc_gage_dat %>% filter(!STATION_NU %in% all_bsns$ID) 

wsc_bsns_with_dat <- all_bsns %>% filter(ID %in% wsc_gage_dat$STATION_NU) #%>% st_drop_geometry()

# write out updated basin file for 2021 
st_write(wsc_bsns_with_dat, "D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_basins.shp")

# convert to mm 

mdays = data.frame(month = c(1:12), mdays = c(31,28.25,31,30,31,30,31,31,30,31,30,31))

area <- stn_meta_final %>% 
  select(STATION_NUMBER, DRAINAGE_AREA_GROSS) %>% 
  mutate(area_m2 = DRAINAGE_AREA_GROSS * 1000000)

flowdepth_unreg <- normal_fltr_updte %>% 
  dplyr::left_join(area) %>% 
  dplyr::mutate(runoff = Value * 1/area_m2 *60*60*24*No_days *1000) %>% 
  dplyr::group_by(STATION_NUMBER, Month) %>% 
  dplyr::summarise(
    area_km2 = first(DRAINAGE_AREA_GROSS),
    runoff = mean(runoff),
    Q = mean(Value)
  )
write.csv(flowdepth_unreg, "D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_long.csv")

flowdepth_wide <- read.csv("D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_long.csv") %>% 
  pivot_wider(c(STATION_NUMBER, area_km2), names_from = Month, values_from = runoff) %>% 
  mutate(ann_mm = rowSums(.[,3:14, drop=TRUE], na.rm = TRUE)) %>% 
  rename(ID = STATION_NUMBER, area = area_km2)

colnames(flowdepth_wide)[3:14] <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(flowdepth_wide, "D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_wide.csv")


# compare to wsc old flows
month_num <- data.frame(num = c(1:12), month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

old_wsc_flows <- wsc_bsn_q %>% 
  st_drop_geometry() %>% 
  pivot_longer(Jan:Dec, names_to = "month", values_to = "runoff_old") %>% 
  left_join(month_num, by = "month") %>% 
  select(ID = WSC_cap, month = num, runoff_old) 

old_reg <- st_read("D:/transboundary_hakai/scratch/reg_Gauged_Data/wsc_all_basins.shp")  %>%
  st_drop_geometry() %>% 
  pivot_longer(Jan:Dec, names_to = "month", values_to = "runoff_old") %>% 
  left_join(month_num, by = "month") %>% 
  select(ID, month = num, runoff_old) 

new_wsc_flows <- read.csv("D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_long.csv") %>% 
  select(ID = STATION_NUMBER, month = Month, runoff_new = runoff)

all <- inner_join(new_wsc_flows, old_reg, by = c("ID", "month")) %>% 
  mutate(diff = runoff_old - runoff_new)

ggplot(all, aes(x = runoff_old, y = runoff_new)) +
  geom_point()
