# script to pull appropriate wsc stations from hydat for dcwbm comparison within the NCPTR. 
#Most up to date wsc gauge basin shapefile is from Joel T/NHC as of 2021 - 06 - 24 had to add new basins from missing seymour, mamquam, clayton from https://open.canada.ca/data/en/dataset/0c121878-ac23-46f5-95df-eb9960753375 with some sent by erika and some more by foundary spatial. 

library(tidyhydat)
library(sf)
library(plyr)
library(dplyr)
library(tmap)
library(ggforce)
library(ggplot2)
library(tidyr)

# choose what set of gauge data to use i.e. for 1981-2010 compare or 1990-2020 compare
which_year_max <- 2010
which_year_min <- 1981

stn_reg <- hy_stn_regulation() %>% 
  dplyr::rename(reg_year_from = Year_from,
                reg_year_to = Year_to)

stn_range <- hy_stn_data_range() 

stn_meta_all <- hy_stations() %>% 
  left_join(stn_reg) %>% 
  left_join(stn_range) 

stn_meta_fltr <- stn_meta_all %>% 
  # left_join(stn_reg, by = "STATION_NUMBER") %>% 
  # left_join(stn_range, by = "STATION_NUMBER") %>% 
  filter(
    PROV_TERR_STATE_LOC %in% c("BC", "YT", "AK"),
    # REGULATED == F, #this removed the englishman and zeballos which still have 10 yrs flow dat before reg. 
    Year_from <= which_year_max - 10, # stations starting after which_year - 10 will not have 10 yrs within range
    Year_to >= which_year_min + 10,
    RECORD_LENGTH >= 10, # need at least 10 yrs will constrain within 1981-2010 later  
    DATA_TYPE == "Q",
    DRAINAGE_AREA_GROSS > 0,
    is.na(DRAINAGE_AREA_GROSS)==F,
    is.na(STATION_NUMBER)==F) # more filters later too

# these bsns need to be trimmed to their regulation start date 

st.stn_meta_all <- stn_meta_all %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) 

st.stn_meta_fltr <- stn_meta_fltr %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) 

st_crs(st.stn_meta_all) <- 4326
st_crs(st.stn_meta_fltr) <- 4326

# transboundary basins

npctr_bsns <- st_read("data/gis/npctr_basins/npctr_basins_valid_no_fr_clm.shp")

# npctr_bsns_wBig2 <- st_read("../data/npctr_basins.shp") %>% 
#   st_transform(st_crs(st.stn_meta_fltr)) %>% 
#   # filter(Name != "Fraser River",
#   #        Name != "Columbia River") %>%
#   st_make_valid() 

# get wsc gauges inside the NPCTR

st.filter <- st.stn_meta_fltr %>% 
  st_filter(npctr_bsns)

# st.filter_wBig2 <- st.stn_meta_fltr %>% 
#   st_filter(npctr_bsns_wBig2)

# map 

# tm_shape(npctr_bsns_wBig2 %>% st_simplify(dTolerance = 400)) +
#   tm_polygons() +
#   tm_shape(st.filter_wBig2) +
#   tm_dots()

# filtered spatially

basins_fltr <- c(st.filter$STATION_NUMBER, "08MF005") # add fraser too 

# grab monthly flow normals but still need to filter by at least 10 yrs data
monthly_normals <- ldply(basins_fltr, function(x) hy_monthly_flows(station_number = x)) 



# apply more filters

normal_fltr <- monthly_normals %>% 
  filter(
    Year >= which_year_min,
    Year <= which_year_max,
    Full_Month == T,
    Sum_stat == "MEAN"
  ) 

# check number of months of data in each year and  total number of months for at least 10 yrs 

month_count <- normal_fltr %>% 
  group_by(STATION_NUMBER) %>% 
  tally() %>% 
  filter(n >= 120) # need at least 10 years of data


# its okay if most years have all months but notokay if all years are missing months 
month_in_yr_count <- normal_fltr %>% 
  filter(STATION_NUMBER %in% month_count$STATION_NUMBER) %>% 
  dplyr::group_by(STATION_NUMBER, Year) %>% 
  tally() %>% 
  dplyr::group_by(STATION_NUMBER) %>% 
  dplyr::summarise(n = mean(n)) %>% 
  filter(n > 10) # basins with an average of over 10 months in each yr; discludes 3 basins that do not have any full years of data


normal_fltr_updte <- normal_fltr %>% 
  filter(STATION_NUMBER %in% month_in_yr_count$STATION_NUMBER)

# 101 basins unregulated and 17 regulated
final_count <- length(unique(normal_fltr_updte$STATION_NUMBER))

# show start and end year for each station
start_end_meta <- normal_fltr_updte %>% 
  dplyr::group_by(STATION_NUMBER) %>% 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1)

# published <- read_csv("D:/transboundary_hakai/deliverables/paper-results/dcwbm_observed_all_long_Basins_update_2021.csv") %>% 
#   left_join(start_end_meta, by = c("ID" = "STATION_NUMBER")) %>% 
#   mutate(start_year = coalesce(start_year.x, start_year.y),
#          end_year = coalesce(end_year.x, end_year.y),
#          record_length = coalesce(record_length.x, record_length.y)) %>% 
#   select(-c(start_year.x:record_length.y))
# 
# write_csv(published, "D:/transboundary_hakai/deliverables/paper-results/dcwbm_observed_all_long_Basins_update_2021.csv")

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
  facet_wrap_paginate(~STATION_NUMBER, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE), ncol = 2, nrow = 6, page = 7)  

# map

st.filter_final <- st.filter %>% 
  filter(STATION_NUMBER %in% normal_fltr_updte$STATION_NUMBER) %>% 
  dplyr::rename(PROV = PROV_TERR_STATE_LOC,
                AREA = DRAINAGE_AREA_GROSS) %>% 
  select(-DRAINAGE_AREA_EFFECT)

tm_shape(st.filter_final) +
  tm_dots()

st_write(st.filter_final, "data/gauged/wsc/regulated/wsc_regulated_points.shp")

st.filter_final <- st_read("data/gauged/wsc/regulated/wsc_regulated_points.shp")

# meta filter
stn_meta_final <- stn_meta_fltr %>% 
  filter(STATION_NUMBER %in% normal_fltr$STATION_NUMBER) 

# convert to mm 

mdays = data.frame(month = c(1:12), mdays = c(31,28.25,31,30,31,30,31,31,30,31,30,31))

area <- stn_meta_final %>% 
  select(STATION_NUMBER, DRAINAGE_AREA_GROSS) %>% 
  mutate(area_m2 = DRAINAGE_AREA_GROSS * 1000000)


record_length <- normal_fltr_updte %>% 
  dplyr::group_by(STATION_NUMBER) %>% 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1
  ) 

flowdepth_unreg <- normal_fltr_updte %>% 
  dplyr::left_join(area) %>% 
  dplyr::mutate(runoff = Value * 1/area_m2 *60*60*24*No_days *1000) %>% 
  dplyr::group_by(STATION_NUMBER, Month) %>% 
  dplyr::summarise(
    area_km2 = first(DRAINAGE_AREA_GROSS),
    runoff = mean(runoff),
    Q = mean(Value)
  ) %>% 
  left_join(record_length) 

write.csv(flowdepth_unreg, paste0("data/gauged/wsc/regulated/wsc_regulated_", 
                                  which_year_min, "_",
                                  which_year_max,
                                  "_flows_long.csv"), 
          row.names = F)

flowdepth_unreg <- read.csv('data/gauged/wsc/regulated/wsc_regulated_1981_2010_flows_long.csv')

flowdepth_wide <- flowdepth_unreg |>  
  group_by(STATION_NUMBER) |> 
  mutate(ann_mm = sum(runoff)) |> 
  ungroup() |> 
  tidyr::pivot_wider(c(STATION_NUMBER, area_km2, ann_mm), names_from = Month, values_from = runoff) |>  
  select(ID = STATION_NUMBER, area = area_km2, `1`:`12`, ann_mm)

colnames(flowdepth_wide)[3:14] <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(flowdepth_wide, paste0("data/gauged/wsc/regulated/wsc_regulated_", 
                                 which_year_min, "_",
                                 which_year_max,
                                 "_flows_wide.csv"),
          row.names = F)


# compare to wsc old flows
old_reg <- read.csv('data/gauged/wsc/regulated/wsc_flowDat_withRegulated_old_file_from_JT_script.csv') |> 
  rename(ID = STATION_NUMBER)

all <- inner_join(flowdepth_wide, old_reg, by = c("ID")) |> mutate(diff = ann_mm.x - ann_mm.y)


ggplot(all, aes(ann_mm.x, ann_mm.y)) +
  geom_point()

# cotton wood creek area was wrong on the old one and is causing the huge error. 

