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

stn_reg <- hy_stn_regulation() |> 
  dplyr::rename(reg_year_from = Year_from,
                reg_year_to = Year_to)

stn_range <- hy_stn_data_range() 

stn_meta_all <- hy_stations() |> 
  left_join(stn_reg) |> 
  left_join(stn_range) 

stn_meta_fltr <- stn_meta_all |> 
  left_join(stn_reg) |> 
  left_join(stn_range) |> 
  filter(
    PROV_TERR_STATE_LOC %in% c("BC", "YT", "AK"),
    # REGULATED == F, #this removed the englishman and zeballos which still have 10 yrs flow dat before reg. 
    Year_from <= which_year_max - 10, # stations starting after which_year - 10 will not have 10 yrs within range
    RECORD_LENGTH >= 10, # need at least 10 yrs will constrain within 1981-2010 later  
    DATA_TYPE == "Q",
    DRAINAGE_AREA_GROSS > 0,
    is.na(DRAINAGE_AREA_GROSS)==F,
    is.na(STATION_NUMBER)==F,
    is.na(REGULATED) == F,
    REGULATED == F | (REGULATED == T & reg_year_from >= which_year_min + 10)) # keep stations that were regulated after 1990. Might have 10 years of data prior to regulation. Need to clip individually later by each stns reg from date. 

# these bsns need to be trimmed to their regulation start date 
stn_reg_fltr <- stn_meta_fltr |> 
  filter(REGULATED == T & reg_year_from >= which_year_min + 10)

stns_for_later <- stn_reg_fltr$STATION_NUMBER

st.stn_meta_all <- stn_meta_all |> 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) 

st.stn_meta_fltr <- stn_meta_fltr |> 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) 

st_crs(st.stn_meta_all) <- 4326
st_crs(st.stn_meta_fltr) <- 4326

# transboundary basins

# npctr_bsns <- st_read("../data/npctr_basins.shp") |> 
#   st_transform(st_crs(st.stn_meta_fltr)) |> 
#   filter(Name != "Fraser River",
#          Name != "Columbia River") |> 
#   st_make_valid() 
# 
# st_write(npctr_bsns, "data/gis/npctr_basins/npctr_basins_valid_no_fr_clm.shp")

npctr_bsns <- st_read("data/gis/npctr_basins/npctr_basins_valid_no_fr_clm.shp")

# get wsc gauges inside the NPCTR

st.filter <- st.stn_meta_fltr |> 
  st_filter(npctr_bsns)

# map 

# tm_shape(npctr_bsns) +
#   tm_polygons() +
#   tm_shape(st.filter) +
#   tm_dots()

# filtered spatially

basins_fltr <- st.filter$STATION_NUMBER

# grab monthly flow normals but still need to filter by at least 10 yrs data
monthly_normals <- ldply(basins_fltr, function(x) hy_monthly_flows(station_number = x)) 



# apply more filters

normal_fltr <- monthly_normals |> 
  filter(
    Year >= which_year_min,
    Year <= which_year_max,
    Full_Month == T,
    Sum_stat == "MEAN"
  ) 

# remove regulated yrs for regulated stations 
zeballos <- normal_fltr |> 
  filter(STATION_NUMBER == "08HE006", 
         Year < 2009)

englishman <- normal_fltr |> 
  filter(STATION_NUMBER == "08HB002", 
         Year < 2000)

# add back filtered stations 
normal_fltr_unreg <- normal_fltr |> 
  filter(!STATION_NUMBER %in% c("08HE006", "08HB002")) |> # out w. old
  rbind(zeballos, englishman) # in w. new

# check number of months of data in each year and  total number of months for at least 10 yrs 

month_count <- normal_fltr_unreg |> 
  group_by(STATION_NUMBER) |> 
  tally() |> 
  filter(n >= 120) # need at least 10 years of data


# its okay if most years have all months but notokay if all years are missing months 
month_in_yr_count <- normal_fltr_unreg |> 
  filter(STATION_NUMBER %in% month_count$STATION_NUMBER) |> 
  dplyr::group_by(STATION_NUMBER, Year) |> 
  tally() |> 
  dplyr::group_by(STATION_NUMBER) |> 
  dplyr::summarise(n = mean(n)) |> 
  filter(n > 10) # basins with an average of over 10 months in each yr; discludes 3 basins that do not have any full years of data

# filter out basins with missing months or removed via joels legacy script. mostly redundant from above
bad_basins <- c("08FC005", # looks like its regulated on sat imagery and has very low flows
                "08EE003", # no years with all months
                "08OA004", # no yrs with all months 
                "08OA005", # no yrs with all months 
                "08EE025" # joel removed previously - maybe bad basin boundary 
)


normal_fltr_updte <- normal_fltr_unreg |> 
  filter(STATION_NUMBER %in% month_in_yr_count$STATION_NUMBER,
         !STATION_NUMBER %in% bad_basins)

final_count <- length(unique(normal_fltr_updte$STATION_NUMBER))

# show start and end year for each station
start_end_meta <- normal_fltr_updte |> 
  dplyr::group_by(STATION_NUMBER) |> 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1)

# published <- read_csv("/mnt/HDD/transboundary_hakai/deliverables/paper-results/dcwbm_observed_all_long_Basins_update_2021.csv") |> 
#   left_join(start_end_meta, by = c("ID" = "STATION_NUMBER")) |> 
#   mutate(start_year = coalesce(start_year.x, start_year.y),
#          end_year = coalesce(end_year.x, end_year.y),
#          record_length = coalesce(record_length.x, record_length.y)) |> 
#   select(-c(start_year.x:record_length.y))
# 
# write_csv(published, "/mnt/HDD/transboundary_hakai/deliverables/paper-results/dcwbm_observed_all_long_Basins_update_2021.csv")

test <- normal_fltr_updte |> 
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

st.filter_final <- st.filter |> 
  filter(STATION_NUMBER %in% normal_fltr_updte$STATION_NUMBER) |> 
  dplyr::rename(PROV = PROV_TERR_STATE_LOC,
                AREA = DRAINAGE_AREA_GROSS) |> 
  select(-DRAINAGE_AREA_EFFECT)

tm_shape(st.filter_final) +
  tm_dots()

# st_write(st.filter_final, "data/gis/wsc_gauge_basins/wsc_unregulated_points_w_data_1981_2010.shp")

st.filter_final <- st_read("data/gis/wsc_gauge_basins/wsc_unregulated_points_w_data_1981_2010.shp")

# meta filter
stn_meta_final <- stn_meta_fltr |> 
  filter(STATION_NUMBER %in% normal_fltr_updte$STATION_NUMBER) 

write.csv(stn_meta_final, 'data/gauged/wsc/wsc_unregulated_1981_2010_metadata.csv', row.names = F)

# check if any of the old basins were not included

wsc_data_old <- read.csv("data/gauged/wsc/2021_UPDATED_wsc_1981-2010_monthly_normals_wide.csv") |>
  filter(!ID %in% st.filter_final$STATION_NUMBER)

wsc_meta_missing <- stn_meta_all |> filter(STATION_NUMBER %in% wsc_data_old$ID)

# get largest list of basins we have 

wsc_bsns_all <- st_read("data/gis/wsc_gauge_basins/hnbp_2022/hnbp_2022_wsc_basins_w_dat_avail.shp") 

new_wsc_gauges_no_bsn <- st.filter_final |> filter(!STATION_NUMBER %in% wsc_bsns_all$StationNum) |> select(STATION_NUMBER, STATION_NAME)

#st_write(new_wsc_gauges_no_bsn, "/mnt/HDD/transboundary_hakai/deliverables/paper-results/GIS/new_wsc_gauges_no_bsn.shp")

tmap_mode("view")

tm_shape(wsc_bsns_all) +
  tm_polygons() +
  tm_shape(new_wsc_gauges_no_bsn) +
  tm_dots()

# convert to mm 

mdays = data.frame(month = c(1:12), mdays = c(31,28.25,31,30,31,30,31,31,30,31,30,31))

area <- stn_meta_final |> 
  select(STATION_NUMBER, DRAINAGE_AREA_GROSS) |> 
  mutate(area_m2 = DRAINAGE_AREA_GROSS * 1000000)


record_length <- normal_fltr_updte |> 
  dplyr::group_by(STATION_NUMBER) |> 
  dplyr::summarise(
    start_year = min(Year),
    end_year = max(Year),
    record_length = (end_year - start_year) +1
  ) 

flowdepth_unreg <- normal_fltr_updte |> 
  dplyr::left_join(area) |> 
  dplyr::mutate(runoff = Value * 1/area_m2 *60*60*24*No_days *1000) |> 
  dplyr::group_by(STATION_NUMBER, Month) |> 
  dplyr::summarise(
    area_km2 = first(DRAINAGE_AREA_GROSS),
    runoff = mean(runoff),
    Q = mean(Value)
  ) |> 
  left_join(record_length) 

write.csv(flowdepth_unreg, "data/gauged/wsc/wsc_unregulated_1981_2010_flows_long_2022.csv", row.names = F)

flowdepth_wide <- flowdepth_unreg |> 
  tidyr::pivot_wider(c(STATION_NUMBER, area_km2), names_from = Month, values_from = runoff) |> 
  mutate(ann_mm = rowSums(across(`1`:`12`, na.rm = TRUE))) |> 
  rename(ID = STATION_NUMBER, area = area_km2)

colnames(flowdepth_wide)[3:14] <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

write.csv(flowdepth_wide, "data/gauged/wsc/wsc_unregulated_1981_2010_flows_wide_2022.csv", row.names = F)


# compare to wsc old flows
wsc_data_old <- read.csv("data/gauged/wsc/2021_UPDATED_wsc_1981-2010_monthly_normals_wide.csv")

month_num <- data.frame(num = c(1:12), month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

old_wsc_flows <- wsc_data_old |> 
  pivot_longer(Jan:Dec, names_to = "month", values_to = "runoff_old") |> 
  left_join(month_num, by = "month") |> 
  select(ID, month = num, runoff_old) 

new_wsc_flows <- flowdepth_unreg |> 
  select(ID = STATION_NUMBER, month = Month, runoff_new = runoff)

all <- inner_join(old_wsc_flows, new_wsc_flows, by = c("ID", "month")) |> 
  mutate(diff = runoff_old - runoff_new)

ggplot(all, aes(x = runoff_old, y = runoff_new)) +
  geom_point()

