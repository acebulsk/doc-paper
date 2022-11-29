# this script does some preprocessing on choosing what gauges we want to use in 
# creating the composite discharge dataset essentially we are choosing the largest
# gauge and removing any others that overlap with it and are redundant. 

library(tidyverse)
library(sf)

# npctr basins 

npctr_bsn <- st_read('data/gis/npctr_basins/npctr_basins.shp')

## NEW STUFF ####

# get observed data with regulated stations included
wsc <- read.csv('data/gauged/wsc/regulated/wsc_regulated_1981_2010_flows_long.csv') |> 
  select(GAUGEID = STATION_NUMBER, Month, area = area_km2, runoff, Q, start_year, end_year, record_length)

usgs <- read.csv('data/gauged/usgs/regulated/usgs_regulated_1981-2010_long.csv') |> 
  select(GAUGEID = site_no, Month = month_nu, area, runoff, Q, start_year, end_year, record_length) |> 
  mutate(area = area / 1e6)

all_flows <- rbind(wsc, usgs)

# basin polygons including regulated gauges

wsc_poly <- st_read('data/gis/wsc_gauge_basins/hnbp_2022/hnbp_2022_wsc_all_basins.shp') |> 
  select(GAUGEID = StationNum, name = NameNom, area = Area_km2) |> 
  filter(GAUGEID %in% wsc$GAUGEID) |> 
  st_transform(st_crs(npctr_bsn))

# we have all wsc polys
wsc$GAUGEID[!wsc$GAUGEID %in% wsc_poly$GAUGEID]

usgs_poly <- st_read('data/gauged/usgs/regulated/usgs_regulated_basins_1981_2010.shp') |> 
  select(GAUGEID = GAGE_ID, name = Name, area = AREA) |> 
  mutate(area = area /1e6)|> 
  st_transform(st_crs(npctr_bsn))

st_crs(wsc_poly) == st_crs(usgs_poly)

# we have all usgs polys
usgs$GAUGEID[!usgs$GAUGEID %in% usgs_poly$GAUGEID]

all_polys <- rbind(wsc_poly, usgs_poly)
# reduce basins to non overlaping - find largest basin in case of gauge overlap ####

obs_centres <- st_centroid(all_polys) |>
  st_join(npctr_bsn |> select(IP_ID))


one_gauge_per <- obs_centres |>
  group_by(IP_ID) |>
  slice_max(area)

one_gauge_bsn <- all_polys |> filter(GAUGEID %in% one_gauge_per$GAUGEID)

# st_write(one_gauge_bsn, '../data/regulated_gauge_flows/all_regulated_basins_one_per_IPID.shp')

# get duplicate basins located in the large gauge selected above so we know which ones we dont want moving forward
dups <- obs_centres |>
  st_join(one_gauge_bsn, left = F)

# do we have any overlapping left??
check <- all_polys |> filter(!GAUGEID %in% c(dups$GAUGEID.x, one_gauge_bsn$GAUGEID)) |>
  st_within(sparse = F)
diag(check) <- NA
any(TRUE == check, na.rm = T) # yes we do - will have to filter these out

non_dups <- all_polys |> filter(!GAUGEID %in% c(dups$GAUGEID.x, one_gauge_bsn$GAUGEID)) |>
  left_join(obs_centres |> select(GAUGEID, IP_ID) |> st_drop_geometry()) |>
  group_by(IP_ID) |>
  slice_max(area) |>
  select(-IP_ID)

update <- rbind(one_gauge_bsn, non_dups)

# st_write(update, '../data/regulated_gauge_flows/all_regulated_basins_no_overlap.shp')

# repeat above
dups_v2 <- obs_centres |>
  st_join(update, left = F) # inner join

# do we have any overlapping polys left??
check_v2 <- all_polys |> filter(!GAUGEID %in% c(dups_v2$GAUGEID.x, update$GAUGEID)) |>
  st_within(sparse = F)

diag(check_v2) <- NA

any(TRUE == check_v2, na.rm = T) # still have some overlapping

non_dups_v2 <- all_polys |> filter(!GAUGEID %in% c(dups_v2$GAUGEID.x, update$GAUGEID))

update_v2 <- rbind(update, non_dups_v2)

# st_write(update_v2, '../data/regulated_gauge_flows/all_regulated_basins_no_overlap_v2.shp')

# repeat above
# above didnt work because the centre pt of 12037400 does not overlap with 12036000, 12035400 but we caught it with the st_within check

update_v3 <- update_v2 |>
  filter(!GAUGEID %in% c(12036000, 12035400)) |>
  select(-area) #get rid of simple features area - we will use hydat/usgs area later

check_final <- update_v3 |> st_within(sparse = F)
diag(check_final) <- NA

any(TRUE == check_final, na.rm = T) # no more overlapping

st_write(update_v3, 'data/composite/all_regulated_basins_no_overlap_v3.shp')

# manually remove port mann and replace with hope as per discussion with bill and amrit

# add back hope basin for more new dataset 

hope_id <- "08MF005"
pt_mann_id <- "08MH126"

hope <- all_polys |>
  select(-area) |> 
  st_transform(st_crs(npctr_bsn)) |>
  st_make_valid() |> 
  filter(GAUGEID == hope_id)

reg_bsn <- update_v3 |> 
  filter(GAUGEID != pt_mann_id) |>  # out with pt mann 
  rbind(hope) #in with hope

st_write(reg_bsn, 'data/composite/all_regulated_basins_no_overlap_v4.shp')

reg_bsn_ip_id <- reg_bsn |> 
  st_centroid() |> 
  st_join(npctr_bsn |> select(IP_ID)) |> 
  st_drop_geometry()

write.csv(reg_bsn_ip_id, 'data/composite/reg_ip_id_filter.csv', row.names = F)

