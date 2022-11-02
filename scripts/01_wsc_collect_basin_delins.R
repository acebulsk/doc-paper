# this script is from the updated 1991-2020 normals project and includes some additional basins we got from NHC.

library(sf)

# bring in the list of basins we care about that have data for the NPCTR

wsc_gauges_w_data <- readRDS('data/gis/wsc_gauge_basins/wsc_unreg_basin_points_w_data_available.rds')

# more comprehensive list of WSC basins from wsc
# as of this writing is still a pre-release from 2022-07-15
# and contains 7300 of the 7896 active and discontinued stations
# https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/
files <- list.files('data/gis/wsc_gauge_basins/hnbp_2022/', recursive = T, pattern = '*DrainageBasin_BassinDeDrainage.shp$', full.names = T)

sh_2022 <- purrr::map_dfr(.x = files, .f = sf::read_sf)

st_write(sh_2022, 'data/gis/wsc_gauge_basins/hnbp_2022/hnbp_2022_wsc_all_basins.shp')

# does this have all the basin delins we care about?
sh_2022_ids <- sh_2022 |> st_drop_geometry()

wsc_gauges_w_data$STATION_NUMBER[!wsc_gauges_w_data$STATION_NUMBER %in% sh_2022_ids$StationNum]

# yes looks like all are here

# now filter sh 2022 to the ones we care about

sh_2022_w_data_avail <- sh_2022 |> 
  st_zm() |> 
  filter(StationNum %in% wsc_gauges_w_data$STATION_NUMBER)

st_write(sh_2022_w_data_avail, 'data/gis/wsc_gauge_basins/hnbp_2022/hnbp_2022_wsc_basins_w_dat_avail.shp')

# compare new areas to HYDAT
left_join(sh_2022_w_data_avail |> st_drop_geometry(), 
          wsc_gauges_w_data |> st_drop_geometry(), 
          by = c('StationNum' = 'STATION_NUMBER')) |> 
  ggplot(aes(Area_km2, DRAINAGE_AREA_GROSS)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

# CAN IGNORE BELOW WAS JUST TESTING HOW THE OLD DELINEATIONS COMPARED TO THIS NEW SET

# https://open.canada.ca/data/en/dataset/0c121878-ac23-46f5-95df-eb9960753375
# National hydrometric network basin polygons this is the original dataset that 
# was used before but was missing quite a few gauges for our interest.

layers <- st_layers("data/gis/wsc_gauge_basins/WSC_Basins.gdb") 

filter <- grepl('EC_08|EC_09', layers$name)

layers_filter <- layers$name[filter]

sh_2016 <- purrr::map_dfr(.x = layers_filter, ~st_read(dsn="data/gis/wsc_gauge_basins/WSC_Basins.gdb",layer=.))

st_write(sh_2016, 'data/gis/wsc_gauge_basins/nhnbp_2016_wsc_all_basins.shp')
saveRDS(sh_2016, 'data/gis/wsc_gauge_basins/nhnbp_2016_wsc_all_basins.shp')

sh_2016_ids <- sh_2016 |> st_drop_geometry()

wsc_gauges_w_data$STATION_NUMBER[!wsc_gauges_w_data$STATION_NUMBER %in% sh_2016_ids$Station]

# so this old set was missing 98 basins we care about which before I filled with various other sources listed below


# new basins from NHC 2021 missing seymour, mamquam, clayton 
nhc_meta <- read.csv("data/gis/wsc_gauge_basins/WSC_Basins_NHC_2021/Appendix 5.3 GIS Data/20200817_3004476_NHC_station_metadata.R0.csv") |> 
  dplyr::rename(ID = Gauge.ID)

nhc_bsns <- st_read("data/gis/wsc_gauge_basins/WSC_Basins_NHC_2021/Appendix 5.3 GIS Data/20200811_3004476_NHC_all_watersheds.R0.shp")

nhc_bsns[nhc_bsns$ID == "15041200", "ID"] <- "08BB005" # conv usgs id to wgs id to match Q dat

nhc_bsns[nhc_bsns$ID == "15024800", "ID"] <- "08CF003" # conv usgs id to wgs id to match Q dat


nhc_ids <- nhc_bsns |> st_drop_geometry()

# has seymout and mamquam
raw_ec_bsns <- st_read("data/gis/wsc_gauge_basins/raw_ec_basins_BC.shp") |> 
  st_transform((st_crs(nhc_bsns))) |> 
  select(ID = Station, Name = StatnNm) |> 
  filter(!ID %in% nhc_bsns$ID)

# has clayton
foundry_sptl_bsns <- st_read("data/gis/wsc_gauge_basins/basins_for_bill_floyd_from_FoundrySpatial/basins_for_bill_floyd/assigned_basins.shp") |> 
  st_transform((st_crs(nhc_bsns))) |> 
  select(ID = station_nu, Name = station_na) |> 
  filter(!ID %in% nhc_bsns$ID)

# combine basins
all_bsns <- rbind(nhc_bsns, raw_ec_bsns, foundry_sptl_bsns) |> 
  left_join(nhc_meta, by = "ID")

# missing PITMAN RIVER NEAR THE MOUTH, STIKINE RIVER ABOVE GRAND CANYON, STIKINE RIVER ABOVE BUTTERFLY CREEK

missing_basins <- wsc_gauges_w_data |> filter(!STATION_NUMBER %in% all_bsns$ID) 

legacy_wsc_basins_w_dat <- all_bsns |> filter(ID %in% wsc_gauges_w_data$STATION_NUMBER) #|> st_drop_geometry()

# now how bad were the original areas?

left_join(sh_2022_w_data_avail |> st_drop_geometry(), 
          legacy_wsc_basins_w_dat |> st_drop_geometry(), 
          by = c('StationNum' = 'ID')) |> 
  ggplot(aes(Area_km2, Watershed.Area..km2.)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)
