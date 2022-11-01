library(dataRetrieval)
library(plyr)
library(dplyr) 
library(sf)
library(tmap)
library(ggplot2)
library(ggforce)

# #Water use freshwater withdrawals in millions of gallons per day (mgd) per square kilometer for year XXXX
# watuse <- read.csv(file = "../data/unregulated_gauge_flows/usgs_metadata/Dataset15_Water_Use/wateruse-huc10.txt")

# wrong scale
# ids <- read.csv(file = "../data/unregulated_gauge_flows/usgs_metadata/Dataset0_Boundary_IDs/boundary_ids-huc10s.txt")
# 
# nodams <- read.csv(file = "../data/unregulated_gauge_flows/usgs_metadata/dams-reservoirs-huc10.txt") %>% 
#   filter(NDAMS_1992_huc10 == 0) %>% 
#   left_join(ids, by = "uniqid_huc10") %>% 
#   select(HUC10, NAME)

dams <- read.csv(file = "data/gis/usgs_gauge_basins/conterm_hydromod_dams.txt") %>% 
  select(ID = STAID, NDAMS_2009) %>% 
  filter(NDAMS_2009 != 0)

# now get all gauges within NPCTR
npctr_bsns <- st_read("data/gis/npctr_basins/npctr_basins.shp") %>% 
  filter(Name != "Fraser River",
         Name != "Columbia River") %>% 
  st_transform(4326) %>% 
  st_make_valid() 

sites_wa <- whatNWISdata(stateCd = "WA" , parameterCd = "00060", statCd = "00003")
sites_or <- whatNWISdata(stateCd = "OR" , parameterCd = "00060", statCd = "00003")
sites_ca <- whatNWISdata(stateCd = "CA" , parameterCd = "00060", statCd = "00003")
sites_ak <- whatNWISdata(stateCd = "AK" , parameterCd = "00060", statCd = "00003")

sites <- rbind(sites_wa, sites_or, sites_ca, sites_ak) %>% 
  filter(
    !site_no %in% dams$ID,
    data_type_cd == "dv",
    begin_date < "1981-01-01",
    end_date > "2011-01-01"
  )

sites.sf <- st_as_sf(sites, coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

st.filter <- sites.sf %>% 
  st_filter(npctr_bsns)

# these are the USGS sites originally included in the 1981-2010 comparison
old_sites <- c(12200500, 14308500, 14306500, 12061500, 11482500, 12040500, 12041200, 12013500)

site_check <- st.filter %>% filter(site_no %in% old_sites) # all 8 still here

sites_final <- st.filter %>% st_drop_geometry()

# map 

# tm_shape(npctr_bsns %>% st_simplify(dTolerance = 400)) +
#   tm_polygons() +
#   tm_shape(st.filter) +
#   tm_dots()

# do we have required shapefiles? https://water.usgs.gov/GIS/dsdl/boundaries_shapefiles_by_aggeco.zip
# https://water.usgs.gov/GIS/metadata/usgswrd/XML/gagesII_Sept2011.xml

usgs_shp1 <- st_read("data/gis/usgs_gauge_basins/boundaries-shapefiles-by-aggeco/bas_nonref_AKHIPR.shp") 
usgs_shp2 <- st_read("data/gis/usgs_gauge_basins/boundaries-shapefiles-by-aggeco/bas_nonref_CntlPlains.shp")
usgs_shp3 <- st_read("data/gis/usgs_gauge_basins/boundaries-shapefiles-by-aggeco/bas_nonref_MxWdShld.shp")
usgs_shp4 <- st_read("data/gis/usgs_gauge_basins/boundaries-shapefiles-by-aggeco/bas_nonref_WestMnts.shp")
usgs_shp5 <- st_read("data/gis/usgs_gauge_basins/boundaries-shapefiles-by-aggeco/bas_nonref_WestPlains.shp")
usgs_shp6 <- st_read("data/gis/usgs_gauge_basins/boundaries-shapefiles-by-aggeco/bas_nonref_WestXeric.shp")

non_ref <- rbind(
  usgs_shp1,
  usgs_shp2,
  usgs_shp3,
  usgs_shp4,
  usgs_shp5,
  usgs_shp6) %>%
  filter(GAGE_ID %in% sites_final$site_no)

usgs_shp <- st_read("data/gis/usgs_gauge_basins/boundaries-shapefiles-by-aggeco/bas_ref_all.shp") %>%
  rbind(usgs_shp1) %>% # keep non ref AK basins keep in mind for later if these have higher error because of problems
  filter(GAGE_ID %in% sites_final$site_no) %>% 
  mutate(st_area = st_area(.)) %>% # check area is in metres
  select(-st_area)

# st_write(usgs_shp, "data/gis/usgs_gauge_basins/usgs_unregulated_basins_1981-2010.shp")

wsc_shp <- st_read('data/gis/wsc_gauge_basins/hnbp_2022/hnbp_2022_wsc_basins_w_dat_avail.shp') %>% st_make_valid()

tm_shape(usgs_shp) +
  tm_polygons(col = "green") +
  tm_shape(wsc_shp) +
  tm_polygons(col = "blue")+
  tm_shape(st.filter) +
  tm_dots()

site_final_have_basin <- inner_join(sites_final, usgs_shp, by = c("site_no" = "GAGE_ID")) %>% 
  select(site_no, station_nm, begin_date, end_date, AREA) %>% 
  mutate(site_no = as.numeric(site_no))


# get discharge 

q <- readNWISdata(site = site_final_have_basin$site_no,
                  parameterCd = "00060",
                  statCd = "00003",
                  service = "dv",
                  startDate = "1981-01-01",
                  endDate = "2011-01-01")

q_final <- ldply(site_final_have_basin$site_no, function(x) readNWISdata(site = x,
                                                                    parameterCd = "00060",
                                                                    service = "stat",
                                                                    statReportType="monthly"))

saveRDS(q_final, "data/gauged/usgs/usgs_1981_2010_raw_monthly_flows.rds")

site_check <- q_final %>% filter(site_no %in% old_sites)
unique(site_check$site_no) # lost 3

q_monthly_fltr <- q_final %>% 
  filter(
    year_nu >= 1981, 
    year_nu <= 2010
  )

month_count <- q_monthly_fltr %>% 
  group_by(site_no) %>% 
  tally() %>% 
  filter(n >= 120) # need at least 10 years of data

# its okay if most years have all months but notokay if all years are missing months. Need to check basins that have low values
month_in_yr_count <- q_monthly_fltr %>% 
  filter(site_no %in% month_count$site_no) %>% 
  dplyr::group_by(site_no, year_nu) %>% 
  tally() %>% 
  dplyr::group_by(site_no) %>% 
  dplyr::summarise(n = mean(n)) %>% 
  filter(n > 10) # basins with an average of over 10 months in each yr; discludes 3 basins that do not have any full years of data

q_monthly_fltr2 <- q_monthly_fltr %>% 
  filter(site_no %in% month_in_yr_count$site_no) |> 
  mutate(site_no = as.numeric(site_no))

final_count <- length(unique(q_monthly_fltr2$site_no))

# bad ones from before
bad<- c(15019990, 15261000,15101490,15238820, 15055500, 15236900, 15129600)

month_num <- data.frame(month_nu = c(1:12), month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

record_length <- q_monthly_fltr2 %>% 
  dplyr::group_by(site_no) %>% 
  dplyr::summarise(
    start_year = min(year_nu),
    end_year = max(year_nu),
    record_length = (end_year - start_year) +1
  ) 


flowdepth <- q_monthly_fltr2 %>% 
  dplyr::left_join(site_final_have_basin %>% select(site_no, AREA)) %>% 
  mutate(q_m3s = mean_va * 0.02832) %>% 
  dplyr::mutate(runoff = q_m3s * 1/AREA *60*60*24*count_nu *1000) %>% 
  dplyr::group_by(site_no, month_nu) %>% 
  dplyr::summarise(
    area = first(AREA),
    runoff = mean(runoff, na.rm = T),
    Q = mean(q_m3s, na.rm = T)
  ) %>% 
  filter(!site_no %in% bad) %>% 
  left_join(month_num, by = "month_nu") %>% 
  left_join(record_length, by = "site_no")

site_check <- flowdepth %>% filter(site_no %in% old_sites)
unique(site_check$site_no) # still just missing 3 not sure why probably because are classified as regulated by GAGES II

write.csv(flowdepth, "data/gauged/usgs/usgs_1981_2010_monthly_flows_long.csv")

for (i in 1:5) {
  print(ggplot(flowdepth, aes(factor(month, levels = c('Oct', 'Nov', 'Dec','Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep')), runoff, group = site_no)) +
          #geom_point() +
          # geom_jitter()+
          geom_point() +
          facet_wrap_paginate(~site_no, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE), ncol = 2, nrow = 6, page = i))
}

flowdepth$site_no[!flowdepth$site_no %in% usgs_shp$GAGE_ID]

st_write(usgs_shp %>% filter(GAGE_ID %in% flowdepth$site_no), "data/gis/usgs_gauge_basins/usgs_unregulated_basins_1981-2010_dataRetrieval_2022_update.shp")

# compare to the existing
new <- usgs_shp %>% filter(GAGE_ID %in% flowdepth$site_no)
old <- read_sf('data/gis/usgs_gauge_basins/usgs_unregulated_basins_1981-2010.shp')
