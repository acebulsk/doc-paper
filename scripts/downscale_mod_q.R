# downscale dcwbm output on gauges for better comparison. first took dcwbm points and filtered to only include pts within 800m of a gauge basin.
# Then turn that into a raster that has the original pt ID and then downsample the raster and then join back the Q data from the full set.

library(dplyr)
library(sf)
library(raster)
library(tmap)
library(sp)

path = 'D:/transboundary_hakai/scratch/unreg_Gauged_Data/'
setwd(path)

# 
# bcakPts <- st_read("dcwbmPointsBCAK_2020.shp")
# 
# 
# # combo of nhc, foundry and env can
# wsc_bsns <- sf::st_read("D:/transboundary_hakai/scratch/unreg_Gauged_Data/2021_UPDATED_wsc_basins.shp") %>% 
#   st_transform(4326)
# 
# # get only the pts we care about aka filter to bsn area
# pts_fltr <- st_filter(bcakPts, wsc_bsns)

pts_fltr <- st_read("dcwbmPointsBCAK_2020_gaugeOnly.shp") %>% 
  st_transform(3005) %>% 
  as_Spatial()

raster_template = raster(extent(pts_fltr), resolution = 401,
                         crs = st_crs(pts_fltr)$proj4string)

raster_template_resample = raster(extent(pts_fltr), resolution = 50,
                                  crs = st_crs(pts_fltr)$proj4string)


r = rasterize(pts_fltr, raster_template, field = "ID")

rsmple = resample(r, raster_template_resample, method = "ngb")

pt_50 <- rasterToPoints(r, progress = "text", spatial = T)

writeRaster(r, "D:/transboundary_hakai/scratch/unreg_Gauged_Data/dcwbmRasterBCAK_2021_400m_R.tif")

st_write(pt_50, "D:/transboundary_hakai/scratch/unreg_Gauged_Data/dcwbmPointsBCAK_2021_50m_R.shp")

plot(r)

tmap_mode("view")

tm_shape(r) +  
  tm_raster() +
  tm_shape(pts_fltr_projected) +
  tm_dots()

st_write(r, )