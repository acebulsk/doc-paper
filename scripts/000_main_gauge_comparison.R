# this script preserves the ordering of the workflow


# collect wsc gauged data using tidyhydat
source('scripts/00_wsc_collect_basin_delins.R', local = T)

# get the wsc gauged basin delineations, the usgs basin delins are handeled within the below script.
# WARNGING: it takes about an hour to load the gdb into memory

# source('scripts/01_wsc_collect_basin_delins.R', local = T)

# now get the usgs gauge delins and use dataRetreival to get the flows
source('scripts/02_usgs_unreg_gauge_data.R', local = T)

# aggregate the raw dcwbm points from amrit (using his regime based calibration) to the WSC gauge delins from above
source('scripts/03_aggregate_mod_to_wsc_basin.R', local = T)

# aggregate the raw dcwbm points from amrit (using his regime based calibration) to the USGS gauge delins from above
source('scripts/04_aggregate_mod_to_usgs_basin.R', local = T)