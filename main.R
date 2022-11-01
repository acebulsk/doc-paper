# this script preserves the ordering of the workflow


source('scripts/00_wsc_collect_basin_delins.R', local = T)

# before we can do anything we need the gauged basin delineations, 
# WARNGING: it takes about an hour to load the gdb into memory

# source('scripts/01_wsc_collect_basin_delins.R', local = T)