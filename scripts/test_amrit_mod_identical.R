# dcwbmPointsBCAK_2020.shp is used in prior scripts but there is no reference of where is came from. 
# This script shows that it is identical to the modelled dataset from Amrit where 
# 1981-2020 runoff normals were modelled using the calibrated Rain/Snow separation temperature based on catchment regime. 

library(sf)
library(tidyverse)

bc1 <- read_sf('data/modelled/dcwbmPointsBCAK_2020.shp') 

bc1_df <- bc1[,5:16] |> st_drop_geometry() |> pivot_longer(everything())

bc2 <- read.csv('data/modelled/HUC_Runoff_Gridded.csv') 

bc2_df <- bc2[,5:16] |> pivot_longer(everything())

bc_tst <- data.frame(bc1_df, bc2_df)

bc_tst$diff <- bc_tst$value - bc_tst$value.1


cus1 <- read_sf('data/modelled/dcwbmPointsCUS_2020.shp')

cus1_df <- cus1[,5:16] |> st_drop_geometry() |> as.data.frame() |> pivot_longer(everything())

cus2 <- read.csv('data/modelled/CUS_Runoff_Gridded.csv') 

cus2_df <- cus2[,5:16] |> pivot_longer(everything())

cus_tst <- data.frame(cus1_df, cus2_df)

cus_tst$diff <- cus_tst$value - cus_tst$value.1

# the diffs are +/- 4.547474e-13 must be a rounding error from arc so we will use the orig amrit set. 
