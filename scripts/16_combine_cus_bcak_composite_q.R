# this file was from the DOC_REPORT file sent off for DOC modelling 
doc_q_original <- read.csv('data/composite/DCWBM_Runnoff_mm_composite_Master_DOC_IP_IDs_20200130.csv')

# combine the bcak and cus composite flows created in 14 and 15, 
# there are NO basins in this file that are all modelled
bcak_comp <- read.csv('data/composite/bcak_composite_flows.csv')
cus_comp <- read.csv('data/composite/cus_composite_flows.csv')

composite_sum <- rbind(
  bcak_comp,
  cus_comp
)

# get the modelled npctr basin flows with no gauges
allmod <- read.csv('data/modelled/npctr_bsn_1981_2010_dcwbm_sim_q.csv') |> 
  filter(!IP_ID %in% composite_sum$IP_ID) |>  # filter out 110 composite basins 
  mutate(is_composite = 'no')  |> 
  mutate(
    comp_area = NA,
    area_sim = Are_km2,
    area_obs = 0) |> 
  select(IP_ID, Jan:Shap_Ar, ann_mm, comp_area:area_obs, q_km3yr:is_composite)
  

comp_out <- composite_sum |> 
  mutate(Month = month.abb[Month]) |> 
  group_by(IP_ID) |> 
  mutate(ann_mm = sum(comp_mm)) |> 
  ungroup() |> 
  pivot_wider(id_cols = c(IP_ID, ann_mm, comp_area, area_sim, area_obs), names_from = Month, values_from = comp_mm) |> 
  left_join(npctr_bsn |> st_drop_geometry()) |> 
  mutate(q_km3yr = (ann_mm / 1e6) * Are_km2) |> 
  mutate(is_composite = 'yes') |> 
  select(IP_ID, Jan:Shap_Ar, ann_mm, comp_area:area_obs, q_km3yr:is_composite) |> 
  rbind(allmod)

write.csv(comp_out, 'data/composite/npctr_bsn_1981_2010_composite_update_20221128.csv', row.names = F)

# compare to old comp

jn <- inner_join(comp_out, doc_q_original, by = "IP_ID") |> 
  mutate(diff = ann_mm.x - ann_mm.y)

ggplot(jn, aes(ann_mm.x, ann_mm.y)) + 
  geom_point() +
  xlab('Updated Runoff (mm)') +
  ylab('Published Runoff (mm)')

ggsave('plots/comparison_of_updated_runoff.png', width = 4, height = 4)

# values of big ten basins only differ slightly from published values, likely due to difference in arc
# manual methods

# some of the smaller basins have a large offset and is attributed to updates
# to the gauged data (discharge or area values from usgs and wsc) 
# in this script compared to the old analysis.

# the fraser and columbia is not included in this analysis, but those data were 
# near entirely gauged and a small portion below the gauged was added manually in arcgis.


