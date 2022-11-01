# bring in and staple together dcwbm data binned to gauged basins for GOF calcs and summary stats. Amrit params 2020. 

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(hydroGOF)
library(sf)

# north of border

# nobGauge <- read.csv("wsc_gaugeDat.csv") %>% 
#   select(ID = WSC, area, Jan:Dec, ann_mm, regime)   # old version

wsc_bsns <- st_read("../scratch/unreg_Gauged_Data/2021_UPDATED_wsc_basins.shp") %>% 
  st_drop_geometry() 

nobGaugeArea <- read.csv("../scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_long.csv") %>% 
  select(ID = STATION_NUMBER, area_km2) %>% 
  distinct()

nobGauge <- read.csv("../scratch/unreg_Gauged_Data/2021_UPDATED_wsc_1981-2010_monthly_normals_wide.csv") %>% 
  left_join(nobGaugeArea) %>% 
  select(ID,
         area = area_km2, 
         Jan:regime)

nobMod <- read.csv("../scratch/unreg_Gauged_Data/2021_dcwbm_nhc_foundry_EC_wsc_usgs.csv") %>% 
  left_join(nobGaugeArea) %>% 
  select(ID,
         area = area_km2, 
         Jan:regime)

# just missing 08EE003 because it does not have data for all months for any year. i.e. all yrs are missing at least one month 
nobModOld <- read.csv("../amrit/Gauged Compare/dcwbm2020_wscPolyUnreg.csv")%>% 
  select(ID = WSC, Jan:Dec, ann_mm) %>% 
  mutate(ID = toupper(ID)) %>% 
  filter(!ID %in% nobMod$ID) 

# statistics
nobMobLong <- nobMod %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "mod_q") %>% 
  mutate(ID = paste0(ID, Month))  %>% 
  select(ID, mod_q)

nobGaugeLong <- nobGauge %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "gauge_q") %>% 
  mutate(ID = paste0(ID, Month))  %>% 
  select(ID, gauge_q)

NSE(nobMobLong$mod_q, nobGaugeLong$gauge_q)

hydroGOF::rmse(nobMobLong$mod_q, nobGaugeLong$gauge_q, na.rm = T)

hydroGOF::mae(nobMobLong$mod_q, nobGaugeLong$gauge_q, na.rm = T)

hydroGOF::pbias(nobMobLong$mod_q, nobGaugeLong$gauge_q, na.rm = T)

hydroGOF::r(nobMobLong$mod_q, nobGaugeLong$gauge_q, na.rm = T)

gof(nobMobLong$mod_q, nobGaugeLong$gauge_q)


#south of border
sobGauge <- read.csv("../amrit/Gauged Compare/usgs_gaugeDat.csv") %>% 
  filter(NDAMS_2009 == 0, OBJECTID != 24) %>%  # set to unregulated - and remove clark basin because some sort of damming must be going on for those high summer flows. 
  select(ID = F_Station, old_ID = OBJECTID, area = Area_SqKm, Jan:Dec, ann_mm, regime) 
regime_dict <- c("Pluvial Dominant Hybrid", "Pluvial", "Pluvial", "Pluvial", "Pluvial", "Pluvial", "Pluvial Dominant Hybrid", "Pluvial")
sobGauge$regime <- regime_dict



sobGgeArea <- sobGauge %>% select(ID, old_ID, area)

sobMod <- read.csv("../amrit/Gauged Compare/dcwbm2020_usgsPolyUnreg.csv") %>% 
  filter(OBJECTID %in% sobGauge$old_ID) %>% # filter to match sob gauge list 
  select(ID = OBJECTID, Jan:Dec, ann_mm, regime)  %>% 
  left_join(sobGgeArea, by = c("ID" = "old_ID")) %>% 
  select(ID = ID.y, area, Jan:Dec, ann_mm, regime)

sobGauge$old_ID <- NULL

sobMobLong <- sobMod %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "mod_q") %>% 
  mutate(ID = paste0(ID, Month))  %>% 
  select(ID, mod_q)

sobGaugeLong <- sobGauge %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "gauge_q") %>% 
  mutate(ID = paste0(ID, Month))  %>% 
  select(ID, gauge_q)

NSE(sobMobLong$mod_q, sobGaugeLong$gauge_q)

hydroGOF::rmse(sobMobLong$mod_q, sobGaugeLong$gauge_q, na.rm = T)

hydroGOF::mae(sobMobLong$mod_q, sobGaugeLong$gauge_q, na.rm = T)

hydroGOF::pbias(sobMobLong$mod_q, sobGaugeLong$gauge_q, na.rm = T)

hydroGOF::r(sobMobLong$mod_q, sobGaugeLong$gauge_q, na.rm = T)

gof(sobMobLong$mod_q, sobGaugeLong$gauge_q)

# create dfs for full domain

mod <- rbind(nobMod, sobMod) 

# write.csv(mod, "../deliverables/paper-results/dcwbm_gaugedBasins_update_2021.csv", row.names = F)

mod_long <- mod %>% 
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "mod_q") %>% 
  mutate(ID = paste0(ID, Month))  %>% 
  select(ID, mod_q, Month) 

gauge <- rbind(nobGauge, sobGauge) %>% 
  filter(ID %in% mod$ID)

# write.csv(gauge, "../deliverables/paper-results/observed_gaugedBasins_update_2021.csv", row.names = F)

gauge_long <- gauge %>%  
  pivot_longer(Jan:Dec, names_to = "Month", values_to = "gauge_q") %>% 
  mutate(ID = paste0(ID, Month)) %>% 
  select(ID, gauge_q, regime)

all <- left_join(mod_long, gauge_long, by = "ID")

# full domain
NSE(all$mod_q, all$gauge_q)

hydroGOF::rmse(all$mod_q, all$gauge_q, na.rm = T)

hydroGOF::mae(all$mod_q, all$gauge_q, na.rm = T)

hydroGOF::pbias(all$mod_q, all$gauge_q, na.rm = T)

hydroGOF::r(all$mod_q, all$gauge_q, na.rm = T)

gof(all$mod_q, all$gauge_q)

obs <- 1:10
sim <- 1:10
NSE(sim, obs)

##### plot full domain #####

# monthly 
all$Month <- factor(all$Month, c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
all$regime <- factor(all$regime, c('Pluvial', 'Pluvial Dominant Hybrid', 'Nival Dominant Hybrid', 'Nival', 'Icefield'))

ggplot(all, aes(x= gauge_q, y = mod_q)) +
  geom_point(aes(colour = regime)) +
  labs(x='Observed Runoff (mm)', y = 'Modelled Runoff (mm)', colour = "Regime:") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 1200) +
  ylim(0, 1200) +
  scale_colour_brewer(palette="Dark2") +
  theme_bw() +
  facet_wrap(~Month, nrow = 4) +
  theme(legend.title = element_blank(),
        #legend.justification=c(1.2,0),
        legend.position='bottom',
        #legend.position=c(1, .001),
        #legend.position=c(.15, 0.9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) 

# ggsave('../deliverables/paper-results/figs/fullcoast_monthly_facets_2021_basins.png', width = 10, height = 10, units = "in")

# annual 

nobGaugeNames <- st_read("../scratch/unreg_Gauged_Data/2021_UPDATED_wsc_basins.shp") %>% 
  st_drop_geometry() %>% 
  select(ID, Name)

# plot annual mm
nobModAnn <- nobMod %>% 
  select(ID, ann_mm_mod = ann_mm, regime_mod = regime)
sobModAnn <- sobMod %>% 
  select(ID, ann_mm_mod = ann_mm, regime_mod = regime)
modAnn <- rbind(nobModAnn, sobModAnn)

nobGaugeAnn <- nobGauge %>% 
  select(ID, area, ann_mm_gauge = ann_mm, regime_obs = regime)
sobGaugeAnn <- sobGauge %>% 
  select(ID, area, ann_mm_gauge = ann_mm, regime_obs = regime)
gaugeAnn <- rbind(nobGaugeAnn, sobGaugeAnn) %>% 
  filter(ID %in% modAnn$ID)


combine_ann <- left_join(gaugeAnn, modAnn, by = "ID") %>% 
  left_join(nobGaugeNames, by = "ID") %>% 
  mutate(diff = (ann_mm_mod - ann_mm_gauge)) %>% 
  mutate(abs_diff = abs(ann_mm_mod - ann_mm_gauge)) %>% 
  mutate(perc_diff = (diff / ann_mm_gauge)*100) %>% 
  mutate(abs_perc_dif = (abs_diff / ann_mm_gauge)*100)

write.csv(combine_ann, "../deliverables/paper-results/dcwbm_observed_all_long_Basins_update_2021.csv", row.names = F)

ggplot(combine_ann, aes(x= ann_mm_gauge, y = ann_mm_mod)) +
  geom_point(aes(colour = factor(regime))) +
  labs(x='Observed Runoff (mm)', y = 'Modelled Runoff (mm)', colour = "Regime:") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0, 7000) +
  ylim(0, 7000) +
  annotate(
    geom = "label", x = 1100, y = 6800, label = "Average Runoff (m): \n Observed: 2003.8 \n Modelled: 1874.7"
  ) +
  scale_colour_brewer(palette="Dark2") +
  theme_bw() 

ggsave('../deliverables/paper-results/figs/fullCoast_annual_2021_basins.png', width = 8, height = 6, units = "in")

plot_ly(
  data = combine_ann, 
  x = ~ann_mm_gauge, y = ~ann_mm_mod, 
  type = "scatter", 
  mode = "markers", 
  color = ~regime,
  text = ~Name,
  hovertemplate = paste(
    "<b>%{text}</b><br>",
    "%{yaxis.title.text}: %{y:,.0f}<br>",
    "%{xaxis.title.text}: %{x:,.0f}")) %>%
  layout(
    xaxis = list(title = "Observed (mm)", range = c(0,7000)),
    yaxis = list(title = "Modelled (mm)", range = c(0,7000))
  )

# plot annual by regime 

summariseRegime <- combine_ann %>% 
  group_by(regime) %>% 
  summarise(ann_mm_gauge = mean(ann_mm_gauge),
            ann_mm_mod = mean(ann_mm_mod))

ggplot(summariseRegime, aes(x=ann_mm_gauge, y = ann_mm_mod)) +
  geom_point(aes(colour = factor(regime))) +
  labs(x='Observed (mm)', y = 'Modelled (mm)', colour = "Regime:") +
  geom_abline(intercept = 0, slope = 1) +
  # xlim(0, 4500) +
  # ylim(0, 4500) +
  scale_colour_brewer(palette="Dark2") +
  theme_bw() 

ggsave('fullCoast_amrit2020_summariseRegime.png', width = 8, height = 6, units = "in")

