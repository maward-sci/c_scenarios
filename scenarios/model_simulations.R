setwd('/Users/melissaward/Documents/Oxford Post-doc/code/c_scenarios')
source('./model/scen_generation.R') #load everything from the scen_generation.R file (akin to running that file)
library(ggplot2)

df <- create_seagrass_exp(model_params, n_sim = 20) #create a df of the simulated data (pulled form distributions)??? 

ggplot(df %>% filter(restoration_status=='Restoration')) +
  geom_line(aes(x = year, y = methane, color = sim, group = sim)) +
  facet_grid(~restoration_status)

summary <- summarize_simulations(df)
#add total mean column
summary$total_mean_carbon <- summary$soil_mean + 
  summary$biomass_mean - 
  summary$methane_mean - 
  summary$nitrous_oxide_mean

#add total SD column
summary$total_sd_carbon <- summary$soil_sd + 
  summary$biomass_sd - 
  summary$methane_sd - 
  summary$nitrous_oxide_sd

#### plots ##### 

## methane ##
ggplot(summary) +
  geom_line(aes(x = year, y = methane_mean)) +
  geom_ribbon(aes(x = year, ymin = methane_mean - methane_sd, ymax = methane_mean + methane_sd), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)

## N2O ##
ggplot(summary) +
  geom_line(aes(x = year, y = nitrous_oxide_mean)) +
  geom_ribbon(aes(x = year, ymin = nitrous_oxide_mean - nitrous_oxide_sd, ymax = nitrous_oxide_mean + nitrous_oxide_sd), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)

## Biomass ##
ggplot(summary) +
  geom_line(aes(x = year, y = biomass_mean)) +
  geom_ribbon(aes(x = year, ymin = biomass_mean - biomass_sd, ymax = biomass_mean + biomass_sd), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)

## Soil ##
ggplot(summary) +
  geom_line(aes(x = year, y = soil_mean)) +
  geom_ribbon(aes(x = year, ymin = soil_mean - soil_sd, ymax = soil_mean + soil_sd), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)

## Net Carbon Gain ##
ggplot(summary) +
  geom_line(aes(x = year, y = total_mean_carbon)) +
  geom_ribbon(aes(x = year, ymin = total_mean_carbon - total_sd_carbon, ymax = total_mean_carbon + total_sd_carbon), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)
