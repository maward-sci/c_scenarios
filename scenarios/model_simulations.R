setwd('/Users/melissaward/Documents/Oxford Post-doc/code/c_scenarios')
source('./model/scen_generation.R') #load everything from the scen_generation.R file (akin to running that file)
library(ggplot2)

df <- create_seagrass_exp(
  n_sim = 20,
  methane=methane,
  nitrous_oxide=nitrous_oxide,
  biomass=biomass,
  soil=soil
  ) #create a df of the simulated data (pulled form distributions)??? 

df <- compute_totals(df)

#ggplot(df) +
  geom_line(aes(x = year, y = total_methane_gains, color = sim, group = sim))

### Melt and summarize the simulation data
mdf <- melt_simulation_df(df)
# Note that you can pass in the appropriate grouping vars as an argument.
mdf_summary <- summarize_simulations(
  mdf,
  grouping_vars = c('treatments', 'year', 'metric', 'restoration_status')
) %>% select(-c('sim_mean', 'sim_sd'))
# Plot melted summaries
ggplot(mdf_summary) +
  geom_line(aes(x = year, y = value_mean, color = treatments)) +
  facet_grid(restoration_status~metric)

ggplot(mdf_summary, aes(x= year, y=value_mean, color = treatments)) +
  geom_point() + geom_smooth() + theme_bw() + 
  facet_grid(restoration_status~metric)

###### Summarize raw data and plot plots #######
summary <- summarize_simulations(df) #summarize_simulations in Scen_generation file
## methane ##
  #restoration scenario 
ggplot(summary) +
  geom_line(aes(x = year, y = methane_carbon_rest_mean)) +
  geom_ribbon(aes(x = year, ymin = methane_carbon_rest_mean - methane_carbon_rest_sd, ymax = methane_carbon_rest_mean + methane_carbon_rest_sd), inherit.aes=TRUE)# +
  #facet_grid(treatments~restoration_status)

ggplot(summary, aes(year, methane_carbon_rest_mean, color= treatments)) + 
  geom_point() + geom_smooth() + theme_bw()

  #baseline scenario
ggplot(summary, aes(year, methane_carbon_unvegetated_mean, color= treatments)) + 
  geom_point() + geom_smooth() + theme_bw()


## N2O ##
  #restoration/project scenarios 
ggplot(summary) +
  geom_line(aes(x = year, y = nox_carbon_rest_mean)) + 
  geom_ribbon(aes(x = year, ymin = nox_carbon_rest_mean - nox_carbon_rest_sd, ymax = nox_carbon_rest_mean + nox_carbon_rest_sd), inherit.aes=TRUE)# +
#facet_grid(treatments~restoration_status)

ggplot(summary, aes(year, nox_carbon_rest_mean, color= treatments)) + 
  geom_point() + geom_smooth() + theme_classic()
#baseline scenario
ggplot(summary, aes(year, nox_carbon_unvegetated_mean, color= treatments)) + 
  geom_point() + geom_smooth() + theme_bw()


## Biomass ##
ggplot(summary) +
  geom_line(aes(x = year, y = biomass_carbon_rest_mean)) +
  geom_ribbon(aes(x = year, ymin = biomass_carbon_rest_mean - biomass_carbon_rest_sd, ymax = biomass_carbon_rest_mean + biomass_carbon_rest_sd), inherit.aes=TRUE)
## Soil ##
ggplot(summary) +
  geom_line(aes(x = year, y = soil_carbon_rest_mean)) +
  geom_ribbon(aes(x = year, ymin = soil_carbon_rest_mean - soil_carbon_rest_sd, ymax = soil_carbon_rest_mean + soil_carbon_rest_sd), inherit.aes=TRUE)# +
#facet_grid(treatments~restoration_status)

ggplot(summary, aes(year, soil_carbon_rest_mean, color= treatments)) + 
  geom_point() + geom_smooth() + theme_bw()

#baseline scenario
ggplot(summary, aes(year, soil_carbon_unvegetated_mean, color= treatments)) + 
  geom_point() + geom_smooth() + theme_bw()

## Net Carbon Gain ##
ggplot(summary) +
  geom_line(aes(x = year, y = total_mean_carbon)) +
  geom_ribbon(aes(x = year, ymin = total_mean_carbon - total_sd_carbon, ymax = total_mean_carbon + total_sd_carbon), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)
  
ggplot(summary, aes(year, total_mean_carbon, color= treatments)) + 
  geom_point() + geom_smooth() + theme_bw()
