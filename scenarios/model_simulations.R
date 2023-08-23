setwd('/Users/melissaward/Documents/Oxford Post-doc/code/c_scenarios')
source('./model/scen_generation.R') #load everything from the scen_generation.R file (akin to running that file)
library(ggplot2)
library(tidyr)

##### create df, restructure, and summarize sims ####
df <- create_seagrass_exp(
  n_sim = 20,
  methane=methane,
  nitrous_oxide=nitrous_oxide,
  biomass=biomass,
  soil=soil
  ) #create a df of the simulated data (pulled form distributions)??? 

df <- compute_totals(df)

ggplot(df) + geom_line(aes(x = year, y = methane_netgain, color = sim, group = sim))

### Melt and summarize the simulation data
mdf <- melt_simulation_df(df)

# Note that you can pass in the appropriate grouping vars as an argument.
mdf_summary <- summarize_simulations(
  mdf,
  grouping_vars = c('scenario', 'year', 'metric', 'treatment')
) %>% select(-c('sim_mean', 'sim_sd'))


## rm some of the factors I dont want ## 
sum_df <- subset(mdf_summary, mdf_summary$metric != 'delta') 
sum_df <- subset(sum_df, sum_df$metric != 'area') 
sum_df <- subset(sum_df, sum_df$metric != 'area') 
sum_df <- subset(sum_df, sum_df$metric != 'overall') 
sum_df <- subset(sum_df, sum_df$treatment != 'netgain') 


ggplot(sum_df) +
  geom_line(aes(x = year, y = value_mean, color = scenario)) +
  facet_grid(treatment~metric)

## seems like there are some issues with the soil BAU for conservation, need to look into this. 


#### confused about what all this crap below is ###

##### plot melted summaries ####
ggplot(mdf_summary) +
  geom_line(aes(x = year, y = value_mean, color = scenario)) +
  facet_grid(treatment~metric)

ggplot(mdf_summary, aes(x= year, y=value_mean, color = treatment)) +
  geom_point() + geom_smooth() + theme_bw() + 
  facet_grid(treatment~metric)


###### Summarize raw data and plot plots #######
summary <- summarize_simulations(df) #summarize_simulations in Scen_generation file

#### METHANE ####
#restoration scenario 
ggplot(summary) +
  geom_line(aes(x = year, y = methane_mgmt_mean)) +
  geom_ribbon(aes(x = year, ymin = methane_mgmt_mean - methane_mgmt_sd, ymax = methane_mgmt_mean + methane_mgmt_sd), inherit.aes=TRUE)# +
#facet_grid(scenario~treatment)

ggplot(summary, aes(year, methane_mgmt_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-10,60) + ylab('Methane MGMT (Ceq/m2)')

#baseline scenario
ggplot(summary, aes(year, methane_bau_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-10,60) + ylab('Methane BAU (Ceq/m2)')

#net gain  
ggplot(summary, aes(year, methane_netgain_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-10,60) + ylab('Methane Net Gain (Ceq/m2)')



#### NOX ####
#restoration scenario 
ggplot(summary) +
  geom_line(aes(x = year, y = nox_mgmt_mean)) +
  geom_ribbon(aes(x = year, ymin = nox_mgmt_mean - nox_mgmt_sd, ymax = nox_mgmt_mean + nox_mgmt_sd), inherit.aes=TRUE)# +
#facet_grid(scenario~treatment)

ggplot(summary, aes(year, nox_mgmt_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-10,40) + ylab('NOX MGMT (Ceq/m2)')

#baseline scenario
ggplot(summary, aes(year, nox_bau_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-10,40) + ylab('NOX BAU (Ceq/m2)')

#net gain  
ggplot(summary, aes(year, nox_netgain_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-10,40) + ylab('NOX Net Gain (Ceq/m2)')


#### BIOMASS ####
#restoration scenario 
ggplot(summary, aes(year, biomass_mgmt_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-500,1200) + ylab('Biomass MGMT (Ceq/m2)') 

#baseline scenario
ggplot(summary, aes(year, biomass_bau_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-500,1200) + ylab('Biomass BAU (Ceq/m2)')

#net gain  
ggplot(summary, aes(year, biomass_netgain_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-1200,1200) + ylab('Biomass Net Gain (Ceq/m2)')



#### SOIL ####
#restoration scenario 
ggplot(summary, aes(year, soil_mgmt_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylab('Soil MGMT (Ceq/m2)') 

#baseline scenario
ggplot(summary, aes(year, soil_bau_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylim(-50,300) + ylab('Soil BAU (Ceq/m2)') 

#net gain  
ggplot(summary, aes(year, biomass_netgain_mean, color= scenario)) + 
  geom_point() + geom_smooth() + theme_bw() + ylab('Biomass Net Gain (Ceq/m2)')









## Net Carbon Gain ##
ggplot(summary) +
  geom_line(aes(x = year, y = total_mean_carbon)) +
  geom_ribbon(aes(x = year, ymin = total_mean_carbon - total_sd_carbon, ymax = total_mean_carbon + total_sd_carbon), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)
  
ggplot(summary, aes(year, total_mean_carbon, color= treatments)) + 
  geom_point() + geom_smooth() + theme_bw()
