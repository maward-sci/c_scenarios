# create data table for simulations
# using reference model paramter values
# from the model_params dataframe
library(dplyr)
library(docstring)
################################
# Define Paramters
################################
# Create the class we will use to store model parameters
# This just provides a single object where all the params are located for conveniance.
# setClass(
#   "ModelParams",
#   slots=list(
#     methane="data.frame",
#     nitrous_oxide="data.frame",
#     biomass="data.frame",
#     soil="data.frame"
#   ))
# DataFrame with Model Parameters (roughly estiamted from Oreska et al. Table 3)
methane <- data.frame( #LTER data, Oreska 2020
    mean_unvegetated = 0.6812,
    sd_unvegetated = 0.4768, #SE
    mean_vegetated = 5.5000,#0.2 Metirc tons CO2eq per ha per yr in veg sites
    sd_vegetated = 3.6103, #SE
    units = "g_Ceq/m2",
    mean_infill = 0.6812, #same as UNVEGETATED
    sd_infill = 0.4768, #same as UNVEGETATED, SE
    mean_dredge = 0.6812, #same as UNVEGETATED
    sd_dredge = 0.4768 #same as UNVEGETATED
    )
nitrous_oxide <- data.frame( #LTER data, Oreska 2020
  mean_unvegetated = 1.6240,#0.06 metric tons co2eq per ha per yr in unveg sites
  sd_unvegetated = 0.8200, #se
  mean_vegetated = 4.8719,
  sd_vegetated = 3.2480, 
  mean_infill = 1.6240, #same as UNVEGETATED
  sd_infill = 0.8200, #same as UNVEGETATED, SE
  mean_dredge = 1.6240, #same as UNVEGETATED
  sd_dredge = 0.8200, #same as UNVEGETATED, SE
  units = "g_Ceq/m2"
  )
biomass <- data.frame(
  mean_unvegetated = 0,
  sd_unvegetated = 0,
  mean_vegetated = 181, #total (not per yr) - model runs cumulatively for biomass areas  sd_vegetated = 1,
  sd_vegetated = 7.74,
  units = ""
  )
soil <- data.frame(
  mean_unvegetated = 10, #based very roughly Greiner fig. 3
  sd_unvegetated = 2,#made this up, none reported
  mean_vegetated = 36.68, #Greiner fig. 3, g C m2 yr
  sd_vegetated = 2.79,
  mean_infill = 10, #same as UNVEGETATED
  sd_infill = 2, #same as UNVEGETATED
  units = "g_C/m2",
  mean_delta_unvegetated = 0.01, # delta = C density (g/m3), made up numbers need to get units and correct values
  sd_delta_unvegetated = 0.01, # made up numbers need to get units and correct values
  infill_depth = 10, # made up numbers need to get units and correct values
  mean_delta_vegetated = 0.1, # made up numbers need to get units and correct values
  sd_delta_vegetated = 0.1, # made up numbers need to get units and correct values
  drege_depth = -10, # made up numbers need to get units and correct values
  infill_proportion_remin = 0.5 # percent
  )

# model_params <- new(
#   "ModelParams",
#   methane=methane,
#   nitrous_oxide=nitrous_oxide,
#   biomass=biomass,
#   soil = soil
# )

################################
# Define Functions
################################

# https://www.r-bloggers.com/2019/02/anatomy-of-a-logistic-growth-curve/
# https://www.tjmahr.com/anatomy-of-a-logistic-growth-curve/
# test: plot(logistic_growth(0:10))

log_growth_general <- function(years, scale = 1, asymptote = 6, year_midpoint = NULL, midpoint = NULL ){
  #" log_growth_general
  #" 
  #" @description Compute logistic growth curve as a function of integer "years"
  #"
  #" @param years list. a list of integers representing successive years in the experiment
  #" @param scale integer. used to scale the result of (midpoint - years) in the logistic growth function
  #" @param asymptote integer  the saturation (maximum) value of the response variable in the growth curve
  #" @param year_midpoint integer  Specifies the year (i.e., x-axis value) that should represent the midpoint of the growth curve
  #" Used to compute the `midpoint` parameter, unless `midpoint` is specified.
  #" @param midpoint integer  Optionally take control of the midpoint parameter in the logistic growth equation
  #" if used, ignores year_midpoint argument.
  #"
  #" @usage transplant = log_growth_general(
  #"                        years = 1:10,
  #"                        scale = 1,
  #"                        asymptote = 60,
  #"                        midpoint = NULL,
  #"                        year_midpoint = 4.3
  #"                      )
  #" @return list of length `length(years)` - The logistic growth values of the response variable over time.
  #" In this model, that var is seagrass area in square-meters
  asymptote <- asymptote # max size of meadow
  scale <- scale
  if (is.null(midpoint)){
    midpoint <- length(years) / (10 / year_midpoint) #midpoint at 4.3 yrs
  }
  seagrass_area_m2 <- asymptote / (1 + exp((midpoint - years) * scale))
  return(seagrass_area_m2)
}

### build the scenarios simulations ###
create_seagrass_exp <- function(
  n_sim,
  methane,
  nitrous_oxide,
  soil,
  biomass,
  scenarios = c("Seed", "Transplant", "Infill", "Conservation"),
  n_years = 10
  ){
  #" create_seagrass_exp
  #" 
  #" @description Run a full set of n_sim simulations of predefined restoration scenarios (i.e., "scenarioss").
  #"
  #" @param n_sim integer. how many simulations to run
  #" @param methane Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param nitrous_oxide Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param soil Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param biomass Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param scenarios list of scenarios to simulate
  #" @param n_years integer The number of years to simulate
  
  #" @usage create_seagrass_exp(
  #"  model_params = model_params,
  #"  n_sim = 10
  #"  )
  #" @return Dataframe with the simulated area, methane, nitrous_oxide, biomass, and soil values for each restoration method and its baseline
  years <- seq(from = 0, to = n_years)
  plot_growth <- simulate_plot_growth(years=years)
### create full-factorial combination of the above descriptive variables (i.e., for each scenario.)
  # -- duplicates the full set of parameters for each scenario
  df <- expand.grid(
    scenario = scenarios,
    year = years,
    sim = 1:n_sim
    )
### create project size field
  df <- df %>% left_join(plot_growth, by = c("year", "scenario"), copy = TRUE)
  # draw parameter values from the corresponding distribution
  # METHANE
  df <- simulate_gas(model_df = df, gas_df = methane, gas_type = 'methane')
  # NOX
  df <- simulate_gas(model_df = df, gas_df = nitrous_oxide, gas_type = 'nox')
  # BIOMASS
  df <- simulate_biomass(model_df = df, gas_df = biomass)
  # SOIL
  df <- simulate_soil(model_df = df, gas_df = soil)
  return(df)
}

# Helper functions that define growth models for various components
##SOIL##
simulate_soil <- function(model_df, gas_df){
  soil_bau_colname <- paste('soil', 'bau', sep = '_')
  soil_mgmt_colname <- paste('soil', 'mgmt', sep = '_')
# Transplant
  transplant_rows <- which(model_df$scenario %in% c('Transplant'))
  # BAU
  model_df[transplant_rows, soil_bau_colname] <- rnorm(
    n = length(transplant_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[transplant_rows])
  # MGMT
  model_df[transplant_rows, soil_mgmt_colname] <- rnorm(
    n = length(transplant_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * model_df[transplant_rows, 'area_m2']
# Infill
  infill_rows <- which(model_df$scenario %in% c('Infill'))
  # BAU
  model_df[infill_rows, soil_bau_colname] <- rnorm(
    n = length(infill_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * model_df[infill_rows, 'area_m2']
  # MGMT
  model_df[infill_rows, soil_mgmt_colname] <- rnorm(
    n = length(infill_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * model_df[infill_rows, 'area_m2']
  # overwrite year 0 with unvegetated params
  infill_year_0 <- which((model_df$scenario == 'Infill') & (model_df$year == min(model_df$year)))
  model_df[infill_year_0, soil_mgmt_colname] <- rnorm(
      n = length(infill_year_0),
      mean = as.numeric(gas_df["mean_unvegetated"]),
      sd = as.numeric(gas_df["sd_unvegetated"])
      ) * as.numeric(model_df$area_m2[infill_year_0]) + 
      rnorm(
        n = length(infill_year_0),
        mean = as.numeric(gas_df["mean_delta_unvegetated"]),
        sd = as.numeric(gas_df["sd_delta_unvegetated"])
        ) * as.numeric(model_df$area_m2[infill_year_0]) * gas_df$infill_depth
# Seed
  seed_rows <- which(model_df$scenario %in% c('Seed'))
  # BAU
  model_df[seed_rows, soil_bau_colname] <- rnorm(
    n = length(seed_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[seed_rows])
  # MGMT
  model_df[seed_rows, soil_mgmt_colname] <- rnorm(
    n = length(seed_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * model_df[seed_rows, 'area_m2']
# Conservation
  conservation_rows <- which(model_df$scenario %in% c('Conservation'))
  # BAU
  model_df[conservation_rows, soil_bau_colname] <- rnorm(
    n = length(conservation_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * model_df[conservation_rows, 'area_m2']
  # replace year 0 for BAU
  conservation_year_0 <- which((model_df$scenario == 'Conservation') & (model_df$year == min(model_df$year)))
  model_df[conservation_year_0, soil_bau_colname] <- rnorm(
      n = length(conservation_year_0),
      mean = as.numeric(gas_df["mean_delta_vegetated"]),
      sd = as.numeric(gas_df["sd_delta_vegetated"])
      ) * as.numeric(model_df$area_m2[conservation_year_0]) * gas_df$drege_depth +
      rnorm(
        n = length(conservation_year_0),
        mean = as.numeric(gas_df["mean_delta_unvegetated"]),
        sd = as.numeric(gas_df["sd_delta_unvegetated"])
        ) * as.numeric(model_df$area_m2[conservation_year_0]) * gas_df$drege_depth
  # MGMT
  model_df[conservation_rows, soil_mgmt_colname] <- rnorm(
    n = length(conservation_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * model_df[conservation_rows, 'area_m2']
  return(model_df)
}

##BIOMASS##
simulate_biomass <- function(model_df, gas_df){
  # create delta_area column
  model_df <- model_df %>%
    group_by(scenario, sim) %>%
    mutate(delta_area = area_m2 - lag(area_m2, order_by = year)) %>%
    arrange(scenario, sim, year)
  biomass_bau_colname <- paste('biomass', 'bau', sep = '_')
  biomass_mgmt_colname <- paste('biomass', 'mgmt', sep = '_')
# Transplant
  transplant_rows <- which(model_df$scenario %in% c('Transplant'))
  # BAU
  model_df[transplant_rows, biomass_bau_colname] <- rnorm(
    n = length(transplant_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[transplant_rows])
  # MGMT
  model_df[transplant_rows, biomass_mgmt_colname] <- rnorm(
    n = length(transplant_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * model_df[transplant_rows, 'delta_area'] # incremental additional vegetated area at t > 0
  # replace year 0
  # overwrite year 0 with mean_infill param
  transplant_year_0 <- which((model_df$scenario == 'Transplant') & (model_df$year == min(model_df$year)))
  model_df[transplant_year_0, biomass_mgmt_colname] <- rnorm(
      n = length(transplant_year_0),
      mean = as.numeric(gas_df["mean_vegetated"]),
      sd = as.numeric(gas_df["sd_vegetated"])
      ) * as.numeric(model_df$area_m2[transplant_year_0])
# Infill
  infill_rows <- which(model_df$scenario %in% c('Infill'))
  # BAU
  model_df[infill_rows, biomass_bau_colname] <- rnorm(
    n = length(infill_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * model_df[infill_rows, 'area_m2']
  # MGMT
  model_df[infill_rows, biomass_mgmt_colname] <- rnorm(
    n = length(infill_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * model_df[infill_rows, 'delta_area'] # incremental additional vegetated area at t > 1
  # overwrite year 0 with unvegetated params
  infill_year_0 <- which((model_df$scenario == 'Infill') & (model_df$year == min(model_df$year)))
  model_df[infill_year_0, biomass_mgmt_colname] <- rnorm(
      n = length(infill_year_0),
      mean = as.numeric(gas_df["mean_unvegetated"]),
      sd = as.numeric(gas_df["sd_unvegetated"])
      ) * as.numeric(model_df$area_m2[infill_year_0])
  # overwrite year 1 with vegetated params
  infill_year_1 <- which((model_df$scenario == 'Infill') & (model_df$year == min(model_df$year)+1))
  model_df[infill_year_1, biomass_mgmt_colname] <- rnorm(
      n = length(infill_year_1),
      mean = as.numeric(gas_df["mean_vegetated"]),
      sd = as.numeric(gas_df["sd_vegetated"])
      ) * as.numeric(model_df$area_m2[infill_year_1])
# Seed
  seed_rows <- which(model_df$scenario %in% c('Seed'))
  # BAU
  model_df[seed_rows, biomass_bau_colname] <- rnorm(
    n = length(seed_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[seed_rows])
  # MGMT
  model_df[seed_rows, biomass_mgmt_colname] <- rnorm(
    n = length(seed_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * model_df[seed_rows, 'delta_area'] # incremental additional vegetated area at t > 0
  # overwrite year 0 with full area (not delta/incremental area)
  seed_year_0 <- which((model_df$scenario == 'Seed') & (model_df$year == min(model_df$year)))
  model_df[seed_year_0, biomass_mgmt_colname] <- rnorm(
      n = length(seed_year_0),
      mean = as.numeric(gas_df["mean_vegetated"]),
      sd = as.numeric(gas_df["sd_vegetated"])
      ) * as.numeric(model_df$area_m2[seed_year_0])
# Conservation
  conservation_rows <- which(model_df$scenario %in% c('Conservation'))
  # BAU
  model_df[conservation_rows, biomass_bau_colname] <- rnorm(
    n = length(conservation_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * model_df[conservation_rows, 'area_m2'] # incremental additional vegetated area at t > 0
  # uses vegetated params for year 0 rather than unvegetated
  conservation_year_0 <- which((model_df$scenario == 'Conservation') & (model_df$year == min(model_df$year)))
    # this is * -1 b/c in year 0 we lose a bunch of carbon due to dredging.
  model_df[conservation_year_0, biomass_bau_colname] <- -1 * rnorm(
      n = length(conservation_year_0),
      mean = as.numeric(gas_df["mean_vegetated"]),
      sd = as.numeric(gas_df["sd_vegetated"])
      ) * as.numeric(model_df$area_m2[conservation_year_0])
  # MGMT
  model_df[conservation_rows, biomass_mgmt_colname] <- 0
  # drop delta_area
  #model_df <- model_df[,-which(names(model_df) == 'delta_area')]
  return(model_df)
}

##NOX & METHANE##
simulate_gas <- function(model_df, gas_df, gas_type){
  model_df <- model_df %>% arrange(scenario, sim, year)
# Transplant
  transplant_rows <- which(model_df$scenario %in% c('Transplant'))
  # BAU
  model_df[transplant_rows, paste(gas_type, 'bau', sep = '_')] <- rnorm(
    n = length(transplant_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[transplant_rows])
  # MGMT
  model_df[transplant_rows, paste(gas_type, 'mgmt', sep = '_')] <- rnorm(
    n = length(transplant_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * as.numeric(model_df$area_m2[transplant_rows])
# Infill
  infill_rows <- which(model_df$scenario %in% c('Infill'))
  # BAU
  model_df[infill_rows, paste(gas_type, 'bau', sep = '_')] <- rnorm(
    n = length(infill_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[infill_rows])
  # MGMT
  model_df[infill_rows, paste(gas_type, 'mgmt', sep = '_')] <- rnorm(
    n = length(infill_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * as.numeric(model_df$area_m2[infill_rows])
  # overwrite year 0 with mean_infill param
  infill_year_0 <- which((model_df$scenario == 'Infill') & (model_df$year == min(model_df$year)))
  model_df[infill_year_0, paste(gas_type, 'mgmt', sep = '_')] <- rnorm(
      n = length(infill_year_0),
      mean = as.numeric(gas_df["mean_infill"]),
      sd = as.numeric(gas_df["sd_infill"])
      ) * as.numeric(model_df$area_m2[infill_year_0])
# Seed
  seed_rows <- which(model_df$scenario %in% c('Seed'))
  # BAU
  model_df[seed_rows, paste(gas_type, 'bau', sep = '_')] <- rnorm(
    n = length(seed_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[seed_rows])
  # MGMT
  model_df[seed_rows, paste(gas_type, 'mgmt', sep = '_')] <- rnorm(
    n = length(seed_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * as.numeric(model_df$area_m2[seed_rows])
# Conservation
  cons_rows <- which(model_df$scenario %in% c('Conservation'))
  # BAU
  model_df[cons_rows, paste(gas_type, 'bau', sep = '_')] <- rnorm(
    n = length(cons_rows),
    mean = as.numeric(gas_df["mean_unvegetated"]),
    sd = as.numeric(gas_df["sd_unvegetated"])
    ) * as.numeric(model_df$area_m2[cons_rows])
  # overwrite year 0 with mean_infill param
  conservation_year_0 <- which((model_df$scenario == 'Conservation') & (model_df$year == min(model_df$year)))
  model_df[conservation_year_0, paste(gas_type, 'bau', sep = '_')] <- rnorm(
      n = length(conservation_year_0),
      mean = as.numeric(gas_df["mean_dredge"]),
      sd = as.numeric(gas_df["sd_dredge"])
      ) * as.numeric(model_df$area_m2[conservation_year_0])
  # MGMT
  model_df[cons_rows, paste(gas_type, 'mgmt', sep = '_')] <- rnorm(
    n = length(cons_rows),
    mean = as.numeric(gas_df["mean_vegetated"]),
    sd = as.numeric(gas_df["sd_vegetated"])
    ) * as.numeric(model_df$area_m2[cons_rows])
  return(model_df)
}

##AREA##
simulate_plot_growth <- function(years, plot_growth_asymptote = 6){
# Transplant scenario
  plot_growth_transplant <- as.data.frame(cbind(
    year = as.numeric(years),
    area_m2 = log_growth_general(
      years = years,
      scale = 1,
      asymptote = plot_growth_asymptote,
      midpoint = NULL,
      year_midpoint = 4.3
      ),
    scenario = 'Transplant'
  ))
# Infill scenario
  plot_growth_infill <- as.data.frame(cbind(
    year = as.numeric(years),
    area_m2 = c(
      plot_growth_asymptote,
      log_growth_general(
        years = years,
        scale = 1,
        asymptote = plot_growth_asymptote,
        midpoint = NULL,
        year_midpoint = 4.3
      )[1:length(years)-1]
    ),
    scenario = 'Infill'
  ))
# Seed scenario
  plot_growth_seed <- as.data.frame(cbind(
    year = as.numeric(years),
    area_m2 = log_growth_general(
      years = years,
      scale = 1,
      asymptote = plot_growth_asymptote,
      midpoint = NULL,
      year_midpoint = 5.55
    ),
    scenario = 'Seed'
  ))
# Conservation Scenario
  plot_growth_cons <- as.data.frame(cbind(
    year = as.numeric(years),
    area_m2 = plot_growth_asymptote,
    scenario = 'Conservation'
  ))
  # Combine Growth Curves
  plot_growth <- rbind(
    plot_growth_transplant,
    plot_growth_infill,
    plot_growth_seed,
    plot_growth_cons
  )
  plot_growth$year <- as.numeric(as.character(plot_growth$year))
  plot_growth$area_m2 <- as.numeric(as.character(plot_growth$area_m2))
  return(plot_growth)
}

##After running create_seagress_exp, compute simulation totals
compute_totals <- function(df){
  #df_out <- df[,c('scenario', 'year', 'sim', 'area_m2')] #keep a subset of df
  df_out <- df
  df_out$nox_netgain <- df$nox_mgmt - df$nox_bau
  df_out$methane_netgain <- df$methane_mgmt - df$nox_bau  
  df_out$soil_netgain <- df$soil_mgmt - df$soil_bau  
  df_out$biomass_netgain <- df$biomass_mgmt - df$biomass_bau  
  df_out$overall_netgain <- df_out$soil_netgain + df_out$biomass_netgain - df_out$methane_netgain - df_out$nox_netgain
  return(df_out)
}
  

## summarizes the simulation outputs 
summarize_simulations <- function(
  df,
  grouping_vars = c('scenario', 'year','treatment')
  ){
  #" summarize_simulations
  #" 
  #" @description Compute the carbon constituent emissions value as a function of area for vegetated habitats
  #"
  #" @param df Dataframe. The simulations resulting form `create_seagrass_exp()`
  
  #" @usage summarize_simulations(
  #"  df = create_seagrass_exp(model_params,n_sims=5)
  #"  )
  #" @return Dataframe with the mean and standard deviation across simulations
  #" for simulated area, methane, nitrous_oxide, biomass, and soil values for each mgmt and bau
  summary <- df %>%
    group_by_at( grouping_vars ) %>%
    summarize_if(.predicate = is.numeric,
    .funs = c(mean = mean, sd = sd), na.rm=TRUE)
  return(summary)
}


### now melt summary so that we have another column with the "BAUvsMGMT
## Restructure db with melt##
melt_simulation_df <- function(df, id_vars){
  id_vars <- c('scenario', 'year', 'sim')
  rdf <- reshape2::melt(df, id_vars)
  area_df <- rdf %>% 
    dplyr::filter(variable == 'area_m2') %>%
    tidyr::separate(variable,into=c('metric', 'unit'), sep='_') %>%
    mutate(treatment = 'all')
  metric_df <- rdf %>% 
    dplyr::filter(variable != 'area_m2') %>%
    tidyr::separate(variable,into=c('metric', 'treatment'), sep='_') %>%
    mutate(unit=NA)
  rdf = rbind(
    area_df,
    metric_df
  )
  return(rdf)
}

