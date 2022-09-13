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
methane <- data.frame(
    mean_unvegetated = 0.025,#0.025 met T co2eq per ha per yr in unveg sites
    sd_unvegetated = 0.01,
    mean_vegetated = 0.2,#0.2 Metirc tons CO2eq per ha per yr in veg sites
    sd_vegetated = 0.08,
    units = "co2eq/ha",
    mean_infill = 0.3,
    sd_infill = 0.1
    )
nitrous_oxide <- data.frame(
  mean_unvegetated = 0.06,#0.06 metric tons co2eq per ha per yr in unveg sites
  sd_unvegetated = 0.02,
  mean_vegetated = 0.5,
  sd_vegetated = 0.15,
  mean_infill = 0.5,
  sd_infill = 0.15,
  units = "co2eq/ha"
  )
biomass <- data.frame(
  mean_unvegetated = 0,
  sd_unvegetated = 0,
  mean_vegetated = 0.4,
  sd_vegetated = 0.02,
  units = ""
  )
soil <- data.frame(
  mean_unvegetated = 8.125, #based very roughly on Oreska table 3 but needs work
  sd_unvegetated = 0.8,
  mean_vegetated = 20, # grams / cubed-meter # Mel says 3000 is a better estimate.
  sd_vegetated = 2,
  mean_infill = 8.125, # grams / cubed-meter # Mel says 3000 is a better estimate.
  sd_infill = 0.8,
  units = "grams/cubed-meter",
  infill_proportion_remin = 0.5 # perecent
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

log_growth_general <- function(years, scale = 1, asymptote = 60, year_midpoint = NULL, midpoint = NULL ){
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


### generic version 
carbon_per_vegetated_area <- function(area, carbon_param){
  #" carbon_per_vegetated_area
  #" 
  #" @description Compute the carbon constituent emissions value as a function of area for vegetated habitats
  #"
  #" @param area list. a list of numeric values representing the area in hectares of a plot over time
  #" @param carbon_param Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param: one of `methane`, `nitrous_oxide`, `biomass`, or `soil`
  
  #" @usage carbon_per_vegetated_area(
  #"  area = 1:10,
  #"  carbon_param = "methane"
  #"  )
  #" @return list of length `length(area)` - The `carbon_param` values as a function of area
  carbon_rest <- area * as.numeric(
    rnorm(
      n = length(area),
      mean = as.numeric(carbon_param$mean_vegetated),
      sd = as.numeric(carbon_param$sd_vegetated)
    )
  )
  return(carbon_rest)
}


### build the scenarios simulations ###
create_seagrass_exp <- function(
  n_sim,
  methane,
  nitrous_oxide,
  soil,
  biomass,
  treatments = c("Seed", "Transplant", "Infill"),
  restoration_status = c("Baseline", "Restoration"),
  n_years = 10
  ){
  #" create_seagrass_exp
  #" 
  #" @description Run a full set of n_sim simulations of predefined restoration treatments (i.e., "scenarios").
  #"
  #" @param n_sim integer. how many simulations to run
  #" @param methane Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param nitrous_oxide Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param soil Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param biomass Dataframe the table of parameters for vegetated and unvegetated areas for each carbon_param
  #" @param treatments list of treatments to simulate
  #" @param restoration_status List of some combo of "Baseline" and "Restoration"
  #" @param n_years integer The number of years to simulate
  
  #" @usage create_seagrass_exp(
  #"  model_params = model_params,
  #"  n_sim = 10
  #"  )
  #" @return Dataframe with the simulated area, methane, nitrous_oxide, biomass, and soil values for each restoration method and its baseline
  years <- seq(from = 0, to = n_years)
  plot_growth <- simulate_plot_growth(years=years)
### create full-factorial combination of the above descriptive variables
  df <- expand.grid(
    treatments = treatments,
    restoration_status = restoration_status,
    year = years,
    sim = 1:n_sim
    )
### create project size field
  df <- df %>% left_join(plot_growth, by = c("year", "treatments"), copy = TRUE)
  # draw parameter values from the corresponding distribution
  # METHANE
  df <- simulate_methane(model_df = df, methane_df = methane)
  # NOX
  df <- simulate_nox(model_df = df, nox_df = nitrous_oxide)
  # BIOMASS
  df <- simulate_biomass(model_df = df, biomass_df = biomass)
  # SOIL
  df <- simulate_soil(model_df = df, soil_df = soil, depth_infill_m = 0.2, depth_veg_accretion_m = 0.2, depth_unveg_accretion_m = 0.2)
  # df[which(df$restoration_status=="Restoration"), "soil"] <- carbon_per_vegetated_area(
  #   df[which(df$restoration_status=="Restoration"), "vegetated_area_m2"],
  #   carbon_param = soil
  # )
  # set N2O for Restoration where it is dependant on project_size_m2
  # Modify some parameters based on descriptive variables
  # x <- df[which(df$restoration_status == "Restoration"),]
  return(df)
}

simulate_soil <- function(model_df, soil_df, depth_infill_m, depth_veg_accretion_m, depth_unveg_accretion_m){
  # pull together the three soil constituents:
    # Veg Natural soil contribution: Area * rho_soil_veg * depth_veg_accretion_m
    # UnVeg Natural soil contribution: Area * rho_soil_unveg * depth_unveg_accretion_m
    # Infill soil contribution: Area * rho_soil_unveg * depth_unveg_accretion_m * percent_remineralized
  # Carbon sequestration amounts are computed as columns in model_df with units of grams / year
  model_df$soil_carbon_veg <- rnorm(
    n = nrow(model_df),
    as.numeric(soil_df$mean_vegetated),
    sd = as.numeric(soil_df$sd_vegetated)) * model_df$vegetated_area_m2 * depth_veg_accretion_m
  model_df$soil_carbon_unveg <- rnorm(
    n = nrow(model_df),
    as.numeric(soil_df$mean_unvegetated),
    sd = as.numeric(soil_df$sd_unvegetated)) * model_df$unvegetated_area_m2 * depth_unveg_accretion_m
  model_df$soil_carbon_infill <- rnorm(
    n = nrow(model_df),
    as.numeric(soil_df$mean_infill),
    sd = as.numeric(soil_df$sd_infill)) * model_df$infill_area_m2 * depth_infill_m * as.numeric(soil_df$infill_proportion_remin)
  model_df$soil_carbon_total <- model_df$soil_carbon_veg + model_df$soil_carbon_unveg + model_df$soil_carbon_infill
  return(model_df)
}

simulate_biomass <- function(model_df, biomass_df){
  # biomass is a function of vegetated area only, because the biomass is the vegetation itself.
  model_df$biomass_carbon <- rnorm(
    n = nrow(model_df),
    mean = as.numeric(biomass_df["mean_vegetated"]),
    sd = as.numeric(biomass_df["sd_vegetated"])
    ) * model_df$vegetated_area_m2
  return(model_df)
}

simulate_nox <- function(model_df, nox_df){
  model_df$nox_carbon_unvegetated <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(nox_df["mean_unvegetated"]),
    sd = as.numeric(nox_df["sd_unvegetated"])
    ) * model_df$unvegetated_area_m2
  model_df$nox_carbon_vegetated <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(nox_df["mean_vegetated"]),
    sd = as.numeric(nox_df["sd_vegetated"])
    ) * model_df$vegetated_area_m2
  model_df$nox_carbon_infill <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(nox_df["mean_infill"]),
    sd = as.numeric(nox_df["sd_infill"])
    ) * model_df$infill_area_m2
  model_df$nox_carbon_total <- model_df$nox_carbon_unvegetated + model_df$nox_carbon_vegetated + model_df$nox_carbon_infill
  return(model_df)
}

simulate_methane <- function(model_df, methane_df){
  model_df$methane_carbon_unvegetated <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(methane_df["mean_unvegetated"]),
    sd = as.numeric(methane_df["sd_unvegetated"])
    ) * model_df$unvegetated_area_m2
  model_df$methane_carbon_vegetated <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(methane_df["mean_vegetated"]),
    sd = as.numeric(methane_df["sd_vegetated"])
    ) * model_df$vegetated_area_m2
  model_df$methane_carbon_infill <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(methane_df["mean_infill"]),
    sd = as.numeric(methane_df["sd_infill"])
    ) * model_df$infill_area_m2
  model_df$methane_carbon_total <- model_df$methane_carbon_unvegetated + model_df$methane_carbon_vegetated + model_df$methane_carbon_infill
  return(model_df)
}

simulate_plot_growth <- function(years, plot_growth_asymptote = 60){
  # Transplant scenario
  plot_growth_transplant <- as.data.frame(cbind(
    year = years,
    vegetated_area_m2 = log_growth_general(
      years = years,
      scale = 1,
      asymptote = plot_growth_asymptote,
      midpoint = NULL,
      year_midpoint = 4.3
      ),
    unvegetated_area_m2 = log_growth_general(
      years = years,
      scale = 1,
      asymptote = plot_growth_asymptote,
      midpoint = NULL,
      year_midpoint = 4.3
      ),
    infill_area_m2 = 0
  ))
  plot_growth_transplant$treatments <- "Transplant"
  # InFill scenario
  plot_growth_infill <- as.data.frame(cbind(
    year = years,
    vegetated_area_m2 = c(
      0,
      log_growth_general(
        years = years,
        scale = 1,
        asymptote = plot_growth_asymptote,
        midpoint = NULL,
        year_midpoint = 4.3
      )[1:length(years)-1]
    ),
    unvegetated_area_m2 = c(
      0,
      log_growth_general(
        years = years,
        scale = 1,
        asymptote = plot_growth_asymptote,
        midpoint = NULL,
        year_midpoint = 4.3
      )[1:length(years)-1]
    ),
    infill_area_m2 = c(plot_growth_asymptote, rep(0, times = (length(years)-1)))
  ))
  plot_growth_infill$treatments <- "Infill"
  # Seed scenario
  plot_growth_seed <- as.data.frame(cbind(
    year = years,
    vegetated_area_m2 = log_growth_general(
      years = years,
      scale = 1,
      asymptote = plot_growth_asymptote,
      midpoint = NULL,
      year_midpoint = 5.55
    ),
    unvegetated_area_m2 = log_growth_general(
      years = years,
      scale = 1,
      asymptote = plot_growth_asymptote,
      midpoint = NULL,
      year_midpoint = 4.3
      ),
    infill_area_m2 = 0
  ))
  plot_growth_seed$treatments <- "Seed"
  # Combine Growth Curves
  plot_growth <- rbind(
    plot_growth_transplant,
    plot_growth_infill,
    plot_growth_seed
  )
  return(plot_growth)
}


## summarizes the simulation outputs 
summarize_simulations <- function(
  df,
  treatments = c("Seed", "Transplant", "Infill"),
  restoration_status = c("Baseline", "Restoration")
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
  #" for simulated area, methane, nitrous_oxide, biomass, and soil values for each restoration method and its baseline
  summary <- df %>%
    group_by(treatments, restoration_status, year) %>%
    summarize_if(.predicate = is.numeric,
    .funs = c(mean = mean, sd = sd), na.rm=TRUE)
  #add total mean column
  summary$total_mean_carbon <- summary$soil_carbon_total_mean + 
    summary$biomass_carbon_mean - 
    summary$methane_carbon_total_mean - 
    summary$nox_carbon_total_mean
  
  #add total SD column
  summary$total_sd_carbon <- summary$soil_carbon_total_sd + 
    summary$biomass_carbon_sd - 
    summary$methane_carbon_total_sd - 
    summary$nox_carbon_total_sd
  
  return(summary)
}