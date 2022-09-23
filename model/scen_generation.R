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
    mean_infill = 0.025, #hmm but if dredged sed, can we assume equal methane emissions (AKA no change b/c what methane is produced in veg site would have been emitted upon dredge anyway?)
    sd_infill = 0.01 #change infill to = unveg (aka bau), as in, 
    )
nitrous_oxide <- data.frame(
  mean_unvegetated = 0.06,#0.06 metric tons co2eq per ha per yr in unveg sites
  sd_unvegetated = 0.02,
  mean_vegetated = 0.5,
  sd_vegetated = 0.15,
  mean_infill = 0.06,
  sd_infill = 0.02,
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

### build the scenarios simulations ###
create_seagrass_exp <- function(
  n_sim,
  methane,
  nitrous_oxide,
  soil,
  biomass,
  treatments = c("Seed", "Transplant", "Infill"),
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
  return(df)
}

simulate_soil <- function(model_df, soil_df, depth_infill_m, depth_veg_accretion_m, depth_unveg_accretion_m){
  # pull together the three soil constituents:
    # Veg Natural soil contribution: Area * rho_soil_veg * depth_veg_accretion_m
    # UnVeg Natural soil contribution: Area * rho_soil_unveg * depth_unveg_accretion_m
    # Infill soil contribution: Area * rho_soil_unveg * depth_unveg_accretion_m * percent_remineralized
  # Carbon sequestration amounts are computed as columns in model_df with units of grams / year
  model_df$soil_carbon_vegetated <- rnorm(
    n = nrow(model_df),
    as.numeric(soil_df$mean_vegetated),
    sd = as.numeric(soil_df$sd_vegetated)) * model_df$vegetated_area_m2 * depth_veg_accretion_m
  model_df$soil_carbon_unvegetated <- rnorm(
    n = nrow(model_df),
    as.numeric(soil_df$mean_unvegetated),
    sd = as.numeric(soil_df$sd_unvegetated)) * model_df$vegetated_area_m2 * depth_unveg_accretion_m
  model_df$soil_carbon_infill <- rnorm(
    n = nrow(model_df),
    as.numeric(soil_df$mean_infill),
    sd = as.numeric(soil_df$sd_infill)) * model_df$infill_area_m2 * depth_infill_m * as.numeric(soil_df$infill_proportion_remin)
  #model_df$soil_carbon_total <- model_df$soil_carbon_vegetated + model_df$soil_carbon_infill - model_df$soil_carbon_unvegetated ##changed to veg+infill-unveg (aka Rest-BAU, rest vs baseline column now unecessary?)
  return(model_df)
}

simulate_biomass <- function(model_df, biomass_df){
  # biomass is a function of vegetated area only, because the biomass is the vegetation itself.
  model_df$biomass_carbon_vegetated <- rnorm(
    n = nrow(model_df),
    mean = as.numeric(biomass_df["mean_vegetated"]),
    sd = as.numeric(biomass_df["sd_vegetated"])
    ) * model_df$vegetated_area_m2
  model_df$biomass_carbon_unvegetated <- rnorm(
    n = nrow(model_df),
    mean = as.numeric(biomass_df["mean_unvegetated"]),
    sd = as.numeric(biomass_df["sd_unvegetated"])
  ) * model_df$vegetated_area_m2
  return(model_df)
}

simulate_nox <- function(model_df, nox_df){
  model_df$nox_carbon_unvegetated <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(nox_df["mean_unvegetated"]),
    sd = as.numeric(nox_df["sd_unvegetated"])
    ) * model_df$vegetated_area_m2
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
  #model_df$nox_carbon_total <- model_df$nox_carbon_vegetated + model_df$nox_carbon_infill - model_df$nox_carbon_unvegetated ###change to subtract unveg
  return(model_df)
}

simulate_methane <- function(model_df, methane_df){
  model_df$methane_carbon_unvegetated <- rnorm(
    n = nrow(model_df), #draw from the normal distribution nrow times
    mean = as.numeric(methane_df["mean_unvegetated"]),
    sd = as.numeric(methane_df["sd_unvegetated"])
    ) * model_df$vegetated_area_m2
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
  #model_df$methane_carbon_total <- model_df$methane_carbon_vegetated + model_df$methane_carbon_infill - model_df$methane_carbon_unvegetated #changed to subtract unveg
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

## summarize rows: rest = infill + vegetated; gain = rest-unvegetated; retain unveg columns 
compute_totals <- function(df){
  df_out <- df[,c('treatments', 'year', 'sim', 'nox_carbon_unvegetated', 'methane_carbon_unvegetated', 'soil_carbon_unvegetated', 'biomass_carbon_unvegetated')]
  df_out$nox_carbon_rest <- df$nox_carbon_vegetated + df$nox_carbon_infill
  df_out$methane_carbon_rest <- df$methane_carbon_vegetated + df$methane_carbon_infill
  df_out$soil_carbon_rest <- df$soil_carbon_vegetated + df$soil_carbon_infill
  df_out$biomass_carbon_rest <- df$biomass_carbon_vegetated
  df_out$area_sqmeters_rest <- df$vegetated_area_m2 + df$infill_area_m2
  df_out$area_sqmeters_unvegetated <- df_out$area_sqmeters_rest #add identical column for the baseline
  
  #now add additionality/gains columns
  df_out$total_nox_gains <- df_out$nox_carbon_rest - df$nox_carbon_unvegetated
  df_out$total_methane_gains <- df_out$methane_carbon_rest - df$methane_carbon_unvegetated
  df_out$total_soil_gains <- df_out$soil_carbon_rest - df$soil_carbon_unvegetated
  df_out$total_biomass_gains <- df_out$biomass_carbon_rest
  #total project additionality
  df_out$total_site_gains <- df_out$total_soil_gains + df_out$total_biomass_gains - df_out$total_methane_gains - df_out$total_nox_gains
  return(df_out)
}

## summarizes the simulation outputs 
summarize_simulations <- function(
  df,
  treatments = c("Seed", "Transplant", "Infill")
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
  #add 'restored' total column (infill + veg)
  summary <- df %>%
    group_by(treatments, year) %>%
    summarize_if(.predicate = is.numeric,
    .funs = c(mean = mean, sd = sd), na.rm=TRUE)
  return(summary)
}


### now melt summary so that we have another column with the "BaselineVsRestoration"
## Restructure db with melt##

melt_simulation_df <- function(df, id_vars){
  id_vars <- c('treatments', 'year', 'sim')
  rdf <- reshape2::melt(df, id_vars)
  rdf <- rdf %>% tidyr::separate(variable,into=c('metric', 'unit', 'restoration_status'), sep='_')
  return(rdf)
}



#(should end up w/ 66 rows)
