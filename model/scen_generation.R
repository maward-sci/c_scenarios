# create data table for simulations
# using reference model paramter values
# from the model_params dataframe
library(dplyr)
################################
# Define Paramters
################################
# DataFrame with Model Parameters (roughly estiamted from Oreska et al. Table 3)
model_params <- data.frame(
  rbind(
  methane = list(
    mean_unvegetated = 0.025,#0.025 met T co2eq per ha per yr in unveg sites
    sd_unvegetated = 0.01,
    mean_vegetated = 0.2,#0.2 Metirc tons CO2eq per ha per yr in veg sites
    sd_vegetated = 0.08,
    units='co2eq/ha'
    ), 
  nitrous_oxide = list(
    mean_unvegetated = 0.06,#0.06 metric tons co2eq per ha per yr in unveg sites
    sd_unvegetated = 0.02,
    mean_vegetated = 0.5,
    sd_vegetated = 0.15,
    units='co2eq/ha'
    ), 
  biomass = list(
    mean_unvegetated = 0,
    sd_unvegetated = 0,
    mean_vegetated = 0.4,
    sd_vegetated = 0.02,
    units='co2eq/ha'
    ),
  soil = list(
    mean_unvegetated = 8.125, #based very roughly on Oreska table 3 but needs work
    sd_unvegetated = 0.8,
    mean_vegetated = 20,
    sd_vegetated = 2,
    units='co2eq/ha'
    ) 
  )
)

################################
# Define Functions
################################

# https://www.r-bloggers.com/2019/02/anatomy-of-a-logistic-growth-curve/
# https://www.tjmahr.com/anatomy-of-a-logistic-growth-curve/
# test: plot(logistic_growth(0:10))

log_growth_general <- function(years, scale = 1, asymptote = 60, midpoint, year_midpoint){
  asymptote = asymptote # max size of meadow
  scale=scale
  if (is.null(midpoint)){
    midpoint = length(years)/(10/year_midpoint) #midpoint at 4.3 yrs
  }
  seagrass_area_ha <-asymptote / (1 + exp((midpoint - years)*scale))
  return(seagrass_area_ha)
}


### generic version 
carbon_per_vegetated_area <- function(area, carbon_param){
  # Scales methane emissions as a function of area in hectares for vegetated habitats
  # Parameters
  # area: list the areas in hectares to transform
  carbon_rest <- area * as.numeric(
    rnorm(
      n = length(area),
      mean = as.numeric(model_params$mean_vegetated[carbon_param]),
      sd = as.numeric(model_params$sd_vegetated[carbon_param])
    )
  )
  # print(meth_rest)
  return(carbon_rest)
}


### build the scenarios simulations ###
create_seagrass_exp <- function(model_params, n_sim){
  treatments <- c("Seed", "Transplant", "Infill")
  restoration_status <- c("Baseline", "Restoration")
  year <- seq(from = 0, to = 10)
  plot_growth_transplant = as.data.frame(cbind(
    year,
    project_size_ha = log_growth_general(
      years = year,
      scale = 1,
      asymptote = 60,
      midpoint = NULL,
      year_midpoint = 4.3
      )
  ))
  plot_growth_transplant$treatments <- "Transplant"
  plot_growth_infill = as.data.frame(cbind(
    year,
    project_size_ha = log_growth_general(
      years = year,
      scale = 1,
      asymptote = 60,
      midpoint = NULL,
      year_midpoint = 4.3
    )
  ))
  plot_growth_infill$treatments <- "Infill"
  plot_growth_seed = as.data.frame(cbind(
    year,
    project_size_ha = log_growth_general(
      years = year,
      scale = 1,
      asymptote = 60,
      midpoint = NULL,
      year_midpoint = 5.55
    )
  ))
  plot_growth_seed$treatments <- "Seed"
  # Combine Growth Curves
  plot_growth <- rbind(
    plot_growth_transplant,
    plot_growth_infill,
    plot_growth_seed
  )
  
### create full-factorial combination of the above descriptive variables
  df <- expand.grid(
    treatments=treatments,
    restoration_status=restoration_status,
    year=year,
    sim = 1:n_sim
    )
  
### create project size field
  df <- df %>% left_join(plot_growth, by=c('year', 'treatments'), copy=TRUE)
  # draw parameter values from the corresponding distribution
  df$methane <- rnorm(
    n = nrow(df),
    mean = as.numeric(model_params['methane','mean_unvegetated']),
    sd = as.numeric(model_params['methane','sd_unvegetated'])
    )
  # set methane for Restoration where it is dependent on project_size_ha
  df[which(df$restoration_status=='Restoration'), 'methane'] <- carbon_per_vegetated_area(
    area = df[which(df$restoration_status=='Restoration'), 'project_size_ha'],
    carbon_param = 'methane'
    )
  df$nitrous_oxide <- rnorm(
    n = nrow(df), #draw from the normal distribution nrow times
    mean = as.numeric(model_params['nitrous_oxide','mean_unvegetated']),
    sd = as.numeric(model_params['nitrous_oxide','sd_unvegetated'])
    )
  df[which(df$restoration_status=='Restoration'), 'nitrous_oxide'] <- carbon_per_vegetated_area(
    df[which(df$restoration_status=='Restoration'), 'project_size_ha'],
    carbon_param = 'nitrous_oxide'
  )
  df$biomass <- rnorm(
    n = nrow(df), #draw from the normal distribution nrow times
    mean = as.numeric(model_params['biomass','mean_unvegetated']),
    sd = as.numeric(model_params['biomass','sd_unvegetated'])
  )
  df[which(df$restoration_status=='Restoration'), 'biomass'] <- carbon_per_vegetated_area(
    df[which(df$restoration_status=='Restoration'), 'project_size_ha'],
    carbon_param = 'biomass'
  )
  df$soil <- rnorm(
    n = nrow(df), #draw from the normal distribution nrow times
    mean = as.numeric(model_params['soil','mean_unvegetated']),
    sd = as.numeric(model_params['soil','sd_unvegetated'])
  )
  df[which(df$restoration_status=='Restoration'), 'soil'] <- carbon_per_vegetated_area(
    df[which(df$restoration_status=='Restoration'), 'project_size_ha'],
    carbon_param = 'soil'
  )
  # set N2O for Restoration where it is dependant on project_size_ha
  # Modify some parameters based on descriptive variables
  # x <- df[which(df$restoration_status == 'Restoration'),]
  return(df)
}


## summarizes the simulation outputs 
summarize_simulations <- function(df){
  summary <- df %>%
    group_by(treatments, restoration_status, year) %>%
    summarize_if(.predicate = is.numeric,
    .funs = c(mean = mean, sd = sd), na.rm=TRUE)
  return(summary)
}
