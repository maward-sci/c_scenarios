# create data table for simulations
# using reference model paramter values
# from the model_params dataframe
library(dplyr)
################################
# Define Paramters
################################
# DataFrame with Model Parameters
model_params <- data.frame(
  rbind(
    methane = list(mean = 0.03, sd = 0.005), #for baseline
    nitrous_oxide = list(mean = 0.06, sd = 0.03), #for baseline
    biomass = list(mean = 0, sd = 0), #for baseline
    soil = list(mean = 0.169, sd = 0.01) #baseline
  )
)

################################
# Define Functions
################################

# https://www.r-bloggers.com/2019/02/anatomy-of-a-logistic-growth-curve/
# https://www.tjmahr.com/anatomy-of-a-logistic-growth-curve/
# test: plot(logistic_growth(0:10))

log_growth_general <- function(years, scale = 1, asymptote = 60, midpoint = NULL, year_midpoint = 4.3){
  asymptote = asymptote # max size of meadow
  scale=scale
  if (is.null(midpoint)){
    midpoint=length(years)/(10/year_midpoint) #midpoint at 4.3 yrs
  }
  seagrass_area_ha <-asymptote / (1 + exp((midpoint - years)*scale))
  return(seagrass_area_ha)
}

#### methane as a function of area ###
# methane at time 1 = area at time 1*0.0005

#asympt at 0.2 so... 0.2/60 = 0.0033

methane_rest_fun <- function(area, year){
  meth_rest <- area*0.005
  year = 
  print(meth_rest)
  return(meth_rest)
}


create_seagrass_exp <- function(model_params){
  treatments <- c("Seed", "Transplant")
  restoration_status <- c("Baseline", "Restoration")
  year <- seq(from = 0, to = 10)
  plot_growth = cbind(year, project_size_ha = log_growth_transplant(year))
  # create full-factorial combination of the above descriptive variables
  df <- expand.grid(
    treatments=treatments,
    restoration_status=restoration_status,
    year=year)
  # create project size field
  df <- df %>% left_join(plot_growth, by=c('year'), copy=TRUE)
  # draw parameter values from the corresponding distribution
  df$methane_b <- rnorm(
    n = nrow(df),
    mean = as.numeric(model_params['methane','mean']),
    sd = as.numeric(model_params['methane','sd'])
    ) #produces a list of selected values (nrows long) from the provided distribution 
  df$nitrous_oxide_b <- rnorm(
    n = nrow(df),
    mean = as.numeric(model_params['nitrous_oxide','mean']),
    sd = as.numeric(model_params['nitrous_oxide','sd'])
  )
  # Modify some parameters based on descriptive variables
  # x <- df[which(df$restoration_status == 'Restoration'),]
  return(df)
}
