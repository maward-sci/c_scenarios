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
    methane = list(mean = 0.03, sd = 0.005, units = 'MTCO2e q/ha'), #for baseline
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

### seeding growth curve ###
log_growth_seed<- function(years, scale = 1, asymptote = 60){
  asymptote = asymptote # max size of meadow
  scale=scale
  midpoint=length(years)/1.8 #midpoint at 5.55 years
  seagrass_area_ha <-asymptote / (1 + exp((midpoint - years)*scale))
  return(seagrass_area_ha)
}
plot(log_growth_seed(1:10), xlab = "Year",  main = "Seed") #take a look at the shape of the curve

### transplant growth curve ###
log_growth_transplant<- function(years, scale = 1, asymptote = 60, midpoint = NULL){
  asymptote = asymptote # max size of meadow
  scale=scale
  if (is.null(midpoint)){
    midpoint=length(years)/2.3 #midpoint at 4.3 yrs
  }
  seagrass_area_ha <-asymptote / (1 + exp((midpoint - years)*scale))
  return(seagrass_area_ha)
}
plot(log_growth_trans(1:10), xlab = "Year", main = "Transplant") #take a look at the shape of the curve


### Infill growth curve ### 
# -- How to move the curve to the right 2 years? (no growth until after the infill)
log_growth_infill<- function(years, scale = 1, asymptote = 6, midpoint = NULL){  #why 4?? resolve
  asymptote = asymptote # max size of meadow
  scale=scale
  if (is.null(midpoint)){
    midpoint=length(years)/2.3 #midpoint at 4.3 yrs
  }
  seagrass_area_ha <-asymptote / (1 + exp((midpoint - years)*scale))
  return(seagrass_area_ha)
}
plot(log_growth_infill(1:10), xlab = "Year", main = "Infill", ylim = range(0,8)) #take a look at the shape of the curve




#### methane as a function of area ###

#asympt at 0.2 so... 0.2/60 = 0.0033

methane_rest <- function(){}



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
  df$methane <- rnorm(
    n = nrow(df),
    mean = as.numeric(model_params['methane','mean']),
    sd = as.numeric(model_params['methane','sd'])
    ) #produces a list of selected values (nrows long) from the provided distribution 
  df$nitrous_oxide <- rnorm(
    n = nrow(df),
    mean = as.numeric(model_params['nitrous_oxide','mean']),
    sd = as.numeric(model_params['nitrous_oxide','sd'])
  )
  # Modify some parameters based on descriptive variables
  # x <- df[which(df$restoration_status == 'Restoration'),]
  return(df)
}

################################
# Run Routines
################################
# Run functions:
df <- create_seagrass_exp(model_params)




### note on where I am leaving off: 

## I have the baselines for 3 scenarios (seed, transplant, and dredge) -  these are all the same
# next: generate the rest scenarios based on the growth curves. the g curves for seed and transplant are good, 
#but I need to shift the infill curve to the right. 
# add these rest value in the df at the end
