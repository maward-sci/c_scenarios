source('./model/scen_generation.R')
################################
# Run Routines
################################
# Test Scenario Curves

# Seeding Growth Curve:
seeding = log_growth_general(
    years = 1:10,
    scale = 1,
    asymptote = 60,
    midpoint = NULL,
    year_midpoint = 5.55
)
plot(seeding, xlab = "Year",  main = "Seed") #take a look at the shape of the curve

# Transplant Growth Curve
transplant = log_growth_general(
    years = 1:10,
    scale = 1,
    asymptote = 60,
    midpoint = NULL,
    year_midpoint = 4.3
)
plot(transplant, xlab = "Year", main = "Transplant") #take a look at the shape of the curve

# Infill Growth Curve
infill = log_growth_general(
    years = 1:10,
    scale = 1,
    asymptote = 6,
    midpoint = NULL,
    year_midpoint = 4.3
)
plot(log_growth_general, xlab = "Year", main = "Infill", ylim = range(0,8)) #take a look at the shape of the curve

#did it even work? how do I tel... 
plot(methane_rest_fun(1:10))

# Run functions:
df <- create_seagrass_exp(model_params)

### note on where I am leaving off: 

## I have the baselines for 3 scenarios (seed, transplant, and dredge) -  these are all the same
# next: generate the rest scenarios based on the growth curves. the g curves for seed and transplant are good, 
#but I need to shift the infill curve to the right. 
# add these rest value in the df at the end