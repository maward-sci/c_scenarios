setwd('/Users/melissaward/Documents/Oxford Post-doc/code/c_scenarios')
source('./model/scen_generation.R') #load everything from the scen_generation.R file, (eg functions) (akin to running that file)
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
    asymptote = 60,
    midpoint = NULL,
    year_midpoint = 4.3
)
plot(infill, xlab = "Year", main = "Infill", ylim = range(0,60)) #take a look at the shape of the curve

#did it even work? how do I tel... 
plot(methane_rest_fun(1:10))

# Run functions:
df <- create_seagrass_exp(n_sim=1, methane=methane, nitrous_oxide = nitrous_oxide, biomass=biomass, soil=soil)




