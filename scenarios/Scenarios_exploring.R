library(dplyr)
library(ggplot2)
library(nlme)
source('./model/scen_generation.R')

# setwd("~/Documents/Oxford Post-doc/c_scenarios")
scen <- read.csv("./db/Scenarios_Data.csv", header = T, na.strings=c("", "NA"),stringsAsFactors = FALSE)
scen <- scen[-c(67:999),-c(17:29)] #remove excess rows/cs
scen <- scen[,-c(17:18)] #remove excess row?


## annual gains
ggplot(scen, aes(x=Year, y=Total_GHG_benefit_MT.ha, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Total annual C gains per hectare')

ggplot(scen, aes(x=Year, y=Total_GHG.benefit_MT, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Total annual C gains in total project area')

## cumulative gains
ggplot(scen, aes(x=Year, y=Cumulative_gains, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Total cumulative C gains in total project area')


## sed
ggplot(scen, aes(x=Year, y=Soil_CSeq_MT.ha, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Sed C gains per hectare')

ggplot(scen, aes(x=Year, y=Soil_CSeq_MT, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Sed C gains in total project area')
