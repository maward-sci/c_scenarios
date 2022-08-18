library(dplyr)
library(ggplot2)
library(nlme)

setwd("~/Documents/Oxford Post-doc")
scen <- read.csv("Scenarios Data.csv", header = T, na.strings=c("", "NA"),stringsAsFactors = FALSE)
scen <- scen[-c(67:999),-c(17:29)] #remove excess rows/cs
scen <- scen[,-c(17:18)] #remove excess row?


## 
ggplot(scen, aes(x=Year, y=Total_GHG_benefit_MT.ha, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Total annual C gains per hectare')

ggplot(scen, aes(x=Year, y=Total_GHG.benefit_MT, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Total annual C gains per project area')

## sed
ggplot(scen, aes(x=Year, y=Soil_CSeq_MT.ha, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Sed C gains in total project area')

ggplot(scen, aes(x=Year, y=Soil_CSeq_MT, color = BaselineVsRest, shape = Treatment)) + geom_point(size = 3) + geom_line() + 
  theme_classic() + ggtitle('Sed C gains in total project area')
