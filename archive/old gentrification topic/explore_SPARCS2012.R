.libPaths(c("/Users/Jennifer/Desktop/R packages", .libPaths()))
library(tidyverse)

setwd("/Users/Jennifer/Documents/school/NYU Wagner/16-17/Capstone")
load("SPARCS_inpatient_deID2012.rda")
colnames(df)
levels(df$APR.DRG.Description)