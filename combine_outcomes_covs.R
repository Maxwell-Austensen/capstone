
library(tidyverse)
library(haven)
library(stringr)
library(feather)
library(foreign)

# how do I shortcut to the directory again? 
dcov <- read_csv("/Users/amy/Dropbox/1. NYU Wagner/Fall 2016/capstone1/capstone/pcsa_gent.csv")
dout <- read.csv("/Users/amy/Dropbox/1. NYU Wagner/Fall 2016/capstone1/capstone/fulldf.csv")


# notes for Amy on how to summarize data
# names(dcov)  
# glimpse(dataframe) 
# summary(dataframe)

summary(dout$year)
glimpse(dout)

#change years from 1999 to 2000. 
dout <- dout %>% 
  mutate(year = if_else(year == 1999L, 2000L, year))

# reshape covariates so years go long
dcov2 <- dcov %>% 
  gather("var_year", "value", -pcsa, -pcsa_name) %>% 
  mutate(year = str_sub(var_year, -4),
         var = str_sub(var_year, 1, -4)) %>% 
  select(-var_year) %>% 
  spread(var, value) %>% 
  filter(year %in% c("1990", "2000", "2010", "2008"))
  select(year, everything())

# merge covariates with outcomes.
  dfull <- dout %>%         
  left_join(dcov2, by = "pcsa")

#export to dta for regressions. 
write.dta(dfull, "/Users/amy/Dropbox/1. NYU Wagner/Fall 2016/capstone1/capstone/dall.dta")