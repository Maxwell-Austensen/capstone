library(tidyverse)
library(stringr)


# ZCTA 2010 population ----------------------------------------------------

zcta2010_pop_names <- read_csv("../Dropbox/capstone/crosswalks/zcta2010_pop_geocorr12.csv", n_max = 1) %>% names

zcta2010_pop <- "../Dropbox/capstone/crosswalks/zcta2010_pop_geocorr12.csv" %>% 
  read_csv(skip = 2, col_names = zcta2010_pop_names, col_types = "ccdd") %>% 
  select(zcta2010 = zcta5, zcta2010_pop = pop10) %>% 
  filter(zcta2010_pop >= 2000)


xwalk_names <- read_csv("../Dropbox/capstone/crosswalks/tract2010_zcta2010_xwalk_geocorr12.csv", n_max = 1) %>% names

tract2010_zcta2010_xwalk <- "../Dropbox/capstone/crosswalks/tract2010_zcta2010_xwalk_geocorr12.csv" %>% 
  read_csv(skip = 2, col_names = xwalk_names, col_types = "cccdd") %>% 
  mutate(tract10 = str_c(county, str_replace(tract, "\\.", ""))) %>% 
  filter(zcta5 != 99999, afact > 0) %>% 
  select(tract10, zcta2010 = zcta5, afact) %>% 
  semi_join(zcta2010_pop, by = "zcta2010")

write_csv(tract2010_zcta2010_xwalk, "../Dropbox/capstone/crosswalks/tract2010_zcta2010_xwalk.csv")
