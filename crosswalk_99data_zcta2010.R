library(tidyverse)

# Read in two zcta-level data sets for 1999 data. recode missings, apply allocation factor, and collapse by pcsa2010

xwalk <- read_csv("../Dropbox/capstone/crosswalks/zcta2000_zcta2010_xwalk.csv", col_types = "ccd")

cms_zcta2000 <- foreign::read.dbf("../Dropbox/capstone/99-01 data/cms_zcta.dbf", as.is = TRUE) %>% 
  janitor::clean_names() %>% 
  rename(zcta2000 = zcta) %>% 
  select(-zcta_l, -zfips_st, -zcta_st, -pcsa, -pcsa_l)

cms_zcta2010 <- inner_join(cms_zcta2000, xwalk, by = "zcta2000") %>% 
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .))) %>% 
  mutate_if(is.double, funs(. * afact)) %>% 
  select(-afact) %>% 
  group_by(zcta2010) %>% 
  summarise_if(is.double, sum, na.rm = T)

write_csv(cms_zcta2010, "../Dropbox/capstone/cms99_zcta2010.csv")



zcta2000_1 <- foreign::read.dbf("../Dropbox/capstone/99-01 data/zcta1.dbf", as.is = TRUE) %>% 
  janitor::clean_names() %>% 
  rename(zcta2000 = zcta) %>% 
  select(-zcta_l, -zfips_st, -zcta_st, -zctaxx, -z_lat, -z_lon, -zarea_sm, -pcsa, -pcsa_l)

zcta2010_1 <- inner_join(zcta2000_1, xwalk, by = "zcta2000") %>% 
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .))) %>% 
  mutate_if(is.double, funs(. * afact)) %>% 
  select(-afact) %>% 
  group_by(zcta2010) %>% 
  summarise_if(is.double, sum, na.rm = T)

write_csv(zcta2010_1, "../Dropbox/capstone/zcta99_zcta2010.csv")

