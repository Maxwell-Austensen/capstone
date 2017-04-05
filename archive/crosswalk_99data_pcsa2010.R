library(tidyverse)

# Read in two zcta-level data sets for 1999 data. recode missings, apply allocation factor, and collapse by pcsa2010

xwalk <- read_csv("../Dropbox/capstone/zcta_pcsa_xwalk.csv", col_types = "ccd") %>% 
  filter(afact != 0) %>% 
  rename(pcsa2010 = pcsa)


cms_zcta <- foreign::read.dbf("../Dropbox/capstone/99-01 data/cms_zcta.dbf", as.is = TRUE) %>% janitor::clean_names()

cms_pcsa <- inner_join(cms_zcta, xwalk, by = "zcta") %>% 
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .))) %>% 
  mutate_if(is.double, funs(. * afact)) %>% 
  group_by(pcsa2010) %>% 
  summarise_if(is.double, sum, na.rm = T)

write_csv(cms_pcsa, "../Dropbox/capstone/cms99_pcsa2010.csv")



zcta1 <- foreign::read.dbf("../Dropbox/capstone/99-01 data/zcta1.dbf", as.is = TRUE) %>% janitor::clean_names()

zcta1_pcsa <- inner_join(zcta1, xwalk, by = "zcta") %>% 
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .))) %>% 
  mutate_if(is.double, funs(. * afact)) %>% 
  group_by(pcsa2010) %>% 
  summarise_if(is.double, sum, na.rm = T)

write_csv(zcta1_pcsa, "../Dropbox/capstone/zcta99_pcsa2010.csv")

