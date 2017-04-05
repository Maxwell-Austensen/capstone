# Get ZCTA-level indicator of presence of hospital(s) form NYC facilities data
# And indicator of closure of hospital between 2000 and 2010 (from news search)
# https://capitalplanning.nyc.gov/facilities/explorer

library(tidyverse)
library(stringr)

facilities <- read_csv("../Dropbox/capstone/data_raw/facilities_hospitals_clinics_raw.csv")

tract_zcta_xwalk <- read_csv("../Dropbox/capstone/crosswalks/tract2010_zcta2010_xwalk.csv", col_types = "ccc")

df <- facilities %>% 
  mutate(county = recode(borocode, `1` = "061", `2` = "005", `3` = "047", `4` = "081", `5` = "085"),
         tract10 = str_c("36", county, str_pad(censtract, 6, "left", "0")),
         facility = 1) %>% 
  select(tract10, facility) %>% 
  right_join(tract_zcta_xwalk, by = "tract10") %>% 
  group_by(zcta2010) %>% 
  summarise(n_facilities = sum(facility, na.rm = TRUE)) %>% 
  replace_na(list(n_facilities = 0))
  
write_csv(df, "../Dropbox/capstone/data_inter/health_facilities.csv")
