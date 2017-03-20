# Get PCSA-level count of hospitals and clincs form NYC facilities data
# https://capitalplanning.nyc.gov/facilities/explorer

library(tidyverse)
library(stringr)

facilities <- read_csv("../Dropbox/capstone/facilities_hospitals_clinics_raw.csv")

tract_pcsa_xwalk <- read_csv("../Dropbox/capstone/tract_pcsa_xwalk.csv", col_types = "ccc")

df <- facilities %>% 
  mutate(county = recode(borocode, `1` = "061", `2` = "005", `3` = "047", `4` = "081", `5` = "085"),
         geoid = str_c("36", county, str_pad(censtract, 6, "left", "0")),
         facility = 1) %>% 
  select(geoid, facility) %>% 
  right_join(tract_pcsa_xwalk, by = "geoid") %>% 
  group_by(pcsa, pcsa_name) %>% 
  summarise(n_facilities = sum(facility, na.rm = TRUE)) %>% 
  replace_na(list(n_facilities = 0))
  
write_csv(df, "../Dropbox/capstone/health_facilities.csv")
