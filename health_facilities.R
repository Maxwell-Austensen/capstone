# Get ZCTA-level indicator of presence of hospital(s) form NYC facilities data
# And indicator of closure of hospital between 2000 and 2010 (from news search)
# https://capitalplanning.nyc.gov/facilities/explorer

library(tidyverse)
library(stringr)

health_facilities <- read_csv("../Dropbox/capstone/data_raw/facilities_hospitals_clinics_raw.csv")

closure_tracts <- c("047050804", "061014402", "081125700", "047030700", "047015200", 
                    "061013300", "061006400", "081075702", "061003800", "081047500", 
                    "081023600", "061019800", "081097204", "081047800", "047004900")

tract_zcta_xwalk <- read_csv("../Dropbox/capstone/crosswalks/tract2010_zcta2010_xwalk.csv", col_types = "ccc")

hospital_vars <- health_facilities %>% 
  mutate(county = recode(borocode, `1` = "061", `2` = "005", `3` = "047", `4` = "081", `5` = "085"),
         tract10 = str_c("36", county, str_pad(censtract, 6, "left", "0")),
         hospital = factype == "Hospital",
         closure = tract10 %in% closure_tracts) %>% 
  select(tract10, hospital, closure) %>% 
  group_by(tract10) %>% 
  top_n(1, hospital) %>% 
  right_join(tract_zcta_xwalk, by = "tract10") %>% 
  group_by(zcta2010) %>% 
  summarise(hospital = if_else(sum(hospital, na.rm = TRUE) > 0, 1L, 0L),
            closure = if_else(sum(closure, na.rm = TRUE) > 0, 1L, 0L))


write_csv(hospital_vars, "../Dropbox/capstone/data_inter/hospital_vars.csv")
