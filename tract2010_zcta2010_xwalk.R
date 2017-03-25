library(tidyverse)
library(stringr)

xwalk_names <- read_csv("../Dropbox/capstone/tract2010_zcta2010_xwalk_geocorr12.csv", n_max = 1) %>% names

tract2010_zcta2010_xwalk <- "../Dropbox/capstone/tract2010_zcta2010_xwalk_geocorr12.csv" %>% 
  read_csv(skip = 2, col_names = xwalk_names) %>% 
  mutate(tract10 = str_c(county, str_replace(tract, "\\.", ""))) %>% 
  filter(zcta5 != 99999, afact > 0) %>% 
  select(tract10, zcta2010 = zcta5, afact)

write_csv(tract2010_zcta2010_xwalk, "../Dropbox/capstone/tract2010_zcta2010_xwalk.csv")
