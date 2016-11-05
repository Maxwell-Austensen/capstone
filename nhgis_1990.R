library(tidyverse)
library(stringr)



raw_file <- "../Dropbox/capstone/data/nhgis/1990/nhgis0010_csv/nhgis0010_ds123_1990_blck_grp.csv"

bg90 <- read_csv(raw_file, 
                  col_types = cols_only(GISJOIN = "c",
                                        COUNTYA = "i",
                                        EYV001 = "i",
                                        EYY001 = "i",
                                        EYY002 = "i"))

nyc <-
  bg90 %>% 
  filter(COUNTYA %in% c(5, 47, 61, 80, 85)) %>% 
  transmute(
    geoid = str_sub(GISJOIN, 2, 3),
    agg_rent = EYV001,
    renters = EYY001 + EYY002,
    avg_rent = if_else(renters==0, NA_real_, agg_rent / renters)
  )

