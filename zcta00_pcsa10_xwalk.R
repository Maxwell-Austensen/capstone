library(tidyverse)
library(stringr)

# load tract10_pcsa10 xwalk fr pcsa_names
pcsa_names <- read_csv("../Dropbox/capstone/tract_pcsa_xwalk.csv", 
                       col_types = cols_only(pcsa = "c", pcsa_name = "c")) %>% 
  group_by(pcsa) %>% 
  slice(1) %>% 
  ungroup

# Get block level population
blk_pop <- read_csv("../Dropbox/capstone/block_pop_2000.csv") %>% 
  mutate(cnty = str_sub(GISJOIN, 5, 7),
         trctblk = str_sub(GISJOIN, 9),
         geoid = str_c(cnty, trctblk)) %>% 
  select(geoid, block_pop = FXS001)

# block-zcta00-pcsa10 relationship file from spatial join
blk_zcta_pcsa <- read_csv("../Dropbox/capstone/blk_pcsa_zcta_xwalk.csv", col_types = "ccc") %>% 
  mutate(boro = str_sub(bctcb2000, 1, 1),
         county = recode(boro, `1` = "061", `2` = "005", `3` = "047", `4` = "081", `5` = "085"),
         geoid = str_c(county, str_sub(bctcb2000, 2))) %>% 
  select(geoid, zcta, pcsa) %>% 
  left_join(blk_pop, by = "geoid") %>% 
  left_join(pcsa_names, by = "pcsa") %>%
  mutate(pcsa_county = str_sub(pcsa, 3, 5)) %>% 
  filter(!is.na(zcta), !is.na(pcsa), block_pop != 0, 
         pcsa_county %in% c("005", "047", "061", "081", "085"))


zcta00_pcsa10_xwalk <- blk_zcta_pcsa %>% 
  group_by(zcta) %>% 
  mutate(zcta_pop = sum(block_pop, na.rm = TRUE)) %>% 
  group_by(zcta, pcsa, pcsa_name) %>% 
  mutate(zcta_pcsa_pop = sum(block_pop, na.rm = TRUE)) %>% 
  ungroup %>% 
  distinct(zcta, pcsa, pcsa_name, zcta_pop, zcta_pcsa_pop) %>% 
  mutate(afact = zcta_pcsa_pop / zcta_pop) %>% 
  select(zcta, pcsa, pcsa_name, afact)

write_csv(zcta00_pcsa10_xwalk, "../Dropbox/capstone/zcta00_pcsa10_xwalk.csv")
