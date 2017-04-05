library(tidyverse)
library(stringr)
library(sf)

options(scipen = 20)

# ZCTA 2000 ---------------------------------------------------------------

xwalk2000_names <- read_csv("/Users/Maxwell/Data/geocorr2k_zcta2000_nyc.csv", n_max = 1) %>% names

zcta2000_nyc_xwalk <- "/Users/Maxwell/Data/geocorr2k_zcta2000_nyc.csv" %>% 
  read_csv(skip = 2, col_names = xwalk2000_names) %>% 
  filter(placefp == "51000") %>% 
  select(zcta2000 = zcta5)

nyc_zcta2000 <- st_read("/Users/Maxwell/Data/US_zcta_2000/US_zcta10_2000.shp", "US_zcta10_2000") %>% 
  mutate(zcta2000 = as.character(ZCTA5CE00)) %>% 
  semi_join(zcta2000_nyc_xwalk, by = "zcta2000") %>% 
  select(zcta2000, geometry)


# ZCTA 2010 ---------------------------------------------------------------

xwalk2010_names <- read_csv("/Users/Maxwell/Data/geocorr2k_zcta2010_nyc.csv", n_max = 1) %>% names

zcta2010_nyc_xwalk <- "/Users/Maxwell/Data/geocorr2k_zcta2010_nyc.csv" %>% 
  read_csv(skip = 2, col_names = xwalk2010_names) %>% 
  filter(placefp == "51000") %>% 
  select(zcta2010 = zcta5)

nyc_zcta2010 <- st_read("/Users/Maxwell/Data/US_zcta_2010/US_zcta_2010.shp", "US_zcta_2010") %>% 
  mutate(zcta2010 = as.character(ZCTA5CE10)) %>% 
  semi_join(zcta2010_nyc_xwalk, by = "zcta2010") %>% 
  select(zcta2010, geometry)

st_write(nyc_zcta2010, "../Dropbox/capstone/nyc_zcta2010.shp", "nyc_zcta2010")

# Comparse ZCTA Vintages --------------------------------------------------

nyc_zcta2000 %>% 
  mutate(random_var = sample(0:30, n(), replace = TRUE) %>% as.factor) %>% 
  ggplot() + 
  geom_sf(aes(fill = random_var), color = NA) +
  geom_sf(data = nyc_zcta2010, fill = NA, color = "black") +
  theme(legend.position = "none")

ggsave("../Dropbox/capstone/zcta2000_zcta2010.png", width = 20, height = 20, units = "cm")

# Block 2000 Shapes -------------------------------------------------------

nyc_block2000 <- st_read("/Users/Maxwell/Data/NY_block_2000/NY_block_2000.shp", "NY_block_2000",
                         stringsAsFactors = FALSE) %>% 
  filter(FIPSSTCO %in% c("36005", "36047", "36061", "36081", "36085")) %>% 
  select(GISJOIN, geometry)


# Block 2000 Population ---------------------------------------------------

block_pop <- read_csv("/Users/Maxwell/Data/block2000_pop.csv") %>% 
  select(GISJOIN, block_pop = FXS001)


# ZCTA 2010 population ----------------------------------------------------

zcta2010_pop_names <- read_csv("../Dropbox/capstone/zcta2010_pop_geocorr12.csv", n_max = 1) %>% names

zcta2010_pop <- "../Dropbox/capstone/zcta2010_pop_geocorr12.csv" %>% 
  read_csv(skip = 2, col_names = zcta2010_pop_names, col_types = "ccdd") %>% 
  select(zcta2010 = zcta5, zcta2010_pop = pop10)


# Create Relationship File ------------------------------------------------

# Assign each block to the ZCTA that a plurality of its area resides

nyc_block_zcta2000 <- nyc_block2000 %>% 
  mutate(block_area = st_area(.)) %>% 
  st_intersection(nyc_zcta2000) %>% 
  mutate(area_in = st_area(.)) %>% 
  mutate(pct_in = area_in / block_area) %>% 
  group_by(GISJOIN) %>% 
  top_n(1, pct_in) %>% 
  ungroup %>% 
  as_tibble


nyc_block_zcta2010 <- nyc_block2000 %>% 
  mutate(block_area = st_area(.)) %>% 
  st_intersection(nyc_zcta2010) %>% 
  mutate(area_in = st_area(.)) %>% 
  mutate(pct_in = area_in / block_area) %>% 
  group_by(GISJOIN) %>% 
  top_n(1, pct_in) %>% 
  ungroup %>% 
  as_tibble

# Join block-zcta2000, block-zcta2010, and block population
block_zcta2000_zcta2010 <- full_join(nyc_block_zcta2000, nyc_block_zcta2010, by = "GISJOIN") %>% 
  left_join(block_pop, by = "GISJOIN") %>% 
  left_join(zcta2010_pop, by = "zcta2010") %>% 
  filter(block_pop > 0,
         zcta2010_pop >= 2000) %>% 
  select(GISJOIN, zcta2000, zcta2010, block_pop) %>% 
  drop_na()

# Calculate zcta00 -> zcta10 allocation factor
# alloc = the share of a given zcta00's population that is in a given zcta2010
zcta2000_zcta2010_xwalk <- block_zcta2000_zcta2010 %>% 
  group_by(zcta2000) %>% 
  mutate(zcta2000_pop = sum(block_pop, na.rm = TRUE)) %>% 
  group_by(zcta2000, zcta2010) %>% 
  mutate(zcta2000_zcta2010_pop = sum(block_pop, na.rm = TRUE)) %>% 
  ungroup %>% 
  distinct(zcta2000, zcta2010, zcta2000_pop, zcta2000_zcta2010_pop) %>% 
  mutate(afact = zcta2000_zcta2010_pop / zcta2000_pop) %>% 
  select(zcta2000, zcta2010, afact)

write_csv(zcta2000_zcta2010_xwalk, "../Dropbox/capstone/zcta2000_zcta2010_xwalk.csv")
 
