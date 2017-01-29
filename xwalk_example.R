library(tidyverse)

df <- tribble(
  ~block, ~zcta, ~pcsa, ~block_pop,
  1,   "A",   "Y",         10,
  2,   "A",   "X",          5,
  3,   "B",   "X",          5,
  4,   "B",   "X",          5,
  5,   "A",   "Y",         10,
  6,   "A",   "X",         10,
  7,   "B",   "X",         10,
  8,   "B",   "X",          5
)

df %>% 
  group_by(zcta) %>% 
  mutate(zcta_pop = sum(block_pop)) %>% 
  group_by(zcta, pcsa) %>% 
  mutate(zcta_pcsa_pop = sum(block_pop)) %>% 
  ungroup %>% 
  distinct(zcta, pcsa, zcta_pop, zcta_pcsa_pop) %>% 
  mutate(afact = zcta_pcsa_pop / zcta_pop) %>% 
  select(zcta, pcsa, afact)

