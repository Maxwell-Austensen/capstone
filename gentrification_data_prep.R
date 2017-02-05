library(tidyverse)
library(haven)


ncdb_raw <- read_dta("J:/DEPT/REUP/Data/National/Neighborhood Change Database 2010/Clean/nyc_all_years_all_fields_long.dta")

cpi <- tribble(
  ~year, ~cpi_2016_base,
  1980,    0.279839766,
  1990,    0.495889735,
  2000,     0.67207108,
  2008,    0.914563439
)


ncdb_wide <- ncdb_raw %>% 
  filter(year %in% c(1990, 2000, 2008)) %>% 
  select(year, geo2010, numhhs, rntocc, aggrent, avhhin_n) %>% 
  left_join(cpi, by = "year") %>% 
  mutate_at(vars(aggrent, avhhin_n), funs("adj" = . * (1 / cpi_2016_base))) %>% 
  mutate(avg_inc_adj = avhhin_n_adj / numhhs,
         avg_rent_adj = aggrent_adj / rntocc) %>% 
  gather("var", "value", -geo2010, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value)

output <- ncdb_wide %>% 
  mutate(p40_inc_1990 = quantile(avg_inc_adj_1990, 0.4, na.rm = TRUE),
         low_inc_1990 = avg_inc_adj_1990 <= p40_inc_1990,
         rent_chg_90_10 = (avg_rent_adj_2008 - avg_rent_adj_1990) / avg_rent_adj_1990,
         p50_rent_chg = quantile(rent_chg_90_10, 0.5, na.rm = TRUE),
         rapid_rent = rent_chg_90_10 > p50_rent_chg) %>% 
  mutate(gent_status = case_when(
                          .$low_inc_1990 == FALSE ~ "High Income",
                          .$rapid_rent == TRUE    ~ "Gentrifying",
                          .$rapid_rent == FALSE   ~ "Non-Gentrifying",
                          TRUE                    ~ NA_character_))

write_csv(output, "../Dropbox/capstone/nyc_tract_gent.csv")
