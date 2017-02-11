library(tidyverse)
library(haven)


# Load files --------------------------------------------------------------

ncdb_raw <- read_dta("J:/DEPT/REUP/Data/National/Neighborhood Change Database 2010/Clean/nyc_all_years_all_fields_long.dta")

# For decennial years the cpi adjustments are actually for the previous year.
# "Consumer Price Index for All Urban Consumers (Current Series) without seasonal adjustments from the US Bureau of Labor Statistics over all major expenditure classes for the New York City metropolitan area"
cpi <- tribble(
  ~year, ~cpi_2016_base,
  1980,    0.279839766,
  1990,    0.495889735,
  2000,     0.67207108,
  2008,    0.914563439
)

tract_pcsa_xwalk <- read_csv("../Dropbox/capstone/tract_pcsa_xwalk.csv", col_types = "ccc")


# Inflation Adjustment ----------------------------------------------------

ncdb_adj <- ncdb_raw %>% 
  filter(year %in% c(1990, 2000, 2008)) %>% 
  select(year, geo2010, numhhs, rntocc, aggrent, avhhin_n) %>% 
  left_join(cpi, by = "year") %>% 
  mutate(geoid = as.character(geo2010)) %>% 
  mutate_at(vars(aggrent, avhhin_n), funs("adj" = . * (1 / cpi_2016_base)))


# Make wide Tract and PCSa files ------------------------------------------

pcsa_wide <- ncdb_adj %>% 
  right_join(tract_pcsa_xwalk, by = "geoid") %>% 
  group_by(year, pcsa, pcsa_name) %>% 
  summarise_at(vars(avhhin_n_adj, numhhs, aggrent_adj, rntocc), sum) %>% 
  ungroup %>% 
  mutate(avg_inc_adj = avhhin_n_adj / numhhs,
         avg_rent_adj = aggrent_adj / rntocc) %>% 
  gather("var", "value", -pcsa, -pcsa_name, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value)
  
tract_wide <- ncdb_adj %>% 
  mutate(avg_inc_adj = avhhin_n_adj / numhhs,
         avg_rent_adj = aggrent_adj / rntocc) %>% 
  gather("var", "value", -geoid, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value)



# Create Gentrification indictor ------------------------------------------

get_gent_vars <- function(.data) {
  .data %>% 
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
}


tract_output <- get_gent_vars(tract_wide)

pcsa_output <- get_gent_vars(pcsa_wide)


write_csv(tract_output, "../Dropbox/capstone/tract_gent.csv")
write_csv(pcsa_output, "../Dropbox/capstone/pcsa_gent.csv")
