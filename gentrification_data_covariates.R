library(tidyverse)
library(haven)
library(stringr)


# Load files --------------------------------------------------------------

ncdb_raw <- read_dta("J:/DEPT/REUP/Data/National/Neighborhood Change Database 2010/Clean/nyc_all_years_all_fields_long.dta")
ncdb_raw %>% names %>% tolower

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
  filter(year %in% c(1990, 2000, 2008, 2010)) %>% 
  select(year, geo2010, trctpop, numhhs, 
         rntocc, aggrent, avhhin_n, povrat_n, povrat_d, avsocs_d, forborn,
         fem4, fem9, fem14, fem17_a, fem24, fem29,  fem34, fem44, fem54, fem64, fem74, fem75, 
         men4, men9, men14, men17_a, men24, men29, men34, men44, men54, men64, men74, men75, 
         shrhsp_n, shrnhb_n, shrnhw_n, shrnha_n, shr_d,
         educ8, educ11, educ12, educ15, educa, educ16, educpp,
         m64emp, f64emp, m64nem, f64nem) %>% 
  left_join(cpi, by = "year") %>% 
  mutate(geoid = as.character(geo2010)) %>% 
  mutate_at(vars(aggrent, avhhin_n), funs("adj" = . * (1 / cpi_2016_base))) %>%
  mutate(age_lt5 = fem4 + men4, 
         age_5_17 = fem9 + fem14 + fem17_a + men9 + men14 + men17_a,
         age_20_34 = fem24 + fem29 + fem34 + men24 + men29 + men34,
         age_35_54 = fem44 + fem54 + men44 + men54, 
         age_55p = fem64 + fem74 + fem75 + men64 + men74 + men75,
         civ_1664_emp = m64emp + f64emp,
         civ_1664_notemp = m64nem + f64nem) %>% 
  select(-matches("^(fem|men)\\d*"),-matches("^(f|m)64"), -aggrent, -avhhin_n)
 


# Functions to generage indicators ----------------------------------------

calc_main_vars <- function(.data) {
  .data %>% 
    mutate(avg_inc_adj = avhhin_n_adj / numhhs,
           avg_rent_adj = aggrent_adj / rntocc, 
           sh_pov = povrat_n / povrat_d, 
           sh_col_ed = educ16 / educpp, 
           sh_hs_ed = educ12 / educpp, 
           sh_nohs_ed = (educ8 + educ11) / educpp, 
           sh_hisp = shrhsp_n / shr_d, 
           sh_blk = shrnhb_n / shr_d, 
           sh_wht = shrnhw_n / shr_d, 
           sh_asian = shrnha_n / shr_d, 
           sh_lt5 = age_lt5 / trctpop, 
           sh_5_17 = age_5_17 / trctpop, 
           sh_20_34 = age_20_34 / trctpop, 
           sh_35_54 = age_35_54 / trctpop, 
           sh_55p = age_55p / trctpop,
           sh_hh_ssinc = avsocs_d / numhhs,
           sh_civ_1664_emp = civ_1664_emp / (civ_1664_emp + civ_1664_notemp),
           sh_forborn = forborn / trctpop)
}

calc_gent_vars <- function(.data) {
  .data %>% 
    mutate(p40_inc_1990 = quantile(avg_inc_adj_1990, 0.4, na.rm = TRUE),
           low_inc_1990 = avg_inc_adj_1990 <= p40_inc_1990,
           rent_chg_90_10 = (avg_rent_adj_2008 - avg_rent_adj_1990) / avg_rent_adj_1990,
           p50_rent_chg = quantile(rent_chg_90_10, 0.5, na.rm = TRUE),
           rapid_rent = rent_chg_90_10 > p50_rent_chg, 
           pop_chg_90_00 = (trctpop_2000 - trctpop_1990)/trctpop_1990, 
           pop_chg_00_10 = (trctpop_2010 - trctpop_2000)/trctpop_2000) %>% 
    mutate(gent_status = case_when(
      .$low_inc_1990 == FALSE ~ "High Income",
      .$rapid_rent == TRUE    ~ "Gentrifying",
      .$rapid_rent == FALSE   ~ "Non-Gentrifying",
      TRUE                    ~ NA_character_))
}


# Make wide Tract and PCSa files ------------------------------------------

pcsa_output <- ncdb_adj %>% 
  right_join(tract_pcsa_xwalk, by = "geoid") %>% 
  group_by(year, pcsa, pcsa_name) %>% 
  summarise_at(vars(-one_of(c("year", "geo2010", "geoid", "cpi_2016_base"))), sum) %>% 
  ungroup %>% 
  calc_vars() %>% 
  gather("var", "value", -pcsa, -pcsa_name, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value) %>% 
  calc_gent_vars()

#do same for tract level data? 
tract_output <- ncdb_adj %>% 
  calc_vars() %>% 
  gather("var", "value", -geoid, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value) %>% 
  calc_gent_vars()


# Save Files --------------------------------------------------------------

write_csv(tract_output, "../Dropbox/capstone/tract_gent.csv")
write_csv(pcsa_output, "../Dropbox/capstone/pcsa_gent.csv")
