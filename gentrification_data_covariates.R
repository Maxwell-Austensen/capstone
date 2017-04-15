
# Install packages if needed
package_list <- c("tidyverse", "janitor")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(haven)
library(stringr)


# Load files --------------------------------------------------------------

ncdb_raw <- read_dta("J:/DEPT/REUP/Data/National/Neighborhood Change Database 2010/Clean/nyc_all_years_all_fields_long.dta")

# For decennial years the cpi adjustments are actually for the previous year.
# "Consumer Price Index for All Urban Consumers (Current Series) without seasonal adjustments from the US Bureau of Labor Statistics over all major expenditure classes for the New York City metropolitan area"
cpi <- tribble(
  ~year, ~cpi_2016_base,
  1980,    0.279839766, # Decennial data $ are really from previous year
  1990,    0.495889735,
  2000,     0.67207108,
  2010,    0.914563439 # this one really is for 2010 b/c the dolar vars are from 2006-10 ACS
)

tract_zcta_xwalk <- read_csv("../Dropbox/capstone/Crosswalks/tract2010_zcta2010_xwalk.csv", col_types = "ccd")


# Inflation Adjustment ----------------------------------------------------

select_ncdb_vars <- function(.data) {
  select(.data,
         year, geo2010, trctpop, numhhs, occhu, vacrt,
         owncr15, owncr20, rntcr15, rntcr20, r39pi, r49pi, r50pi,
         rntocc, aggrent, avhhin_n, povrat_n, povrat_d, avsocs_d, forborn,
         fem4, fem9, fem14, fem17_a, fem24, fem29,  fem34, fem44, fem54, fem64, fem74, fem75, 
         men4, men9, men14, men17_a, men24, men29, men34, men44, men54, men64, men74, men75, 
         shrhsp_n, shrnhb_n, shrnhw_n, shrnha_n, shr_d,
         educ8, educ11, educ12, educ15, educa, educ16, educpp,
         m64emp, f64emp, m64nem, f64nem)
}


ncdb2008 <- ncdb_raw %>% 
  filter(year == 2008) %>% 
  select_ncdb_vars() %>% 
  mutate(trctpop = shr_d) %>%
  select(-starts_with("shr")) %>% 
  mutate(year = 2010)

ncdb2010 <- ncdb_raw %>% 
  filter(year == 2010) %>% 
  select_ncdb_vars() %>% 
  select(year, geo2010, starts_with("shr")) %>% 
  full_join(ncdb2008, by = c("year", "geo2010"))

ncdb_adj <- ncdb_raw %>% 
  filter(year %in% c(1990, 2000)) %>% 
  select_ncdb_vars() %>% 
  bind_rows(ncdb2010) %>% 
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
    mutate(
  pop_tot = trctpop,
  hhs_tot = numhhs, #total households
  hhs_occ = occhu, # occupied households
  avg_inc_adj = avhhin_n_adj / numhhs, #average household income
  avg_rent_adj = aggrent_adj / rntocc, # average gross rent
  sh_renter = rntocc / occhu,
  sh_rent_vac = vacrt / (rntocc + vacrt), # Rental vacancy rate
  sh_sev_crowd = (owncr15 + owncr20 + rntcr15 + rntcr20) / occhu, # % renter hhs with 1.5+ ppl/room
  sh_rent_burd = (r39pi + r49pi + r50pi) / rntocc, # % renters hhs rent-burdend (>30% rent/inc)
  sh_sev_rent_burd = r50pi / rntocc, # % renters hhs severely rent-burdend (>50% rent/inc)
  sh_pov = povrat_n / povrat_d, #Poverty rate
  sh_col_ed = educ16 / educpp, # % aged 25+ college grads
  sh_hs_ed = educ12 / educpp,  # % aged 25+ High school grads (inc'l GED)
  sh_nohs_ed = (educ8 + educ11) / educpp,  # % aged 25+ not graduated High school
  sh_hisp = shrhsp_n / shr_d, 
  sh_blk = shrnhb_n / shr_d, 
  sh_wht = shrnhw_n / shr_d, 
  sh_asian = shrnha_n / shr_d, 
  sh_lt5 = age_lt5 / trctpop, 
  sh_5_17 = age_5_17 / trctpop, 
  sh_20_34 = age_20_34 / trctpop, 
  sh_35_54 = age_35_54 / trctpop, 
  sh_55p = age_55p / trctpop,
  sh_hh_ssinc = avsocs_d / numhhs, # Households with social security income last year
  sh_civ_1664_emp = civ_1664_emp / (civ_1664_emp + civ_1664_notemp), # % employed (aged 16+ civilians)
  sh_forborn = forborn / trctpop # % foreign-born
           )
}

calc_gent_vars <- function(.data) {
  .data %>% 
    mutate(p40_inc_1990 = quantile(avg_inc_adj_1990, 0.4, na.rm = TRUE),
           low_inc_1990 = avg_inc_adj_1990 <= p40_inc_1990,
           rent_chg_90_10 = (avg_rent_adj_2010 - avg_rent_adj_1990) / avg_rent_adj_1990,
           p50_rent_chg = quantile(rent_chg_90_10, 0.5, na.rm = TRUE),
           rapid_rent = rent_chg_90_10 > p50_rent_chg) %>% 
    mutate(gent_status = case_when(
      .$low_inc_1990 == FALSE ~ "Higher Income",
      .$rapid_rent == TRUE    ~ "Gentrifying",
      .$rapid_rent == FALSE   ~ "Non-Gentrifying",
      TRUE                    ~ NA_character_))
}



# Make wide files for analysis --------------------------------------------




# ZCTA 2010 ---------------------------------------------------------------

# merge in PCSA IDs, collapse by pcsa (getting sums), calculate vars at pcsa level, replace undefined with NA
zcta_vars1 <- ncdb_adj %>% 
  right_join(tract_zcta_xwalk, by = c("geoid" = "tract10")) %>% 
  mutate_at(vars(-one_of(c("year", "zcta2010", "geo2010", "geoid", "cpi_2016_base"))), funs(. * afact)) %>% 
  group_by(year, zcta2010) %>% 
  summarise_at(vars(-one_of(c("year", "zcta2010", "geo2010", "geoid", "cpi_2016_base"))), sum) %>% 
  ungroup %>% 
  calc_main_vars() %>% 
  select(year, zcta2010, matches("^(sh_|hhs_|pop_|avg_)")) %>% 
  mutate_all(funs(ifelse(is.nan(.), NA, .))) # converts NaN to NA for all variables

# reshape the above data set so wide by year, calculate changes between years for gentrification status, keep just that for a crosswalk
zcta_gent <- zcta_vars1 %>% 
  gather("var", "value", -zcta2010, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value) %>% 
  calc_gent_vars() %>% 
  select(zcta2010, gent_status)

# Calculate changes between years as simple differences ("ch2" indicates 2-year lag, ie. 1990-2010)
zcta_vars2 <- zcta_vars1 %>% 
  group_by(zcta2010) %>% 
  complete(zcta2010, nesting(year)) %>% # ensure no missing years messes up lag/lead calculations
  arrange(zcta2010, year) %>% 
  mutate_at(vars(-year, -zcta2010), funs(ch = . - lag(.))) %>% 
  mutate_at(vars(-year, -zcta2010, -matches("_ch$")), funs(ch2 = . - lag(., n = 2L))) %>% 
  set_names(., names(.) %>% str_replace("(.*)_(ch2?)$", "\\2_\\1")) %>%  # move the "ch" to the front
  janitor::remove_empty_cols()


# Tract 2010 --------------------------------------------------------------


# Apply the same steps as above to tract data, leaving out the PCSA aggregation
tract_vars1 <- ncdb_adj %>% 
  calc_main_vars() %>% 
  select(year, geoid, matches("^(sh_|hhs_|pop_|avg_)")) %>% 
  mutate_all(funs(ifelse(is.nan(.), NA, .)))

tract_gent <- tract_vars1 %>% 
  gather("var", "value", -geoid, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value) %>% 
  calc_gent_vars() %>% 
  select(geoid, gent_status)

tract_vars2 <- tract_vars1 %>% 
  group_by(geoid) %>% 
  complete(geoid, nesting(year)) %>% # ensure no missing years messes up lag/lead calculations
  arrange(geoid, year) %>% 
  mutate_at(vars(-year, -geoid), funs(ch = . - lag(.))) %>% 
  mutate_at(vars(-year, -geoid, -matches("_ch$")), funs(ch2 = . - lag(., n = 2L))) %>% 
  set_names(., names(.) %>% str_replace("(.*)_(ch2?)$", "\\2_\\1")) %>% 
  janitor::remove_empty_cols()


# Save Files --------------------------------------------------------------

write_csv(tract_gent, "../Dropbox/capstone/data_inter/tract_gent_xwalk.csv")
write_csv(zcta_gent, "../Dropbox/capstone/data_inter/zcta_gent_xwalk.csv")

write_csv(tract_vars2, "../Dropbox/capstone/data_inter/tract_cov_vars.csv")
write_csv(zcta_vars2, "../Dropbox/capstone/data_inter/zcta_cov_vars.csv")
