
# Install packages if needed
package_list <- c("tidyverse", "stringr", "haven", "feather")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse) # for tidy data manipulation
library(stringr) # for string manipulation
library(haven) # for importing SAS/STATA/SPSS data
library(feather) # for saving data fileslibrary(tidyverse)


# Load Files & Create Crosswalks ------------------------------------------

# Tract-level census data
# This if from Furman Center Data for now - later I can create all the files
tract_file <- "J:/DEPT/REUP/SOC/SOC 2015/Report/Focus/Data/NCDB/ncdb_acs_tract_adj.sas7bdat"

raw_tract <- read_sas(tract_file)

# Load and prep Tract-ZCTA crosswalk
tract_zip_file <- "../Dropbox/capstone/data/crosswalks/tract10_zcta10.csv"

col_names <- read_csv(tract_zip_file, n_max = 0) %>% names()

xwalk <- 
  read_csv(tract_zip_file, col_names = col_names, skip = 2) %>% 
  transmute(
    geoid = str_c(county, str_replace(tract, "\\.", "")),
    zcta = zcta5,
    afact = afact
  )

# Load UHF34-ZCTA crosswalk
uhf34_zip_xwalk <- read_feather("../Dropbox/capstone/data/crosswalks/uhf34_zip.feather")



# Prep ZCTA-level data ----------------------------------------------------


long_zip <-
  raw_tract %>%
  filter(year %in% c("1990", "1014")) %>% 
  mutate(geoid = str_c("36", county, tract)) %>% 
  select(year, geoid, rentocc, tot_units, agg_rent, agg_hhinc) %>% 
  left_join(xwalk, by = "geoid") %>% 
  mutate(
    agg_rent_zip = agg_rent*afact,
    agg_hhinc_zip = agg_hhinc*afact,
    tot_units_zip = tot_units*afact,
    rent_units_zip = rentocc*afact
  ) %>% 
  group_by(year, zcta) %>% 
  summarize(
    agg_rent = sum(agg_rent_zip, na.rm = TRUE),
    agg_hhinc = sum(agg_hhinc_zip, na.rm = TRUE),
    tot_units = sum(tot_units_zip, na.rm = TRUE),
    rent_units = sum(rent_units_zip, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    avg_rent = agg_rent/rent_units,
    avg_hhinc = agg_hhinc/tot_units
  )


wide_zip <-
  long_zip %>%
  filter(year == "1014") %>% 
  rename(
    agg_rent_1014 = agg_rent, avg_rent_1014 = avg_rent, 
    agg_hhinc_1014 = agg_hhinc, avg_hhinc_1014 = avg_hhinc,
    tot_units_1014 = tot_units, rent_units_1014 = rent_units
  ) %>% 
  left_join(filter(long_zip, year == "1990"), by = "zcta") %>% 
  rename(
    agg_rent_1990 = agg_rent, avg_rent_1990 = avg_rent, 
    agg_hhinc_1990 = agg_hhinc, avg_hhinc_1990 = avg_hhinc,
    tot_units_1990 = tot_units, rent_units_1990 = rent_units
  )


# Create ZCTA and UHF34 Gentrification Status -----------------------------

gent_zip <-
  wide_zip %>% 
  filter(!is.na(avg_rent_1990), !is.na(avg_rent_1014)) %>% 
  mutate(
    hhinc_1990_40 = quantile(avg_hhinc_1990, .40),
    rent_chg = (avg_rent_1014 - avg_rent_1990) / avg_rent_1990,
    rent_chg_50 = quantile(rent_chg, .50),
    inc_status = if_else(avg_hhinc_1014 <= hhinc_1990_40, "Lower Income", "Higher Income"),
    gent_status = if_else(inc_status=="Higher Income", "Higher Income",
                          if_else(rent_chg > rent_chg_50, "Gentrifying", "Non-Gentrifying"))
  ) %>% 
  select(zcta, gent_status, inc_status, rent_chg, starts_with("avg"))


gent_uhf34 <-
  wide_zip %>% 
  filter(!is.na(avg_rent_1990), !is.na(avg_rent_1014)) %>% 
  left_join(uhf34_zip_xwalk, by = c("zcta" = "zip")) %>% 
  group_by(uhf34_code) %>% 
  summarize(
    agg_rent_1990 = sum(agg_rent_1990, na.rm = TRUE),
    agg_hhinc_1990 = sum(agg_hhinc_1990, na.rm = TRUE),
    agg_rent_1014 = sum(agg_rent_1014, na.rm = TRUE),
    agg_hhinc_1014 = sum(agg_hhinc_1014, na.rm = TRUE),
    tot_units_1990 = sum(tot_units_1990, na.rm = TRUE),
    rent_units_1990 = sum(rent_units_1990, na.rm = TRUE),
    tot_units_1014 = sum(tot_units_1014, na.rm = TRUE),
    rent_units_1014 = sum(rent_units_1014, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    avg_rent_1990 = agg_rent_1990/rent_units_1990,
    avg_hhinc_1990 = agg_hhinc_1990/tot_units_1990,
    avg_rent_1014 = agg_rent_1014/rent_units_1014,
    avg_hhinc_1014 = agg_hhinc_1014/tot_units_1014
  ) %>% 
  mutate(
    hhinc_1990_40 = quantile(avg_hhinc_1990, .40),
    rent_chg = (avg_rent_1014 - avg_rent_1990) / avg_rent_1990,
    rent_chg_50 = quantile(rent_chg, .50),
    inc_status = if_else(avg_hhinc_1014 <= hhinc_1990_40, "Lower Income", "Higher Income"),
    gent_status = if_else(inc_status=="Higher Income", "Higher Income",
                          if_else(rent_chg > rent_chg_50, "Gentrifying", "Non-Gentrifying"))
  ) %>% 
  select(uhf34_code, gent_status, inc_status, rent_chg, starts_with("avg"))
    


# Save Files --------------------------------------------------------------

write_feather(gent_zip, "../Dropbox/capstone/data/clean/gentrification/gent_zip.feather")
write_feather(gent_uhf34, "../Dropbox/capstone/data/clean/gentrification/gent_uhf34.feather")
