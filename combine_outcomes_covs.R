
# Install packages if needed
package_list <- c("tidyverse", "janitor")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(stringr)

# how do I shortcut to the directory again? 
# You need to have your dropbox capstone folder be in the first level of you dropbox folder, 
# and also have your capstone github repo on the save level as your dropbox 
# then you can use the "../" to navigate up from the repo, and then into your dropbox


# Load Data Sets ----------------------------------------------------------

gent_status <- read_csv("../dropbox/capstone/data_inter/zcta_gent_xwalk.csv", col_types = "cc") %>% 
  mutate(gent_status = ordered(gent_status, levels = c("Non-Gentrifying", "Gentrifying", "Higher Income")))

census <- read_csv("../dropbox/capstone/data_inter/zcta_cov_vars.csv", col_types = cols(zcta2010 = "c"))

hospital_vars <- read_csv("../dropbox/capstone/data_inter/hospital_vars.csv", col_types = cols(zcta2010 = "c"))

health <- read_csv("../dropbox/capstone/data_inter/dart_clean.csv", 
                 col_types = cols(zcta2010 = "c", gent_status = "c")) %>% 
  select(-gent_status) %>% 
  mutate(year = if_else(year == 1999, 2000, year))

# Create changes for health variables
health_changes <- health %>%
  group_by(zcta2010) %>% 
  complete(zcta2010, nesting(year)) %>% # ensure no missing years messes up lag/lead calculations
  arrange(zcta2010, year) %>% 
  mutate_at(vars(-year, -zcta2010), funs(ch = . - lag(.))) %>% 
  set_names(., names(.) %>% str_replace("(.*)_(ch)$", "\\2_\\1")) 

# merge together census covariates and health variables
all_long <- full_join(census, health_changes, by = c("year", "zcta2010"))

# Gather: This reshapes to be super long (long by pcsa, year, variable)
# Unite: then combine the variable name and year (how we want the new columns to be named)
# Spread: reshape teh data wide so that the columns are var_year since we potentially combine 
#  variables of different types into teh single "value" column it can change the variable type, 
#  so "convert = TRUE" changes them back by guessing what they should be
# Since some variables aren't available in all years some empty columns are created
all_wide <- all_long %>%
  gather("var", "value", -zcta2010, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value, convert = TRUE) %>% 
  janitor::remove_empty_cols() %>% 
  left_join(gent_status, by = "zcta2010") %>% 
  left_join(hospital_vars, by = "zcta2010")
  

#export to dta for regressions. 
haven::write_dta(all_wide, "../dropbox/capstone/data_clean/all_data.dta")
feather::write_feather(all_wide, "../dropbox/capstone/data_clean/all_data.feather")
