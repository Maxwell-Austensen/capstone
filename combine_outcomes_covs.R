
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

gent_xwalk <- read_csv("../Dropbox/capstone/pcsa_gent_xwalk.csv")

dcov <- read_csv("../Dropbox/capstone/pcsa_cov_vars.csv")

dout <- read_csv("../Dropbox/capstone/fulldf.csv", guess_max = 250) %>% 
  select(-X1, -gent_status) %>% 
  mutate(year = if_else(year == 1999L, 2000L, year))

# Create changes for health variables
dout_changes <- dout %>%
  group_by(pcsa) %>% 
  arrange(pcsa, year) %>% 
  mutate_at(vars(-year, -pcsa), funs(ch = . - lag(.))) %>% 
  set_names(., names(.) %>% str_replace("(.*)_(ch)$", "\\2_\\1")) %>% 
  janitor::remove_empty_cols()


# merge together census covariates and health variables
all_vars <- full_join(dcov, dout_changes, by = c("year", "pcsa"))

# Gather: This reshapes to be super long (long by pcsa, year, variable)
# Unite: then combine the variable name and year (how we want the new columns to be named)
# Spread: reshape teh data wide so that the columns are var_year since we potentially combine 
#  variables of different types into teh single "value" column it can change the variable type, 
#  so "convert = TRUE" changes them back by guessing what they should be
# Since some variables aren't available in all years some empty columns are created
all_vars_wide <- all_vars %>%
  gather("var", "value", -pcsa, -pcsa_name, -year) %>% 
  unite(var_year, var, year) %>% 
  spread(var_year, value, convert = TRUE) %>% 
  janitor::remove_empty_cols()
  

#export to dta for regressions. 
haven::write_dta(all_vars_wide, "../Dropbox/capstone/dall.dta")
