# Create Zip-UHF34 crosswalk

# zcta_uhf34_xwalk_raw.csv was created by hand from this pdf:
# https://a816-healthpsi.nyc.gov/epiquery/CHS/uhf-zip-information.pdf

# Install packages if needed
package_list <- c("tidyverse")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse) # for tidy data manipulation


# -------------------------------------------------------------------------


zcta_uhf34_xwalk <- read_csv("../dropbox/capstone/crosswalks/zcta_uhf34_xwalk_raw.csv", col_types = "cccc") %>% 
  mutate(zcta = stringr::str_split(zcta, ",")) %>% 
  unnest(zcta)

feather::write_feather(zcta_uhf34_xwalk, "../Dropbox/capstone/crosswalks/zcta_uhf34_xwalk.feather")


# The CHS data uses a 1:34 number for uhf34, so need extra crosswalk
chs_uhf34_codes <- read_csv("../dropbox/capstone/crosswalks/chs_uhf34_codes.csv", 
                            col_types = cols_only(chs_uhf34 = "i", uhf34 = "c"))

# the "totpop" vars come from the dartmouth data, which uses decennial 2010 - here we want the acs 06-10
# there are 9 zctas in the xwalk that aren't in our sample (these are ones dropped for pop < 2000)
uhf_gent_status <- feather::read_feather("../dropbox/capstone/data_clean/all_data.feather") %>% 
  select(zcta2010, pop_tot_2010, gent_status) %>% 
  inner_join(zcta_uhf34_xwalk, by = c("zcta2010" = "zcta")) %>% 
  group_by(uhf34, uhf34_name, gent_status) %>% 
  summarise(gent_pop = sum(pop_tot_2010)) %>% 
  group_by(uhf34, uhf34_name) %>% 
  mutate(uhf34_pop = sum(gent_pop)) %>% 
  ungroup() %>% 
  mutate(gent_pop_pct = gent_pop / uhf34_pop,
         gent_short = recode(gent_status, "High Income" = "hiinc", 
                                          "Non-Gentrifying" = "nongent", 
                                          "Gentrifying" = "gent")) %>% 
  select(uhf34, uhf34_name, gent_short, gent_pop_pct) %>% 
  spread(gent_short, gent_pop_pct, fill = 0) %>% 
  left_join(chs_uhf34_codes, by = "uhf34")


feather::write_feather(uhf_gent_status, "../Dropbox/capstone/data_inter/uhf34_gent_status.feather")

  
