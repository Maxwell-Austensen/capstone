# Create Zip-UHF34 crosswalk

# Install packages if needed
package_list <- c("tidyverse", "stringr", "feather")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse) # for tidy data manipulation
library(stringr) # for string manipulation
library(feather) # for saving data files

raw_xwalk <- read_csv("../Dropbox/capstone/data/crosswalks/uhf34_zip.csv")

clean_xwalk <-
  raw %>%  
  mutate(zips = str_split(`ZIP Code`, ",")) %>% 
  unnest(zips) %>% 
  transmute(
    boro = Borough,
    uhf_34_name = `UHF Neighborhood Name (34)`,
    uhf34_code = str_replace_all(`UHF Code`, "/", ""),
    zip = as.integer(zips)
  )

write_feather(clean_xwalk, "../Dropbox/capstone/data/crosswalks/uhf34_zip.feather")
