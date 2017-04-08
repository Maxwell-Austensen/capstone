################################################################################
# NYU Wagner
# Capstone
# October 10, 2016

# Program:   GitHub/capstone/download_chs.R
# Ouput:     ROOT/
# Purpose:   Download and save NYC's Community Health Survey (CHS) 
#            Public Microdata & Documenation
################################################################################

# Install packages if needed
package_list <- c("tidyverse")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)
# devtools::install_github("tidyverse/glue")

# Load packages
library(tidyverse) # for tidy data manipulation
library(glue) # for string interpetation
library(haven) # for importing SAS/STATA/SPSS data
library(feather) # for saving data files


# Create Local CHS Directory ----------------------------------------------

dir.create("../Dropbox/capstone/documentation/chs/", showWarnings = FALSE)
  

# Download CHS Data -------------------------------------------------------

data_urls <- glue("https://www1.nyc.gov/assets/doh/downloads/sas/episrv/chs{2002:2010}_public.sas7bdat")
data_paths <- glue("../Dropbox/capstone/data_raw/chs{2002:2010}.feather")

walk2(data_urls, data_paths, ~ read_sas(.x) %>% write_feather(.y))


# Download CHS Documentation ----------------------------------------------

survey_urls <- glue("https://www1.nyc.gov/assets/doh/downloads/pdf/episrv/chs{2002:2010}survey.pdf")
survey_paths <- glue("../Dropbox/capstone/documentation/chs/chs_survey{2002:2010}.pdf")

walk2(survey_urls, survey_paths, ~ download.file(.x, .y, method = "curl", quiet = TRUE))


codebook_urls <- glue("https://www1.nyc.gov/assets/doh/downloads/pdf/episrv/chs{2002:2010}-codebook.pdf")
codebook_paths <- glue("../Dropbox/capstone/documentation/chs/chs_codebook{2002:2010}.pdf")

walk2(codebook_urls, codebook_paths, ~ download.file(.x, .y, method = "curl", quiet = TRUE))
