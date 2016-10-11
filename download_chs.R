################################################################################
# NYU Wagner
# Capstone
# October 10, 2016

# Program:   GitHub/capstone/download_chs.R
# Ouput:     ROOT/chs/
# Purpose:   Download and save NYC's Community Health Survey Public Microdata
################################################################################

# Utility functions

`%S%` <- function(x, y) {
  paste0(x, y)
}

`%notin%` <- Negate(`%in%`)

################################################################################

# Install packages if needed
package_list <- c("tidyverse", "stingr", "haven")
new_packages <- package_list[package_list %notin% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(stringr)
library(haven)

# Set directories
root_ <- "C:/Users/austensen/Downloads/"
url_ <- "https://www1.nyc.gov/assets/doh/downloads/sas/episrv/"



file_ <- "chsyyyy_public.sas7bdat"


dir.create(file.path(root_, "chs"), showWarnings = FALSE)

dir_ <- paste0(root_, "chs/")

y <- 2014
filename_ <- str_replace(file_, "yyyy", y)
filepath_ <- paste0(url_, filename_)

data <- read_sas(filepath_)
saveRDS(data, paste0(dir_, "hvs_", y, ".RDS"))



data14

View(data14)

names(data14)

str(data14)

counts <-
  data14 %>% 
  group_by(uhf34) %>% 
  summarise(n())


data13 <- read_sas(paste0(url_, file_))

uhf_counts <-
  data13 %>% 
  group_by(uhf34) %>% 
  summarise(n())


inc_counts <-
  data13 %>% 
  group_by(uhf34, IMPUTED_POV200) %>% 
  summarise(n())





