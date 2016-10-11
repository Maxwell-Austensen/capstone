################################################################################
# NYU Wagner
# Capstone
# October 10, 2016

# Program:   GitHub/capstone/download_chs.R
# Ouput:     ROOT/
# Purpose:   Download and save NYC's Community Health Survey (CHS) 
#            Public Microdata & Documenation
################################################################################

# Utility functions

`%S%` <- function(x, y) {
  paste0(x, y)
}

`%notin%` <- Negate(`%in%`)

################################################################################

# Install packages if needed
package_list <- c("tidyverse", "stringr", "haven")
new_packages <- package_list[package_list %notin% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(stringr)
library(haven)



# Create Local CHS Directory ----------------------------------------------


# Set directories
root_ <- "C:/Users/austensen/Downloads/"


# Create new directories in the ROOT location
chs_ <- paste0(root_, "chs/")
data_ <- paste0(root_, "chs/data/")
raw_ <- paste0(root_, "chs/data/raw/")
clean_ <- paste0(root_, "chs/data/clean/")
output_ <- paste0(root_, "chs/data/output/")
docs_ <- paste0(root_, "chs/documentation/")
codebooks_ <- paste0(root_, "chs/documentation/codebooks/")
questionaires_ <- paste0(root_, "chs/documentation/questionaires/")

# Create vector of all new folders
folders <- c(chs_, data_, raw_, clean_, output_, docs_, codebooks_, questionaires_)

# Loop the vector of paths to create these folders
for(f in folders){
  dir.create(f, showWarnings = FALSE)
}


# Download CHS Data -------------------------------------------------------


# Root URL for data location
data_url_ <- "https://www1.nyc.gov/assets/doh/downloads/sas/episrv/"

# Name of file to download for website, with year replace with yyyy
data_name_temp <- "chsyyyy_public.sas7bdat"

# Loop over years to download each file
for(y in 2002:2014) {
  # Replace yyy with year in filename
  data_name <- str_replace(data_name_temp, "yyyy", y)
  
  # Create link to file by combining the altered name and root URL
  data_file_url_ <- paste0(data_url_, data_name)
  
  # Download/read in SAS data file from link
  data <- read_sas(data_file_url_)
  
  # Save data file in RDS format, to easily read into R later
  data_path_ <- paste0(raw_, "chs_", y, ".RDS")
  saveRDS(data, data_path_)
}


# Download CHS Documentation ----------------------------------------------


# Root URL for questionaire & codebook locations
docs_url_ <- "https://www1.nyc.gov/assets/doh/downloads/pdf/episrv/"

# Name of file to download for website, with year replace with yyyy
questionaire_name_temp <- "chsyyyysurvey.pdf"
codebook_name_temp <- "chsyyyy-codebook.pdf"

# Loop over years to download each file
for(y in 2002:2014) {
  # Replace yyyy with year in filename
  questionaire_name <- str_replace(questionaire_name_temp, "yyyy", y)
  codebook_name <- str_replace(codebook_name_temp, "yyyy", y)
  
  # Create link to file by combining the altered name and root URL
  questionaire_url_ <- paste0(docs_url_, questionaire_name)
  codebook_url_ <- paste0(docs_url_, codebook_name)
  
  # create local filename with path
  questionaire_path_ <- paste0(questionaires_, questionaire_name)
  codebook_path_ <- paste0(codebooks_, codebook_name)
  
  # Download/save questionaire
  download.file(questionaire_url_, questionaire_path_, mode = "wb", quiet = TRUE)
  download.file(codebook_url_, codebook_path_, mode = "wb", quiet = TRUE)
}


############################################################
############################################################
####################    END PROGRAM    #####################
############################################################
############################################################