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
package_list <- c("tidyverse", "stringr", "haven", "feather")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse) # for tidy data manipulation
library(stringr) # for string manipulation
library(haven) # for importing SAS/STATA/SPSS data
library(feather) # for saving data files


# Create Local CHS Directory ----------------------------------------------

# Create new directories in the ROOT location
raw_ <- "../Dropbox/capstone/data/raw/chs/"
docs_ <- "../Dropbox/capstone/data/documentation/chs/"
codebooks_ <- "../Dropbox/capstone/data/documentation/chs/codebooks/"
questionaires_ <- "../Dropbox/capstone/data/documentation/chs/questionaires/"

# Create vector of all new folders
folders <- c(raw_, docs_, codebooks_, questionaires_)

# Loop the vector of paths to create these folders
for(f in folders){
  dir.create(f, showWarnings = FALSE)
}


# Download CHS Data -------------------------------------------------------


# Root URL for data location
data_url <- "https://www1.nyc.gov/assets/doh/downloads/sas/episrv/"

# Name of file to download for website, with year replace with yyyy
data_name_temp <- "chsyyyy_public.sas7bdat"

# Loop over years to download each file
for(y in 2002:2014) {
  # Replace yyy with year in filename
  data_name <- str_replace(data_name_temp, "yyyy", y)
  
  # Create link to file by combining the altered name and root URL
  data_file_url <- paste0(data_url, data_name)
  
  # Download/read in SAS data file from link
  data <- read_sas(data_file_url)
  
  # Save data file in feather format, to easily read into R later
  data_path <- paste0(raw_, "chs_", y, ".feather")
  write_feather(data, data_path)
}


# Download CHS Documentation ----------------------------------------------


# Root URL for questionaire & codebook locations
docs_url <- "https://www1.nyc.gov/assets/doh/downloads/pdf/episrv/"

# Name of file to download for website, with year replace with yyyy
questionaire_name_temp <- "chsyyyysurvey.pdf"
codebook_name_temp <- "chsyyyy-codebook.pdf"

# Loop over years to download each file
for(y in 2002:2014) {
  # Replace yyyy with year in filename
  questionaire_name <- str_replace(questionaire_name_temp, "yyyy", y)
  codebook_name <- str_replace(codebook_name_temp, "yyyy", y)
  
  # Create link to file by combining the altered name and root URL
  questionaire_url <- paste0(docs_url, questionaire_name)
  codebook_url <- paste0(docs_url, codebook_name)
  
  # create local filename with path
  questionaire_path <- paste0(questionaires_, questionaire_name)
  codebook_path <- paste0(codebooks_, codebook_name)
  
  # Download/save questionaire
  download.file(questionaire_url, questionaire_path, mode = "wb", quiet = TRUE)
  download.file(codebook_url, codebook_path, mode = "wb", quiet = TRUE)
}


############################################################
############################################################
####################    END PROGRAM    #####################
############################################################
############################################################