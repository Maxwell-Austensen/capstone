################################################################################
# NYU Wagner
# Capstone
# October 10, 2016

# Program:   GitHub/capstone/explore_chs.R
# Ouput:     ROOT/chs/
# Purpose:   Explore NYC's Community Health Survey (CHS) Public Microdata
################################################################################

# Install packages if needed
package_list <- c("tidyverse", "stringr", "labelled", "feather")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse) # for tidy data manipulation
library(stringr) # for string manipulation
library(feather) # for saving data files



# Create Local CHS Directory ----------------------------------------------


# Load CHS 2014
data14 <- read_feather("../Dropbox/capstone/data/raw/chs/chs_2014.feather")

# Prints head of data table
data14

# View data in Viewer window (can also click data set in "objects" pane)
View(data14)

# Prints names of all columns
names(data14)

# Lists data type of each column (includes variable label when present)
# str() works for all objects (vectors, lists, etc) 
# if you have a data set glimpse() is a little nicer
str(data14)
glimpse(data14)


# The symbol %>% (pronounced "pipe" is a helpful operator that allows you 
# to reorganize the way you write code to improve readability.

# It simply takes whatever is on the left, and implicitly 
# places it as the first argument of the function call on the right.

# For example, these are equivalent:
count(data14)
data14 %>% count()

# This is helpful when you can to nest many calls to different functions.

# Without these you would have to write it nested, like the two below:

summarise(group_by(filter(data14, borough==1), uhf34), n())

summarise(
  group_by(
    filter(data14, borough==1), 
    uhf34
  ), 
  n()
)

# Or assign intermediate objects along the way (which gets messy)

filtered <- filter(data14, borough==1)
grouped <- group_by(filtered, uhf34)
summarised <- summarise(grouped, n())
summarised

# But with %>% (pipe) [Ctrl + Shift + M] you can write it like this:

data14 %>% 
  filter(borough==1) %>% 
  group_by(uhf34) %>% 
  summarise(n())

# Or write the result to a new object

mn_uhf_counts <-
  data14 %>% 
  filter(borough==1) %>% 
  group_by(uhf34) %>% 
  summarise(n())



inc_counts <-
  data14 %>% 
  group_by(borough, uhf34, imputed_pov200) %>% 
  summarise(count = n())


data14 %>% 
  ggplot(aes(borough, fill = factor(imputed_pov200))) +
  geom_bar(position = "dodge")


