library(tidyverse)
library(stringr)

temp <- tempfile()
download.file("http://www.dartmouthatlas.org/downloads/pcsa/ct_pcsav31.dbf", temp)

output <- foreign::read.dbf(temp) %>% 
  as_tibble %>% 
  mutate(county = str_sub(CT, 3, 5)) %>% 
  filter(CT_ST == "NY", county %in% c("005", "047", "061", "081", "085")) %>% 
  select(geoid = CT, pcsa = PCSA, pcsa_name = PCSA_L)


write_csv(output, "../Dropbox/capstone/tract_pcsa_xwalk.csv")
