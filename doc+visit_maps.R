library(tidyverse)
library(stringr)
library(rgeos) # This is neded for the fortify (i think..)
library(rgdal) # this is to read in the shapefile
library(spdplyr) # allows use of main dyplr verbs (commands) on spatial data
#library(sf)
library(viridis)

# NOTE: temporarily dropping one pcsa in provider map that is outlier messing up scales
# After midterm report/presentaiton we should probably look at this one anyway
# Though if we switch to zcta these sorts of issues would likely be resolved

# read in PCSA shapefile
pcsas <- readOGR("../dropbox/capstone/shapefiles/pcsav3_1shapefiles/uspcsav31_HRSA.shp", "uspcsav31_HRSA")

## THIS DIDN'T WORK FOR ME
# EXAMPLE: nc <- st_read(system.file("shape/nc.shp", package="sf"))
#pcsas <- st_read("../dropbox/capstone/shapefiles/pcsav3_1shapefiles/uspcsav31_HRSA.shp", package = "sf")


# Read in PCSA-level gentrifiction status crosswalk
map10 <- read_csv("../dropbox/capstone/map10.csv",col_types =cols_only(pcsa = "c", gent_status = "c", allpcp_p1000 = "n", acscd_rt = "n"))
map99 <- read_csv("../dropbox/capstone/map99.csv",col_types =cols_only(pcsa = "c", gent_status = "c", allpcp_p1000 = "n"))

summary(map10$allpcp_p1000)
boxplot(map10$allpcp_p1000)
hist(map10$allpcp_p1000)
count(subset)

pcpmap <- map10 %>% select(pcsa, allpcp_p1000)
pcpmap99 <- map99 %>% select(pcsa, allpcp_p1000)
acsmap <- map10 %>% select(pcsa, acscd_rt)

# create county code variable, restrict to only NYC using county, "fortify" data makes it work with ggplot, merge in gentrification status
pcpmap <- pcsas %>% 
  mutate(county = str_sub(PCSA, 3, 5)) %>% 
  filter(PCSA_ST == "NY", county %in% c("005", "047", "061", "081", "085")) %>% 
  fortify(region = "PCSA") %>%
  left_join(pcpmap, by = c("id" = "pcsa"))

pcpmap99 <- pcsas %>% 
  mutate(county = str_sub(PCSA, 3, 5)) %>% 
  filter(PCSA_ST == "NY", county %in% c("005", "047", "061", "081", "085")) %>% 
  fortify(region = "PCSA") %>%
  left_join(pcpmap99, by = c("id" = "pcsa"))

acsmap <- pcsas %>% 
  mutate(county = str_sub(PCSA, 3, 5)) %>% 
  filter(PCSA_ST == "NY", county %in% c("005", "047", "061", "081", "085")) %>% 
  fortify(region = "PCSA") %>%
  left_join(acsmap, by = c("id" = "pcsa"))

## 2010 ALL PCP MAP ##
# the x, y, and group have to take these values created by the fortify, fill is the relevant variable (here gentrification), 
pcp_map <- ggplot(pcpmap, aes(x= long, y = lat, group = group, fill = allpcp_p1000)) +
  geom_polygon() + # this plots the main shapefiles, using the group and fill from above
  geom_polygon(fill = NA, color = "white", size = 0.10) + # this is just to all a white outline around neighborhoods
  coord_map() + # this maps the map projection look right
  scale_fill_viridis(option = "magma", limits = c(0, 5)) +
  theme_void() + #removes formatting for graphs, makes background white
  labs(title = "All Primary Care Providers per 1,000 Population by PCSA",
       subtitle = "New York City, 2010",
       fil = "",
       caption = "Source: Dartmouth Atlas of Health Care, Primary Care Service Area Project") +
  theme(legend.title = element_blank(), # These options just remove legend tiles, move the legend, and make the source caption grey
        legend.position = c(.1, .7),
        plot.caption = element_text(colour = "grey50"))

# This save the ggplot object with given file type, and demensions
ggsave("pcp1000_map_pcsa.png", pcp_map, width = 6, height = 6, units = "in") 

## 1999 (2000) ALL PCP MAP ##
pcp_map99 <- ggplot(pcpmap99, aes(x= long, y = lat, group = group, fill = allpcp_p1000)) +
  geom_polygon() + 
  geom_polygon(fill = NA, color = "white", size = 0.10) +
  coord_map() +
  scale_fill_viridis(option = "magma", limits = c(0, 5)) +
  theme_void() +
  labs(title = "All Primary Care Providers per 1,000 Population by PCSA",
       subtitle = "New York City, 2000",
       fil = "",
       caption = "Source: Dartmouth Atlas of Health Care, Primary Care Service Area Project") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .7),
        plot.caption = element_text(colour = "grey50"))

ggsave("pcp1000_map99_pcsa.png", pcp_map99, width = 6, height = 6, units = "in") 

## 2010 ACS MAP ##
acs_map <- ggplot(acsmap, aes(x= long, y = lat, group = group, fill = acscd_rt)) +
  geom_polygon() + 
  geom_polygon(fill = NA, color = "white", size = 0.10) +
  coord_map() +
  scale_fill_viridis(option = "magma") +
  theme_void() +
  labs(title = "Ambulatory Sensitive Care Condition Discharges \nper 1,000 Medicare Beneficiaries by PCSA",
       subtitle = "New York City, 2010",
       fil = "",
       caption = "Source: Dartmouth Atlas of Health Care, Primary Care Service Area Project") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .7),
        plot.caption = element_text(colour = "grey50"))

ggsave("acs10_map_pcsa.png", acs_map, width = 6, height = 6, units = "in") 

