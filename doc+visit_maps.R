library(tidyverse)
library(stringr)
library(rgeos) # This is neded for the fortify (i think..)
library(rgdal) # this is to read in the shapefile
library(spdplyr) # allows use of main dyplr verbs (commands) on spatial data
library(sf)

# read in PCSA shapefile
pcsas <- readOGR("/Users/Jennifer/dropbox/capstone/shapefiles/pcsav3_1shapefiles/uspcsav31_HRSA.shp", "uspcsav31_HRSA")

#nc <- st_read(system.file("shape/nc.shp", package="sf"))
#pcsas <- ("../dropbox/capstone/shapefiles/pcsav3_1shapefiles/uspcsav31_HRSA.shp", package = "sf")
#pcsas <- st_read(pcsas)

# Read in PCSA-level gentrifiction status crosswalk
map10 <- read_csv("../dropbox/capstone/new10.csv",col_types =cols_only(pcsa = "c", gent_status = "c", phys_p1000 = "i", acscd_rt = "i"))



select(pcsa, gent_status)
# create county code variable, restrict to only NYC using county, "fortify" data makes it work with ggplot, merge in gentrification status
nyc_pcsas <- pcsas %>% 
  mutate(county = str_sub(PCSA, 3, 5)) %>% 
  filter(PCSA_ST == "NY", county %in% c("005", "047", "061", "081", "085")) %>% 
  fortify(region = "PCSA") %>%
  left_join(gent_df, by = c("id" = "pcsa"))

# the x, y, and group have to take these values created by the fortify, fill is the relevant variable (here gentrification), 
gent_map <- ggplot(nyc_pcsas, aes(x= long, y = lat, group = group, fill = gent_status)) +
  geom_polygon() + # this plots the main shapefiles, using the group and fill from above
  scale_fill_manual(values = c("#FFD200", "#00B2AB", "#B21293")) + # this sets the colors for gentrification status levels
  geom_polygon(fill = NA, color = "white", size = 0.10) + # this is just to all a white outline around neighborhoods
  coord_map() + # this maps the map projection look right
  ggmap::theme_nothing(legend = TRUE) + # this just remove the basic formatting used for graphs
  labs(title = "Gentrification Status by PCSA",
       subtitle = "New York City, 2006-2010",
       fil = "",
       caption = "Source: Dartmouth Atlas of Health Care, Neighborhood Change Database") +
  theme(legend.title = element_blank(), # These options just remove legend tiles, move the legend, and make the source caption grey
        legend.position = c(.1, .7),
        plot.caption = element_text(colour = "grey50"))

# This save the ggplot object with given file type, and demensions
ggsave("gent_map_pcsa.png", gent_map, width = 6, height = 6, units = "in") 
