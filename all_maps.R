library(tidyverse)
library(sf)

all_data <- feather::read_feather("../dropbox/capstone/data_clean/all_data.feather")

map_data <- st_read("../dropbox/capstone/shapefiles/nyc_zcta2010/nyc_zcta2010.shp", "nyc_zcta2010", 
                        stringsAsFactors = FALSE) %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  left_join(all_data, by = "zcta2010")

map_theme <- function() {
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(colour = "grey50", face = "italic", size = 8))
}

# Gentrification
map_data %>% 
  ggplot(aes(fill = gent_status)) + 
  geom_sf(color = "white", size = 0.1) +
  scale_fill_manual(values = c("#FFD200", "#B21293", "#00B2AB")) +
  map_theme() +
  labs(title = "Neighborhood Gentrification Status in New York City",
       subtitle = "ZIP Census Tabulation Areas (ZCTAs)",
       fill = NULL,
       caption = "Sources: Minnesota Population Center, NHGIS; Neighborhood Change Database")

# Primary Care Providers 2000
map_data %>% 
  ggplot(aes(fill = log(allpcp_p1000_2000 + 1))) + 
  geom_sf(color = "white", size = 0.1) +
  viridis::scale_fill_viridis() +
  # scale_fill_manual(values = c("#FFD200", "#B21293", "#00B2AB")) +
  map_theme() +
  labs(title = "Neighborhood Gentrification Status in New York City",
       subtitle = "ZIP Census Tabulation Areas (ZCTAs)",
       fill = NULL,
       caption = "Sources: Minnesota Population Center, NHGIS; Neighborhood Change Database")

# Abulatory Sensitive Conditions Discharges 2010
map_data %>% 
  ggplot(aes(fill = acscd_rt_2010)) + 
  geom_sf(color = "white", size = 0.1) +
  # viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_fill_viridis() +
  # scale_fill_manual(values = c("#FFD200", "#B21293", "#00B2AB")) +
  map_theme() +
  labs(title = "Neighborhood Gentrification Status in New York City",
       subtitle = "ZIP Census Tabulation Areas (ZCTAs)",
       fill = NULL,
       caption = "Sources: Minnesota Population Center, NHGIS; Neighborhood Change Database")


# ggsave("../dropbox/capstone/images/zcta_gent_map_med.png", width = 20, height = 20, units = "cm")