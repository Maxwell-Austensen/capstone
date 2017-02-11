library(tidyverse)
library(stringr)
library(rgeos)
library(rgdal)
library(spdplyr)

pcsas <- readOGR("../Dropbox/capstone/shapefiles/pcsav3_1shapefiles", "uspcsav31_HRSA")

gent_df <- read_csv("../Dropbox/capstone/pcsa_gent.csv", col_types = cols_only(pcsa = "c", gent_status = "c"))

nyc_pcsas <- pcsas %>% 
  mutate(county = str_sub(PCSA, 3, 5)) %>% 
  filter(PCSA_ST == "NY", county %in% c("005", "047", "061", "081", "085")) %>% 
  fortify(region = "PCSA") %>%
  left_join(gent_df, by = c("id" = "pcsa"))

gent_map <- ggplot(nyc_pcsas, aes(x= long, y = lat, group = group, fill = gent_status)) +
  geom_polygon() +
  scale_fill_manual(values = c("#FFD200", "#00B2AB", "#B21293")) +
  geom_polygon(fill = NA, color = "white", size = 0.10) +
  coord_map() +
  ggmap::theme_nothing(legend = TRUE) +
  labs(title = "Gentrification Status by PCSA",
       subtitle = "New York City, 2006-2010",
       fil = "",
       caption = "Source: Dartmouth Atlas of Health Care, Neighborhood Change Database") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .7),
        plot.caption = element_text(colour = "grey50"))

ggsave("gent_map_pcsa.png", gent_map, width = 6, height = 6, units = "in")
