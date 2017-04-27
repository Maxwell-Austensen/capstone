library(tidyverse)
library(feather)
library(sf)


# Load Data and Shapes ----------------------------------------------------

# Prepare ZCTA-level data for maps
all_data <- feather::read_feather("../dropbox/capstone/data_clean/all_data.feather")

map_data <- st_read("../dropbox/capstone/shapefiles/nyc_zcta2010/nyc_zcta2010.shp", "nyc_zcta2010", 
                        stringsAsFactors = FALSE) %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  left_join(all_data, by = "zcta2010")

# Prepare CHS UHF34-level data for map
uhf34_xwalk <- read_feather("../Dropbox/capstone/data_inter/uhf34_gent_status.feather") %>% 
  transmute(uhf34_name = uhf34_name,
            UHF34_CODE = stringr::str_replace_all(uhf34, "/", "") %>% as.numeric())

chs_vars <- feather::read_feather("../dropbox/capstone/data_clean/chs_uhf34_0309_all.feather") %>% 
  left_join(uhf34_xwalk, by = "uhf34_name")

chs_map_data <- st_read("../dropbox/capstone/shapefiles/CHS_2009_DOHMH_2010B/CHS_2009_DOHMH_2010B.shp",
                  stringsAsFactors = FALSE) %>% 
  inner_join(chs_vars, by = "UHF34_CODE")

# Get borough shapes for basemap (grey in areas with no zcta shapes)
boros <- st_read("http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nybb/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson")

# Get shapes for outline of gent and non-gent areas
gent_status_shapes <- map_data %>% 
  group_by(gent_status) %>% 
  summarise(geometry = st_union(geometry))

gent_shape <- filter(gent_status_shapes, gent_status == "Gentrifying")
nongent_shape <- filter(gent_status_shapes, gent_status == "Non-Gentrifying")
hiinc_shape <- filter(gent_status_shapes, gent_status == "Higher Income")


# Map Theme ---------------------------------------------------------------

map_theme <- function() {
  theme(legend.position = c(.1, .7),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(colour = "grey50", face = "italic", size = 8))
}

# Tried to pick a good outline color that doesn't make the nearby colors look lighter/darker
# ¯\_(ツ)_/¯
# https://color.adobe.com/create/color-wheel/?base=2&rule=Custom&selected=4&name=My%20Color%20Theme&mode=hex&rgbvalues=0.23251253702980193,0,0.39,1,0.983995966878183,0,0.37798036799112983,0.79,0.4595429435023306,0.3686274509803922,0.4593902909249347,0.79,1,0.6519256786492802,0.47057121242820443&swatchOrder=0,3,2,1,4
outline_gent <- function(greyout = FALSE) {
  if (greyout == FALSE) {
    list(
      geom_sf(data = gent_shape, fill = NA, color = "#FF8945", size = 1)
    )
  } else {
    list(
      geom_sf(data = nongent_shape, fill = "white", alpha = .2, color = NA),
      geom_sf(data = hiinc_shape, fill = "white", alpha = .2, color = NA),
      geom_sf(data = gent_shape, fill = NA, color = "#FF8945", size = 1)
    )
  }
}

outline_nongent <- function(greyout = FALSE) {
  if (greyout == FALSE) {
    list(
      geom_sf(data = nongent_shape, fill = NA, color = "#FF8945", size = 1)
    )
  } else {
    list(
      geom_sf(data = gent_shape, fill = "white", alpha = .2, color = NA),
      geom_sf(data = hiinc_shape, fill = "white", alpha = .2, color = NA),
      geom_sf(data = nongent_shape, fill = NA, color = "#FF8945", size = 1)
    )
  }
}


# MAPS! -------------------------------------------------------------------

# Gentrification
map_data %>% 
  filter(!is.na(gent_status)) %>% 
  ggplot(aes(fill = gent_status)) + 
  geom_sf(data = boros, fill = "grey", color = "white", size = 0.1) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_manual(values = c("Gentrifying" = "#FFD200", 
                               "Non-Gentrifying" = "#B21293", 
                               "Higher Income" = "#00B2AB")) +
  map_theme() +
  labs(title = "Neighborhood Gentrification Status \nNew York City",
       subtitle = "ZIP Census Tabulation Areas (ZCTAs)",
       fill = NULL,
       caption = "Sources: Minnesota Population Center, NHGIS; Neighborhood Change Database")

ggsave("../dropbox/capstone/images/zcta_gentrification.png", width = 20, height = 20, units = "cm")



# Abulatory Sensitive Conditions Discharges 2010
acsc_map <- map_data %>% 
  mutate(acscd_p1000_2010 = acscd_rt_2010 * 1000) %>% 
  ggplot(aes(fill = acscd_p1000_2010)) + 
  geom_sf(data = boros, fill = "grey", color = "white", size = 0.1) +
  geom_sf(color = "white", size = 0.1) +
  viridis::scale_fill_viridis() +
  map_theme() +
  labs(title = "Ambulatory Care Sensitive Condition Discharges per 1,000 Medicare Beneficiaries \nNew York City, 2010",
       subtitle = "ZIP Census Tabulation Areas (ZCTAs)",
       fill = NULL,
       caption = "Sources: Dartmouth Atlas; Minnesota Population Center, NHGIS")

acsc_map
ggsave("../dropbox/capstone/images/zcta_acsc_2010.png", width = 20, height = 20, units = "cm")

acsc_map + outline_gent()
ggsave("../dropbox/capstone/images/zcta_acsc_2010_outline_gent.png", width = 20, height = 20, units = "cm")

acsc_map + outline_gent(greyout = TRUE)
ggsave("../dropbox/capstone/images/zcta_acsc_2010_outline_gent_grey.png", width = 20, height = 20, units = "cm")

acsc_map + outline_nongent()
ggsave("../dropbox/capstone/images/zcta_acsc_2010_outline_nongent.png", width = 20, height = 20, units = "cm")

acsc_map + outline_nongent(greyout = TRUE)
ggsave("../dropbox/capstone/images/zcta_acsc_2010_outline_nongent_grey.png", width = 20, height = 20, units = "cm")



# Primary Care Providers 2010

all_pcp_d_levels <- c("Less than 0.5", "0.5 to 1.0", "1.0 to 2.0", "2.0 to 3.0", "Greater than 3.0")

allpcp_map <- map_data %>%
  filter(!is.na(gent_status), !is.na(allpcp_p1000_2010)) %>%
  mutate(allpcp_p1000_2010_d = case_when(.$allpcp_p1000_2010 < 0.5  ~ all_pcp_d_levels[[1]],
                                         .$allpcp_p1000_2010 < 1.0  ~ all_pcp_d_levels[[2]],
                                         .$allpcp_p1000_2010 < 2.0  ~ all_pcp_d_levels[[3]],
                                         .$allpcp_p1000_2010 < 3.0  ~ all_pcp_d_levels[[4]],
                                         .$allpcp_p1000_2010 >= 3.0 ~ all_pcp_d_levels[[5]]),
         allpcp_p1000_2010_d = ordered(allpcp_p1000_2010_d, levels = all_pcp_d_levels)) %>%
  ggplot(aes(fill = allpcp_p1000_2010_d)) +
  geom_sf(data = boros, fill = "grey", color = "white", size = 0.1) +
  geom_sf(color = "white", size = 0.1) +
  viridis::scale_fill_viridis(discrete = TRUE, begin = 0.15, end = 1, 
                              guide = guide_legend(reverse = TRUE)) +
  map_theme() +
  labs(title = "Primary Care Providers per 1,000 Residents \nNew York City, 2010",
       subtitle = "ZIP Census Tabulation Areas (ZCTAs)",
       fill = NULL,
       caption = "Sources: Dartmouth Atlas; Minnesota Population Center, NHGIS")


allpcp_map
ggsave("../dropbox/capstone/images/zcta_allpcp_2010.png", width = 20, height = 20, units = "cm")

allpcp_map + outline_gent()
ggsave("../dropbox/capstone/images/zcta_allpcp_2010_outline_gent.png", width = 20, height = 20, units = "cm")

allpcp_map + outline_gent(greyout = TRUE)
ggsave("../dropbox/capstone/images/zcta_allpcp_2010_outline_gent_grey.png", width = 20, height = 20, units = "cm")

allpcp_map + outline_nongent()
ggsave("../dropbox/capstone/images/zcta_allpcp_2010_outline_nongent.png", width = 20, height = 20, units = "cm")

allpcp_map + outline_nongent(greyout = TRUE)
ggsave("../dropbox/capstone/images/zcta_allpcp_2010_outline_nongent_grey.png", width = 20, height = 20, units = "cm")



# CHS general Health (% Excellent or Very Good)
chs_map_data %>% 
  ggplot(aes(fill = good_health)) + 
  geom_sf(data = boros, fill = "grey", color = "white", size = 0.1) +
  geom_sf(color = "white", size = 0.1) +
  viridis::scale_fill_viridis(labels = scales::percent_format(), limits = c(0, 1)) +
  map_theme() +
  labs(title = "Percent of Residents Reporting \"Excellent\" or \"Very Good\" General Health \nNew York City, 2009",
       subtitle = "United Hospital Fund Neighborhoods (UHF 34)",
       fill = NULL,
       caption = "Source: New York City Community Health Survey")


ggsave("../dropbox/capstone/images/uhf34_gen_health.png", width = 20, height = 20, units = "cm")

