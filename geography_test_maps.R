uhf34_03 <- st_read("../dropbox/capstone/shapefiles/chs_2003_dohmh_2008/CHS_2003_DOHMH_2008.shp", "CHS_2003_DOHMH_2008",
                    stringsAsFactors = FALSE)

uhf34_09 <- st_read("../dropbox/capstone/shapefiles/CHS_2009_DOHMH_2010B/CHS_2009_DOHMH_2010B.shp", "CHS_2009_DOHMH_2010B",
                    stringsAsFactors = FALSE)

# These are the same (yay!)
uhf34_03 %>% 
  mutate(random_var = sample(0:30, n(), replace = TRUE) %>% as.factor) %>% 
  ggplot() + 
  geom_sf(aes(fill = random_var), color = NA) +
  geom_sf(data = uhf34_09, fill = NA, color = "black") +
  theme(legend.position = "none")

zcta2010 <- st_read("../dropbox/capstone/shapefiles/nyc_zcta2010/nyc_zcta2010.shp", "nyc_zcta2010", 
                    stringsAsFactors = FALSE)

boros <- st_read("../dropbox/capstone/shapefiles/nybb_17a/nybb.shp", "nybb", stringsAsFactors = FALSE)


zcta2010 %>% 
  mutate(random_var = sample(0:30, n(), replace = TRUE) %>% as.factor) %>% 
  ggplot() + 
  geom_sf(aes(fill = random_var), color = "white", size = 0.5) +
  geom_sf(data = uhf34_09, fill = NA, color = "black", linetype = "dashed", size = 0.3) +
  geom_sf(data = boros, fill = NA, color = "red", size = 0.3) +
  theme(legend.position = "none")
