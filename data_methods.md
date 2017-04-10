Data & Methods
================
2017-04-10

``` r
library(tidyverse)
library(sf)
library(units)

options(scipen = 20)
```

ZCTAs
-----

``` r
zcta2010 <- st_read("../dropbox/capstone/shapefiles/nyc_zcta2010/nyc_zcta2010.shp", "nyc_zcta2010", 
                    stringsAsFactors = FALSE)

zcta_vars <- feather::read_feather("../dropbox/capstone/data_clean/all_data.feather") %>% 
  transmute(zcta2010 = zcta2010,
            pop = pop_tot_2010, 
            inc90 = avg_inc_adj_1990,
            chg_rent = (avg_rent_adj_2010 - avg_rent_adj_1990) / avg_rent_adj_1990)
```

``` r
dropped_zctas <- anti_join(zcta2010, zcta_vars, by = "zcta2010") %>% nrow()
```

There are 10 ZCTAs excluded because their population was less than 2,000, leaving 179 for our analysis.

``` r
zcta_stats <- zcta2010 %>% 
  inner_join(zcta_vars, by = "zcta2010") %>% 
  mutate(area = st_area(.) %>% set_units(mi^2)) %>% 
  summarize_at(vars(area, pop), funs(mean, median, IQR, sd))
```

Of the ZCTAs we include, the median size is 1.35 and the median population is 40286

ZCTA Gentrification stats
-------------------------

``` r
gent_stats <- zcta_vars %>% 
  summarise(inc90_40th = quantile(inc90, .4),
            rent_chg_med = median(chg_rent))
```

For our zcta-level gentrification definition:

40th percentile zcta for household income was 72586 (2016$) Median percent change in average gross rent between 1990 and 2010 was 10.2%

PCSAs
-----

``` r
# pcsa2010 <- st_read("../dropbox/capstone/shapefiles/pcsav3_1shapefiles/uspcsav31_HRSA.shp", "uspcsav31_HRSA", 
#                     stringsAsFactors = FALSE)
# 
# (pcsa_area <- pcsa2010 %>% 
#   mutate(area = st_area(.) %>% set_units(mi^2)) %>% 
#   summarize_at(vars(area), funs(mean, median, IQR, sd)))
```

Tracts
------

``` r
tract2010 <- st_read("../dropbox/capstone/shapefiles/nyct2010_17a/nyct2010.shp", "nyct2010", 
                    stringsAsFactors = FALSE)
tract_stats <- tract2010 %>% 
  mutate(area = st_area(.) %>% set_units(mi^2)) %>% 
  summarize_at(vars(area), funs(mean, median, IQR, sd))
```

The median 2010 census tract is 0.071 square miles.
