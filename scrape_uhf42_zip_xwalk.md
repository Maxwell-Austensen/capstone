Scrape Crosswalk: UHF (42) - Zip Code
================

``` r
# Install packages if needed
package_list <- c("tidyverse", "rvest", "stringr", "feather", "knitr")
new_packages <- package_list[! package_list %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse) # for tidy data manipulation
library(rvest) # for html web scraping
library(stringr) # for string manipulation
library(feather) # for saving data files
```

### Scrape table from website

``` r
url <- "http://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm"

table <- 
  url %>%
  read_html() %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  .[[1]] %>% 
  as_data_frame()

table %>% select(`ZIP Codes`)
```

    ## # A tibble: 42 × 1
    ##                                `ZIP Codes`
    ##                                      <chr>
    ## 1                      10453, 10457, 10460
    ## 2                      10458, 10467, 10468
    ## 3                      10451, 10452, 10456
    ## 4               10454, 10455, 10459, 10474
    ## 5                             10463, 10471
    ## 6               10466, 10469, 10470, 10475
    ## 7  10461, 10462,10464, 10465, 10472, 10473
    ## 8        11212, 11213, 11216, 11233, 11238
    ## 9                      11209, 11214, 11228
    ## 10              11204, 11218, 11219, 11230
    ## # ... with 32 more rows

### Reshape data for crosswalk format

``` r
xwalk <-
  table %>%  
  mutate(zips = str_split(`ZIP Codes`, ",")) %>% 
  unnest(zips) %>% 
  transmute(
    boro = Borough,
    uhf_42 = Neighborhood,
    zip = as.integer(zips)
  )

xwalk
```

    ## # A tibble: 178 × 3
    ##     boro                     uhf_42   zip
    ##    <chr>                      <chr> <int>
    ## 1  Bronx              Central Bronx 10453
    ## 2  Bronx              Central Bronx 10457
    ## 3  Bronx              Central Bronx 10460
    ## 4  Bronx     Bronx Park and Fordham 10458
    ## 5  Bronx     Bronx Park and Fordham 10467
    ## 6  Bronx     Bronx Park and Fordham 10468
    ## 7  Bronx High Bridge and Morrisania 10451
    ## 8  Bronx High Bridge and Morrisania 10452
    ## 9  Bronx High Bridge and Morrisania 10456
    ## 10 Bronx Hunts Point and Mott Haven 10454
    ## # ... with 168 more rows

### Save clean crosswalk

``` r
dir.create("../Dropbox/capstone/data/crosswalks/", showWarnings = FALSE)
write_feather(xwalk, "../Dropbox/capstone/data/crosswalks/uhf_42_zip.feather")
```
