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


# Set directories
root <- "C:/Users/austensen/Dropbox/capstone/"
knitr::opts_knit$set(root.dir = root)
# setwd(root)
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

table
```

    ## # A tibble: 42 × 3
    ##     Borough               Neighborhood
    ##       <chr>                      <chr>
    ## 1     Bronx              Central Bronx
    ## 2     Bronx     Bronx Park and Fordham
    ## 3     Bronx High Bridge and Morrisania
    ## 4     Bronx Hunts Point and Mott Haven
    ## 5     Bronx  Kingsbridge and Riverdale
    ## 6     Bronx            Northeast Bronx
    ## 7     Bronx            Southeast Bronx
    ## 8  Brooklyn           Central Brooklyn
    ## 9  Brooklyn         Southwest Brooklyn
    ## 10 Brooklyn               Borough Park
    ## # ... with 32 more rows, and 1 more variables: `ZIP Codes` <chr>

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
dir.create("./data/crosswalks/", showWarnings = TRUE)
```

    ## Warning in dir.create("./data/crosswalks/", showWarnings = TRUE): '.\data
    ## \crosswalks' already exists

``` r
write_feather(xwalk, "./data/crosswalks/uhf_42_zip.feather")
```
