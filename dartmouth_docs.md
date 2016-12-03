Dartmouth\_docs
================
2016-12-03

``` r
library(stringr); library(foreign); library(QuantPsyc); library(psych); library(knitr); library(tidyverse)
```

``` r
dart_raw <- read.dbf("../dropbox/capstone/data/raw/t_103113_1.dbf", as.is = TRUE)
names(dart_raw) <- names(dart_raw) %>% str_to_lower()

dart_raw2 <- read.dbf("../dropbox/capstone/data/raw/other_files/t_103113_2.dbf", as.is = T)
names(dart_raw2) <- names(dart_raw2) %>% str_to_lower()

dart_np <- read.dbf("../dropbox/capstone/data/raw/other_files/t_cnm_np_122013.dbf", as.is = T)
names(dart_np) <- names(dart_np) %>% str_to_lower()
```

``` r
dart_np <-
  subset.data.frame(dart_np, county %in% c("36005", "36047", "36061", "36081", "36085"))

dart_np <-
  dart_np %>%
  transmute(
    geoid = str_c(county, tract),
    boro = recode(county, 
            "36005" = "Bronx", "36047" = "Brooklyn", "36061" = "Manhattan", 
            "36081" = "Queens", "36085" = "Staten Island"),
    pccnm = t_cnmfte, 
    pcnp = t_npfte
  ) %>%
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .)))
```

``` r
xwalk <- 
  read_csv("../dropbox/capstone/data/crosswalks/gent_xwalk.csv", col_types = cols(geoid = "c")) %>% 
  mutate(gent = recode(gent_ind, `0` = NA_character_, `1` = "Gentrifying", 
                                 `2` = "Non-Gentrifying", `3` = "Higher-Income"),
         gent = ordered(gent, levels = c("Gentrifying", "Non-Gentrifying", "Higher-Income"))) %>% 
  select(geoid, gent) %>% 
  filter(!is.na(gent))
```

``` r
dart_fem <-
  dart_raw2 %>%
  as_data_frame() %>%
  filter(county %in% c("36005", "36047", "36061", "36081", "36085")) %>% 
  transmute(
    geoid = str_c(county, tract),
    boro = recode(county, 
            "36005" = "Bronx", "36047" = "Brooklyn", "36061" = "Manhattan", 
            "36081" = "Queens", "36085" = "Staten Island"),
    totfem15_64 = tf15_64) %>%
    mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .)))
```

``` r
dart_nyc <- 
  dart_raw %>% 
  as_data_frame() %>%
  filter(county %in% c("36005", "36047", "36061", "36081", "36085")) %>% 
  transmute(
    geoid = str_c(county, tract),
    boro = recode(county, 
            "36005" = "Bronx", "36047" = "Brooklyn", "36061" = "Manhattan", 
            "36081" = "Queens", "36085" = "Staten Island"),
    medicare_denom = tbene_n10,
    medicare_acscd = tacs_10,
    pcp = tg_doc,
    specialist = ts_doc,
    obgyn = to_doc,
    fedpcp = tg_feddoc,
    fedspec = ts_feddoc, 
    fedob = to_feddoc,
    nonfedpcp = tg_nfeddoc, 
    nonfedspec = ts_nfeddoc,
    nonfedob = to_nfeddoc,
    img_pcp = tg_img,
    img_spec = ts_img,
    img_ob = to_img, 
    cmg_pcp = tg_cmg,
    cmg_spec = ts_cmg, 
    cmg_ob = to_cmg, 
    nimg_pcp = tg_nimg,
    nimg_spec = ts_nimg,
    nimg_ob = to_nimg,
    pa_pcp = tpa_pc,
    pa_spec = tpa_pc_3sp,
    pa_ob = tpa_pc_ob,
    pa_other = tpa_pc_oth,
    fqhc = tfqhc10,
    totage0_14 = tage00_14,
    totage15_64 = tage15_64,
    totage_65up = tage65_up
  ) %>% 
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .))) %>%
  inner_join(dart_np, by = c("boro", "geoid")) %>%
  inner_join(dart_fem, by = c("geoid", "boro")) %>%
  inner_join(xwalk, by = "geoid")
```

``` r
dart_nyc$physicians <- dart_nyc$pcp + dart_nyc$specialist + dart_nyc$obgyn
dart_nyc$pa <- dart_nyc$pa_ob + dart_nyc$pa_pcp + dart_nyc$pa_spec + dart_nyc$pa_other
dart_nyc$obstets <- dart_nyc$obgyn + dart_nyc$pa_ob + dart_nyc$pccnm
dart_nyc$allpcp <- dart_nyc$pcp + dart_nyc$pcnp + dart_nyc$pa_pcp
dart_nyc$specs <- dart_nyc$specialist + dart_nyc$pa_spec
dart_nyc$totpop <- dart_nyc$totage0_14 + dart_nyc$totage15_64 + dart_nyc$totage_65up
dart_nyc$allimg <- dart_nyc$img_ob + dart_nyc$img_pcp + dart_nyc$img_spec
```

``` r
sumtable <- 
  dart_nyc %>%
  group_by(gent) %>%
  summarise(physicians_rt = sum(physicians) / sum(totpop)*1000,
            pa_rt = sum(pa) / sum(totpop)*10000,
            np_rt = sum(pcnp) / sum(totpop)*100000,
            cnm_rt = sum(pccnm) / sum(totfem15_64)*10000,
            allpcp_rt = sum(allpcp) / sum(totpop)*1000,
            specs_rt = sum(specs) / sum(totpop)*1000,
            obstets_rt = sum(obstets) / sum(totfem15_64)*10000,
            img_rt = sum(allimg) / sum(totpop)*1000,
            acscd_rt = sum(medicare_acscd, na.rm=T) / sum(medicare_denom, na.rm=T) * 1000) %>%
  mutate_if(is.numeric, funs(round(., digits = 2)))

sumtable %>% gather("var", "value", -gent) %>% spread(gent, value)
```

    ## # A tibble: 9 × 4
    ##             var Gentrifying `Non-Gentrifying` `Higher-Income`
    ## *         <chr>       <dbl>             <dbl>           <dbl>
    ## 1      acscd_rt       71.38             77.33           50.21
    ## 2     allpcp_rt        0.97              1.03            1.42
    ## 3        cnm_rt        1.56              0.86            1.39
    ## 4        img_rt        0.95              1.20            1.62
    ## 5         np_rt        7.69              7.76           12.18
    ## 6    obstets_rt        4.43              4.49            7.90
    ## 7         pa_rt        2.56              2.43            4.39
    ## 8 physicians_rt        1.78              2.44            4.06
    ## 9      specs_rt        0.94              1.52            2.79

``` r
kable(sumtable, col.names = c("Gentrification Status", "Physicians per 1000", "PAs per 10,000", "NPs per 100,000", "Cert. Nurse Midwives per 10,000", "Primary Care Providers per 1000", "Specialists per 1000", "Repro. Health Providers per 10,000", "International Medical Grads per 1000", "Ambulatory Sensitive Condition Discharges per 1000"))
```

| Gentrification Status |  Physicians per 1000|  PAs per 10,000|  NPs per 100,000|  Cert. Nurse Midwives per 10,000|  Primary Care Providers per 1000|  Specialists per 1000|  Repro. Health Providers per 10,000|  International Medical Grads per 1000|  Ambulatory Sensitive Condition Discharges per 1000|
|:----------------------|--------------------:|---------------:|----------------:|--------------------------------:|--------------------------------:|---------------------:|-----------------------------------:|-------------------------------------:|---------------------------------------------------:|
| Gentrifying           |                 1.78|            2.56|             7.69|                             1.56|                             0.97|                  0.94|                                4.43|                                  0.95|                                               71.38|
| Non-Gentrifying       |                 2.44|            2.43|             7.76|                             0.86|                             1.03|                  1.52|                                4.49|                                  1.20|                                               77.33|
| Higher-Income         |                 4.06|            4.39|            12.18|                             1.39|                             1.42|                  2.79|                                7.90|                                  1.62|                                               50.21|

col.names = c("Physicians per 1000", "PAs per 10,000", "NPs per 100,000", "Cert. Nurse Midwives per 10,000", "Primary Care Providers per 1000", "Specialists per 1000", "Repro. Health Providers per 10,000", ))
