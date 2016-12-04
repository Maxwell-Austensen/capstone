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
# Upper Limit = (1000 / n) (d + (1.96 x square root of d)) #
ci <- function(x , n , d) {
  l <- (x / sum(n))*((sum(d)) - (1.96 * sqrt(sum(d))))
  r <- (x / sum(n))*((sum(d)) + (1.96 * sqrt(sum(d))))
  output <-  paste0(signif(l, digits = 3), ",", signif(r, digits = 3))
  return(output)
}

ci00 <- function(x, n, d, r) {
  l <- l <- (x / sum(n[!is.na(r)], na.rm = T))*((sum(d[!is.na(r)], na.rm = T)) - (1.96 * sqrt(sum(d[!is.na(r)], na.rm = T))))
  r <- (x / sum(n[!is.na(r)], na.rm = T))*((sum(d[!is.na(r)], na.rm = T)) + (1.96 * sqrt(sum(d[!is.na(r)], na.rm = T))))
  output <-  paste0(signif(l, digits = 3), ",", signif(r, digits = 3))
  return(output)
}
cimedicare <- function(x) {
  l <- (x / sum(dart_nyc$medicare_denom[!is.na(dart_nyc$acscd_rt)], na.rm=T))*((sum(dart_nyc$medicare_acscd[!is.na(dart_nyc$acscd_rt)], na.rm=T)) - (1.96 * sqrt(sum(dart_nyc$medicare_acscd[!is.na(dart_nyc$acscd_rt)], na.rm=T))))
  r <- (x / sum(dart_nyc$medicare_denom[!is.na(dart_nyc$acscd_rt)], na.rm=T))*((sum(dart_nyc$medicare_acscd[!is.na(dart_nyc$acscd_rt)], na.rm=T)) + (1.96 * sqrt(sum(dart_nyc$medicare_acscd[!is.na(dart_nyc$acscd_rt)], na.rm=T))))
  output <-  paste0(signif(l, digits = 4), ",", signif(r, digits = 4))
  return(output)
}

ci(1000, dart_nyc$totpop, dart_nyc$physicians)
```

    ## [1] "3.16,3.24"

``` r
dart_nyc <- mutate(dart_nyc, acscd_rt = if_else(medicare_denom != 0, (medicare_acscd / medicare_denom)*1000, NA_real_))

sumtotpop <- sum(dart_nyc$totpop)
sumfempop <- sum(dart_nyc$totfem15_64)
summedicare <- sum(dart_nyc$medicare_denom[!is.na(dart_nyc$acscd_rt)], na.rm=T)

sumtable <- 
  dart_nyc %>%
  group_by(gent) %>%
  summarise(physicians_rt = sum(physicians) / sum(totpop)*1000,
            phys_ci = ci(1000, totpop, physicians),
            pa_rt = sum(pa) / sum(totpop)*10000,
            pa_ci = ci(10000, totpop, pa),
            np_rt = sum(pcnp) / sum(totpop)*100000,
            np_ci = ci(100000, totpop, pcnp),
            cnm_rt = sum(pccnm) / sum(totfem15_64)*10000,
            cnm_ci = ci(10000, totfem15_64, pccnm),
            allpcp_rt = sum(allpcp) / sum(totpop)*1000,
            allpco_ci = ci(1000, totpop, allpcp), 
            specs_rt = sum(specs) / sum(totpop)*1000,
            specs_ci = ci(1000, totpop, specs),
            obstets_rt = sum(obstets) / sum(totfem15_64)*10000,
            obstets_ci = ci(10000, totfem15_64, obstets),
            img_rt = sum(allimg) / sum(totpop)*1000,
            img_ci = ci(1000, totpop, allimg),
            acscd_rt = sum(medicare_acscd[!is.na(acscd_rt)], na.rm=T) / sum(medicare_denom[!is.na(acscd_rt)], na.rm=T) * 1000) %>%
  mutate_if(is.numeric, funs(round(., digits = 2)))

sumtable %>% gather("var", "value", -gent) %>% spread(gent, value)
```

    ## # A tibble: 17 × 4
    ##              var Gentrifying `Non-Gentrifying` `Higher-Income`
    ## *          <chr>       <chr>             <chr>           <chr>
    ## 1       acscd_rt       86.82             90.35           61.21
    ## 2      allpco_ci   0.93,1.01        0.976,1.08       1.38,1.45
    ## 3      allpcp_rt        0.97              1.03            1.42
    ## 4         cnm_ci   1.28,1.84        0.606,1.12       1.21,1.56
    ## 5         cnm_rt        1.56              0.86            1.39
    ## 6         img_ci 0.909,0.993         1.14,1.25       1.58,1.65
    ## 7         img_rt        0.95               1.2            1.62
    ## 8          np_ci   6.51,8.88          6.3,9.22       11.2,13.2
    ## 9          np_rt        7.69              7.76           12.18
    ## 10    obstets_ci    3.96,4.9         3.91,5.07       7.48,8.33
    ## 11    obstets_rt        4.43              4.49             7.9
    ## 12         pa_ci   2.35,2.78         2.17,2.68        4.2,4.58
    ## 13         pa_rt        2.56              2.43            4.39
    ## 14       phys_ci   1.72,1.84         2.36,2.53          4,4.12
    ## 15 physicians_rt        1.78              2.44            4.06
    ## 16      specs_ci 0.902,0.985         1.45,1.58       2.74,2.84
    ## 17      specs_rt        0.94              1.52            2.79

``` r
kable(sumtable, col.names = c("Gentrification Status", "Physicians per 1,000", "CI", "PAs per 10,000", "CI", "NPs per 100,000","CI", "Cert. Nurse Midwives per 10,000", "CI", "Primary Care Providers per 1,000","CI", "Specialists per 1,000","CI", "Repro. Health Providers per 10,000", "CI","International Medical Grads per 1,000", "CI","Ambulatory Sensitive Condition Discharges per 1,000"), caption = "Rates of providers and ambulatory sensitive conditions by gentrification status of providers' census tract")
```

| Gentrification Status |  Physicians per 1,000| CI        |  PAs per 10,000| CI        |  NPs per 100,000| CI        |  Cert. Nurse Midwives per 10,000| CI         |  Primary Care Providers per 1,000| CI         |  Specialists per 1,000| CI          |  Repro. Health Providers per 10,000| CI        |  International Medical Grads per 1,000| CI          |  Ambulatory Sensitive Condition Discharges per 1,000|
|:----------------------|---------------------:|:----------|---------------:|:----------|----------------:|:----------|--------------------------------:|:-----------|---------------------------------:|:-----------|----------------------:|:------------|-----------------------------------:|:----------|--------------------------------------:|:------------|----------------------------------------------------:|
| Gentrifying           |                  1.78| 1.72,1.84 |            2.56| 2.35,2.78 |             7.69| 6.51,8.88 |                             1.56| 1.28,1.84  |                              0.97| 0.93,1.01  |                   0.94| 0.902,0.985 |                                4.43| 3.96,4.9  |                                   0.95| 0.909,0.993 |                                                86.82|
| Non-Gentrifying       |                  2.44| 2.36,2.53 |            2.43| 2.17,2.68 |             7.76| 6.3,9.22  |                             0.86| 0.606,1.12 |                              1.03| 0.976,1.08 |                   1.52| 1.45,1.58   |                                4.49| 3.91,5.07 |                                   1.20| 1.14,1.25   |                                                90.35|
| Higher-Income         |                  4.06| 4,4.12    |            4.39| 4.2,4.58  |            12.18| 11.2,13.2 |                             1.39| 1.21,1.56  |                              1.42| 1.38,1.45  |                   2.79| 2.74,2.84   |                                7.90| 7.48,8.33 |                                   1.62| 1.58,1.65   |                                                61.21|

``` r
# add footnote for what is in each cat and for rates of obstets and nurse midwives per females 15 - 65 #
```
