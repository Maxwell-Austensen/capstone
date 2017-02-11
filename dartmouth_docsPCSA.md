Dartmouth\_docsPCSA
================
2017-02-11

``` r
library(stringr); library(foreign); library(QuantPsyc); library(psych); library(knitr); library(tidyverse)
```

``` r
dart_raw <- read.dbf("../dropbox/capstone/2010 data/p_103113_1.dbf", as.is = TRUE)
names(dart_raw) <- names(dart_raw) %>% str_to_lower()

dart_raw2 <- read.dbf("../dropbox/capstone/2010 data/p_103113_2.dbf", as.is = T)
names(dart_raw2) <- names(dart_raw2) %>% str_to_lower()

dart_np <- read.dbf("../dropbox/capstone/2010 data/p_cnm_np_122013.dbf", as.is = T)
names(dart_np) <- names(dart_np) %>% str_to_lower()
```

``` r
dart_np$pcsa <-as.numeric(dart_np$pcsa)

xwalk <- read_csv("../Dropbox/capstone/tract_pcsa_xwalk.csv") %>% distinct(pcsa, pcsa_name)
```

    ## Parsed with column specification:
    ## cols(
    ##   geoid = col_double(),
    ##   pcsa = col_double(),
    ##   pcsa_name = col_character()
    ## )

``` r
dart_np <- semi_join(dart_np, xwalk, by = "pcsa")

dart_np <-
  dart_np %>%
  transmute(pcsa = pcsa,
    pcsa_l = pcsa_l,
    cnm_fte = p_cnmfte, 
    np_fte = p_npfte
  ) %>%
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .)))
```

``` r
dart_raw2$pcsa <- as.numeric(dart_raw2$pcsa)
dart_raw2 <- semi_join(dart_raw2, xwalk, by = "pcsa")

dart_fem <-
  dart_raw2 %>%
  transmute(pcsa = pcsa,
    pcsa_l = pcsa_l,
    fem15_44 = pp12i30 + pp12i31 + pp12i32 + pp12i33 + pp12i34 + pp12i36 + pp12i37 + pp12i38
  ) %>%
    mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .)))
```

``` r
gentxwalk <- 
  read_csv("../dropbox/capstone/pcsa_gent.csv", col_types = cols(pcsa = "c")) %>% 
  select(pcsa, gent_status) %>% 
  filter(!is.na(gent_status))

gentxwalk$pcsa <- as.numeric(gentxwalk$pcsa)
gentxwalk$gent_status <- as.ordered(gentxwalk$gent_status)
```

``` r
dart_raw$pcsa <- as.numeric(dart_raw$pcsa)
dart_raw <- semi_join(dart_raw, xwalk, by = "pcsa")

dart_nyc <- 
  dart_raw %>% 
  transmute(
    pcsa = pcsa,
    pcsa_l = pcsa_l,
    pcp = pg_doc,
    famprac = pf_doc, 
    internist = pi_doc,
    specialist = ps_doc,
    obgyn = po_doc,
    nonfedpcp = pg_nfeddoc,
    nonfedspec = ps_nfeddoc,
    nonfedob = po_nfeddoc,
    img_pcp = pg_img,
    img_spec = ps_img,
    img_ob = po_img,
    pa_pcp = ppa_pc,
    pa_spec = ppa_pc_3sp, #GP/IM/Pediatrics PAs - as specialists
    pa_ob = ppa_pc_ob, #ob pa
    pa_oth = ppa_pc_oth, #geriatrics or unk specialty PA
    partb_pcp = pvt_pc10, #pc visits Part b only
    pcpvt_tot = pvt_pc10 + pvt_rhc10 + pvt_fqhc10, #total pcp visits for part B and OTP
    partb_amb = pvt_am10, #ambulatory visits part b only
    edperday = pvt_ed10, #est total number of ED visits per day
    pcpvt_rhc = pvt_rhc10,
    pcpvt_fqhc = pvt_fqhc10,
    medicare_acscd = pacs_10, #acs discharges
    medicare_denom = pbene_n10, #total bene pop for 2010, all medicare, not part-specific
    totpop = page00_14 + page15_64 + page65_up
  ) %>% 
  mutate_if(is.double, funs(if_else(. %in% c(-99, -999), NA_real_, .))) %>%
  inner_join(dart_np, by = "pcsa") %>%
  inner_join(dart_fem, by = "pcsa") %>%
  inner_join(gentxwalk, by = "pcsa")
```

``` r
dart_nyc$physicians <- dart_nyc$pcp + dart_nyc$specialist + dart_nyc$obgyn + dart_nyc$famprac + dart_nyc$internist
dart_nyc$pa <- dart_nyc$pa_ob + dart_nyc$pa_pcp + dart_nyc$pa_spec + dart_nyc$pa_oth
dart_nyc$obstets <- dart_nyc$obgyn + dart_nyc$pa_ob + dart_nyc$cnm_fte
dart_nyc$allpcp <- dart_nyc$pcp + dart_nyc$np_fte + dart_nyc$pa_pcp #should we include dart_nyc$famprac & dart_nyc$internist ??
dart_nyc$specs <- dart_nyc$specialist + dart_nyc$pa_spec + dart_nyc$pa_oth
dart_nyc$allimg <- dart_nyc$img_ob + dart_nyc$img_pcp + dart_nyc$img_spec
```

``` r
# Upper Limit = (1000 / n) (d + (1.96 x square root of d)) #
ci <- function(x , n , d) {
  l <- (x / sum(n))*((sum(d)) - (1.96 * sqrt(sum(d))))
  r <- (x / sum(n))*((sum(d)) + (1.96 * sqrt(sum(d))))
  output <-  paste0(signif(l, digits = 3), " , ", signif(r, digits = 3))
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

    ## [1] "4.01 , 4.1"

``` r
### WON'T WORK
sumtotpop <- sum(dart_nyc$totpop)
sumfempop <- sum(dart_nyc$fem15_44)

sumtable <- 
  dart_nyc %>%
  group_by(gent_status) %>%
  summarise(physicians_rt = sum(physicians) / sum(totpop)*1000,
            phys_ci = ci(1000, totpop, physicians),
            pa_rt = sum(pa) / sum(totpop)*10000,
            pa_ci = ci(10000, totpop, pa),
            np_rt = sum(np_fte) / sum(totpop)*100000,
            np_ci = ci(100000, totpop, np_fte),
            cnm_rt = sum(cnm_fte) / sum(fem15_44)*10000,
            cnm_ci = ci(10000, fem15_44, cnm_fte),
            allpcp_rt = sum(allpcp) / sum(totpop)*1000,
            allpcp_ci = ci(1000, totpop, allpcp), 
            specs_rt = sum(specs) / sum(totpop)*1000,
            specs_ci = ci(1000, totpop, specs),
            obstets_rt = sum(obstets) / sum(fem15_44)*10000,
            obstets_ci = ci(10000, fem15_44, obstets),
            img_rt = sum(allimg) / sum(totpop)*1000,
            img_ci = ci(1000, totpop, allimg)) %>%
    mutate_if(is.numeric, funs(round(., digits = 2)))

kable(sumtable)
```

| gent\_status    |  physicians\_rt| phys\_ci    |  pa\_rt| pa\_ci      |  np\_rt| np\_ci      |  cnm\_rt| cnm\_ci     |  allpcp\_rt| allpcp\_ci  |  specs\_rt| specs\_ci   |  obstets\_rt| obstets\_ci |  img\_rt| img\_ci     |
|:----------------|---------------:|:------------|-------:|:------------|-------:|:------------|--------:|:------------|-----------:|:------------|----------:|:------------|------------:|:------------|--------:|:------------|
| Gentrifying     |            2.72| 2.64 , 2.8  |    2.17| 1.95 , 2.39 |    8.63| 7.24 , 10   |     2.98| 2.39 , 3.57 |        1.06| 1.01 , 1.1  |       1.29| 1.24 , 1.35 |         9.32| 8.28 , 10.4 |     1.10| 1.05 , 1.15 |
| High Income     |            4.71| 4.65 , 4.77 |    4.20| 4.03 , 4.38 |   11.78| 10.9 , 12.7 |     2.63| 2.31 , 2.95 |        1.38| 1.35 , 1.41 |       2.87| 2.83 , 2.92 |        15.46| 14.7 , 16.2 |     1.53| 1.5 , 1.56  |
| Non-Gentrifying |            2.95| 2.85 , 3.05 |    3.41| 3.08 , 3.74 |    7.64| 6.07 , 9.21 |     2.72| 2.04 , 3.4  |        1.09| 1.03 , 1.15 |       1.57| 1.5 , 1.64  |         9.45| 8.18 , 10.7 |     1.34| 1.27 , 1.4  |

``` r
dart_nyc <- mutate(dart_nyc, acscd_rt = if_else(medicare_denom != 0, (medicare_acscd / medicare_denom)*1000, NA_real_))
dart_nyc <- mutate(dart_nyc, pcp_rt = if_else(medicare_denom != 0, (pcpvt_tot / medicare_denom)*1000, NA_real_))
dart_nyc <- mutate(dart_nyc, ed_rt = if_else(medicare_denom !=0, (edperday / medicare_denom)*1000, NA_real_))

summedicare <- sum(dart_nyc$medicare_denom[!is.na(dart_nyc$acscd_rt)], na.rm=T)

vsttable <- 
  dart_nyc %>%
  group_by(gent_status) %>%
  summarise(acscd_rt = sum(medicare_acscd[!is.na(acscd_rt)], na.rm=T) / sum(medicare_denom[!is.na(acscd_rt)], na.rm=T) * 1000,
            pcp_rt = sum(pcpvt_tot[!is.na(pcp_rt)], na.rm = T) / sum(medicare_denom[!is.na(pcp_rt)], na.rm=T) * 1000,
            ed_rt = sum(edperday[!is.na(ed_rt)], na.rm= T / sum(medicare_denom)[!is.na(ed_rt)], na.rm=T) * 1000) %>%
  mutate_if(is.numeric, funs(round(., digits = 2)))
```

``` r
# vsttb <- names(vsttable)
# dput(vsttb)
order_vec <- c("gent_status", "acscd_rt", "pcp_rt", "ed_rt")

vsttable %>% 
  gather("var", "value", -gent_status) %>% 
  spread(gent_status, value) %>% 
  mutate(var = ordered(var, levels = order_vec)) %>% 
  arrange(var) %>% 
  kable()
```

| var       |  Gentrifying|  High Income|  Non-Gentrifying|
|:----------|------------:|------------:|----------------:|
| acscd\_rt |        82.84|  5.97400e+01|            80.31|
| pcp\_rt   |      3318.17|  3.04389e+03|          4905.32|
| ed\_rt    |  37324000.00|  1.62250e+08|      32848000.00|

``` r
# foo <- names(sumtable)
# dput(foo)
order_vec <- c("gent", "physicians_rt", "phys_ci", "pa_rt", "pa_ci", "np_rt", 
"np_ci", "cnm_rt", "cnm_ci", "allpcp_rt", "allpcp_ci", "specs_rt", 
"specs_ci", "obstets_rt", "obstets_ci", "img_rt", "img_ci", "acscd_rt") 

sumtable %>% 
  gather("var", "value", -gent_status) %>% 
  spread(gent_status, value) %>% 
  mutate(var = ordered(var, levels = order_vec)) %>% 
  arrange(var) %>% 
  kable()
```

| var            | Gentrifying | High Income | Non-Gentrifying |
|:---------------|:------------|:------------|:----------------|
| physicians\_rt | 2.72        | 4.71        | 2.95            |
| phys\_ci       | 2.64 , 2.8  | 4.65 , 4.77 | 2.85 , 3.05     |
| pa\_rt         | 2.17        | 4.2         | 3.41            |
| pa\_ci         | 1.95 , 2.39 | 4.03 , 4.38 | 3.08 , 3.74     |
| np\_rt         | 8.63        | 11.78       | 7.64            |
| np\_ci         | 7.24 , 10   | 10.9 , 12.7 | 6.07 , 9.21     |
| cnm\_rt        | 2.98        | 2.63        | 2.72            |
| cnm\_ci        | 2.39 , 3.57 | 2.31 , 2.95 | 2.04 , 3.4      |
| allpcp\_rt     | 1.06        | 1.38        | 1.09            |
| allpcp\_ci     | 1.01 , 1.1  | 1.35 , 1.41 | 1.03 , 1.15     |
| specs\_rt      | 1.29        | 2.87        | 1.57            |
| specs\_ci      | 1.24 , 1.35 | 2.83 , 2.92 | 1.5 , 1.64      |
| obstets\_rt    | 9.32        | 15.46       | 9.45            |
| obstets\_ci    | 8.28 , 10.4 | 14.7 , 16.2 | 8.18 , 10.7     |
| img\_rt        | 1.1         | 1.53        | 1.34            |
| img\_ci        | 1.05 , 1.15 | 1.5 , 1.56  | 1.27 , 1.4      |
