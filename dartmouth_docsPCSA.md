Dartmouth\_docsPCSA
================
2017-02-18

``` r
library(stringr); library(foreign); library(QuantPsyc); library(psych); library(knitr); library(tidyverse)
```

``` r
# 2010 raw data
dart_raw <- read.dbf("../dropbox/capstone/2010 data/p_103113_1.dbf", as.is = TRUE)
names(dart_raw) <- names(dart_raw) %>% str_to_lower()

dart_raw2 <- read.dbf("../dropbox/capstone/2010 data/p_103113_2.dbf", as.is = T)
names(dart_raw2) <- names(dart_raw2) %>% str_to_lower()

dart_np <- read.dbf("../dropbox/capstone/2010 data/p_cnm_np_122013.dbf", as.is = T)
names(dart_np) <- names(dart_np) %>% str_to_lower()
```

``` r
# limit to NYC PCSAs

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
gentxwalk$gent_status <- ordered(gentxwalk$gent_status, levels = c("Non-Gentrifying", "Gentrifying", "High Income"))
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
    pcpdenom = pvden_10, # denom for PCP rate
    totpop = page00_14 + page15_64 + page65_up, 
    ed_crudert = pcr_ed10,
    hospdenom = phden_10
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
# functions for CIs
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
| Non-Gentrifying |            2.95| 2.85 , 3.05 |    3.41| 3.08 , 3.74 |    7.64| 6.07 , 9.21 |     2.72| 2.04 , 3.4  |        1.09| 1.03 , 1.15 |       1.57| 1.5 , 1.64  |         9.45| 8.18 , 10.7 |     1.34| 1.27 , 1.4  |
| Gentrifying     |            2.72| 2.64 , 2.8  |    2.17| 1.95 , 2.39 |    8.63| 7.24 , 10   |     2.98| 2.39 , 3.57 |        1.06| 1.01 , 1.1  |       1.29| 1.24 , 1.35 |         9.32| 8.28 , 10.4 |     1.10| 1.05 , 1.15 |
| High Income     |            4.71| 4.65 , 4.77 |    4.20| 4.03 , 4.38 |   11.78| 10.9 , 12.7 |     2.63| 2.31 , 2.95 |        1.38| 1.35 , 1.41 |       2.87| 2.83 , 2.92 |        15.46| 14.7 , 16.2 |     1.53| 1.5 , 1.56  |

``` r
dart_nyc <- mutate(dart_nyc, acscd_rt = if_else(medicare_denom != 0, (medicare_acscd / medicare_denom), NA_real_))
dart_nyc <- mutate(dart_nyc, pcp_rt = if_else(pcpdenom != 0, (pcpvt_tot / pcpdenom), NA_real_))
dart_nyc <- mutate(dart_nyc, ed_rt = if_else(hospdenom !=0, (edperday / hospdenom), NA_real_))

summedicare <- sum(dart_nyc$medicare_denom[!is.na(dart_nyc$acscd_rt)], na.rm=T)

vsttable <- 
  dart_nyc %>%
  group_by(gent_status) %>%
  summarise(acscd_rt = sum(medicare_acscd[!is.na(acscd_rt)], na.rm=T) / sum(medicare_denom[!is.na(acscd_rt)], na.rm=T) * 1000,
            pcp_rt = sum(pcpvt_tot[!is.na(pcp_rt)], na.rm = T) / sum(pcpdenom[!is.na(pcp_rt)], na.rm=T) * 100, 
            ed_rt = sum(edperday[!is.na(ed_rt)], na.rm= T / sum(medicare_denom)[!is.na(ed_rt)], na.rm=T) * 1000) %>%
  mutate_if(is.numeric, funs(round(., digits = 2)))
## ED RT is not tallying with crude rate from dartmouth data
```

``` r
order_vec <- c("gent_status", "acscd_rt", "pcp_rt", "ed_rt")

options(scipen=999)

vsttable %>% 
  gather("var", "value", -gent_status) %>% 
  spread(gent_status, value) %>% 
  mutate(var = ordered(var, levels = order_vec)) %>% 
  arrange(var) %>% 
  kable()
```

| var       |  Non-Gentrifying|  Gentrifying|   High Income|
|:----------|----------------:|------------:|-------------:|
| acscd\_rt |            80.31|        82.84|         59.74|
| pcp\_rt   |           590.41|       404.61|        365.32|
| ed\_rt    |      32848000.00|  37324000.00|  162250000.00|

``` r
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

| var            | Non-Gentrifying | Gentrifying | High Income |
|:---------------|:----------------|:------------|:------------|
| physicians\_rt | 2.95            | 2.72        | 4.71        |
| phys\_ci       | 2.85 , 3.05     | 2.64 , 2.8  | 4.65 , 4.77 |
| pa\_rt         | 3.41            | 2.17        | 4.2         |
| pa\_ci         | 3.08 , 3.74     | 1.95 , 2.39 | 4.03 , 4.38 |
| np\_rt         | 7.64            | 8.63        | 11.78       |
| np\_ci         | 6.07 , 9.21     | 7.24 , 10   | 10.9 , 12.7 |
| cnm\_rt        | 2.72            | 2.98        | 2.63        |
| cnm\_ci        | 2.04 , 3.4      | 2.39 , 3.57 | 2.31 , 2.95 |
| allpcp\_rt     | 1.09            | 1.06        | 1.38        |
| allpcp\_ci     | 1.03 , 1.15     | 1.01 , 1.1  | 1.35 , 1.41 |
| specs\_rt      | 1.57            | 1.29        | 2.87        |
| specs\_ci      | 1.5 , 1.64      | 1.24 , 1.35 | 2.83 , 2.92 |
| obstets\_rt    | 9.45            | 9.32        | 15.46       |
| obstets\_ci    | 8.18 , 10.7     | 8.28 , 10.4 | 14.7 , 16.2 |
| img\_rt        | 1.34            | 1.1         | 1.53        |
| img\_ci        | 1.27 , 1.4      | 1.05 , 1.15 | 1.5 , 1.56  |

``` r
## bring in 99 data
raw99 <- read.csv("../dropbox/capstone/zcta99_pcsa2010.csv", as.is = TRUE)
names(raw99) <- names(raw99) %>% str_to_lower()

cms_pcsa <-read.csv("../dropbox/capstone/cms99_pcsa2010.csv", as.is = T)
names(cms_pcsa) <- names(cms_pcsa) %>% str_to_lower()
```

``` r
cms_pcsa <-
  cms_pcsa %>%
  transmute(pcsa = pcsa2010, 
            pcpvt = zvt_pc99, #part B & outpatient file
            ambvt = zvt_am99, #part B & outpatient file
            edvt = zvt_ed99, #total #ED visits per bene per day by Med Part B bene resident in ZCTA
            pcpdenom = zvden_99,
            eddenom = zhden_99,
            totbene = zbene_n)

nyc99 <-
  raw99 %>%
  transmute(pcsa = pcsa2010,
            pcp = zg_doc,
            specialist = zs_doc,
            obgyn = zo_doc,
            nonfedpcp = zg_nfeddoc,
            nonfedspec = zs_nfeddoc,
            nonfedob = zo_nfeddoc,
            img_pcp = zg_img,
            img_spec = zs_img,
            img_ob = zo_img,
            pa_tot = zpa_tot,
            pa_pcp = zpa_pc,
            pa_spec = zpa_spec,
            pa_ob = zpa_obgyn,
            pa_oth = zpa_oth,
            fqhcs = zfqhc,
            fem15_44 = zp012030 + zp012031 + zp012032 + zp012033 + zp012034 + zp012035 + zp012036 + zp012037 + zp012038,
            totpop = zage00_14 + zage15_64 + zage65_up) %>%
  inner_join(cms_pcsa, by = "pcsa") %>%
  inner_join(gentxwalk, by = "pcsa")
```

``` r
nyc99$physicians <- nyc99$pcp + nyc99$specialist + nyc99$obgyn
nyc99$obstets <- nyc99$obgyn + nyc99$pa_ob
nyc99$allpcp <- nyc99$pcp + nyc99$pa_pcp
nyc99$specs <- nyc99$specialist + nyc99$pa_spec + nyc99$pa_oth
nyc99$allimg <- nyc99$img_ob + nyc99$img_pcp + nyc99$img_spec
```

``` r
sumtable99 <- 
  nyc99 %>%
  group_by(gent_status) %>%
  summarise(physicians_rt = sum(physicians) / sum(totpop)*1000,
            phys_ci = ci(1000, totpop, physicians),
            pa_rt = sum(pa_tot) / sum(totpop)*10000,
            pa_ci = ci(10000, totpop, pa_tot),
            allpcp_rt = sum(allpcp) / sum(totpop)*1000,
            allpcp_ci = ci(1000, totpop, allpcp), 
            specs_rt = sum(specs) / sum(totpop)*1000,
            specs_ci = ci(1000, totpop, specs),
            obstets_rt = sum(obstets) / sum(fem15_44)*10000,
            obstets_ci = ci(10000, fem15_44, obstets),
            img_rt = sum(allimg) / sum(totpop)*1000,
            img_ci = ci(1000, totpop, allimg)) %>%
    mutate_if(is.numeric, funs(round(., digits = 2)))

kable(sumtable99)
```

| gent\_status    |  physicians\_rt| phys\_ci    |  pa\_rt| pa\_ci      |  allpcp\_rt| allpcp\_ci    |  specs\_rt| specs\_ci   |  obstets\_rt| obstets\_ci |  img\_rt| img\_ci       |
|:----------------|---------------:|:------------|-------:|:------------|-----------:|:--------------|----------:|:------------|------------:|:------------|--------:|:--------------|
| Non-Gentrifying |            1.73| 1.65 , 1.8  |    2.33| 2.05 , 2.6  |        0.68| 0.637 , 0.731 |       1.19| 1.13 , 1.25 |         3.74| 3.04 , 4.45 |     0.99| 0.933 , 1.05  |
| Gentrifying     |            1.61| 1.55 , 1.68 |    1.83| 1.63 , 2.04 |        0.62| 0.586 , 0.663 |       1.08| 1.03 , 1.13 |         3.70| 3.11 , 4.3  |     0.87| 0.824 , 0.914 |
| High Income     |            3.84| 3.79 , 3.89 |    3.18| 3.03 , 3.33 |        1.12| 1.09 , 1.15   |       2.81| 2.77 , 2.86 |         9.65| 9.1 , 10.2  |     1.63| 1.6 , 1.67    |

``` r
order_vec <- c("gent_status", "physicians_rt", "phys_ci", "pa_rt", "pa_ci", "allpcp_rt", "allpcp_ci", "specs_rt", "specs_ci", "obstets_rt", "obstets_ci", "img_rt", "img_ci", "acscd_rt") 

sumtable99 %>% 
  gather("var", "value", -gent_status) %>% 
  spread(gent_status, value) %>% 
  mutate(var = ordered(var, levels = order_vec)) %>% 
  arrange(var) %>% 
  kable()
```

| var            | Non-Gentrifying | Gentrifying   | High Income |
|:---------------|:----------------|:--------------|:------------|
| physicians\_rt | 1.73            | 1.61          | 3.84        |
| phys\_ci       | 1.65 , 1.8      | 1.55 , 1.68   | 3.79 , 3.89 |
| pa\_rt         | 2.33            | 1.83          | 3.18        |
| pa\_ci         | 2.05 , 2.6      | 1.63 , 2.04   | 3.03 , 3.33 |
| allpcp\_rt     | 0.68            | 0.62          | 1.12        |
| allpcp\_ci     | 0.637 , 0.731   | 0.586 , 0.663 | 1.09 , 1.15 |
| specs\_rt      | 1.19            | 1.08          | 2.81        |
| specs\_ci      | 1.13 , 1.25     | 1.03 , 1.13   | 2.77 , 2.86 |
| obstets\_rt    | 3.74            | 3.7           | 9.65        |
| obstets\_ci    | 3.04 , 4.45     | 3.11 , 4.3    | 9.1 , 10.2  |
| img\_rt        | 0.99            | 0.87          | 1.63        |
| img\_ci        | 0.933 , 1.05    | 0.824 , 0.914 | 1.6 , 1.67  |

``` r
nyc99 <- mutate(nyc99, pcp_rt = if_else(pcpdenom != 0, (pcpvt / pcpdenom), NA_real_))
nyc99 <- mutate(nyc99, ed_rt = if_else(eddenom != 0, (edvt /eddenom), NA_real_))


vsttable99 <- 
  nyc99 %>%
  group_by(gent_status) %>%
  summarise(pcp_rt = sum(pcpvt[!is.na(pcp_rt)], na.rm = T) / sum(pcpdenom[!is.na(pcp_rt)], na.rm=T) * 100,
ed_rt = sum(edvt[!is.na(ed_rt)], na.rm= T / sum(eddenom)[!is.na(ed_rt)], na.rm=T) *1000) %>%
  mutate_if(is.numeric, funs(round(., digits = 2)))


order_vec <- c("gent_status", "pcp_rt", "ed_rt")

vsttable99 %>% 
  gather("var", "value", -gent_status) %>% 
  spread(gent_status, value) %>% 
  mutate(var = ordered(var, levels = order_vec)) %>% 
  arrange(var) %>% 
  kable()          
```

| var     |  Non-Gentrifying|  Gentrifying|   High Income|
|:--------|----------------:|------------:|-------------:|
| pcp\_rt |           295.65|       267.97|        266.64|
| ed\_rt  |      30182600.00|  38216450.00|  146563500.00|
