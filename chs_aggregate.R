# Need dev version for summarise_at()
# devtools::install_github("gergness/srvyr")

library(tidyverse)
library(feather)
library(stringr)
library(srvyr)

get_srvy_means <- function(data) {
  data %>% 
    as_survey_design(strata = strata, weights = wt) %>% 
    group_by(uhf34, year) %>% 
    summarise_at(vars(-wt, -strata), funs(survey_mean), na.rm = TRUE)
}

# 2003 --------------------------------------------------------------------

chs03 <- read_feather("../dropbox/capstone/data_raw/chs2003.feather") %>% 
  transmute(uhf34 = uhf34,
            year = 2003L,
            wt = wt3,
            strata = strata,
            good_health = generalhealth <= 2, # (excelent & very good)
            gen_health = recode(generalhealth, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), 
            # no_care = didntseedr == 1, # this is didn't see doc b/c of COST
            has_pcp = pcp == 1,
            insured = insured == 1,
            age65p = agegroup == 4,
            le200pov = newpovgrps <= 2,
            pov = newpovgrps == 1,
            wht = newrace == 1,
            blk = newrace == 2,
            his = newrace == 3,
            asn = newrace == 4,
            forborn = usborn == 2)

chs03_all <- chs03 %>% 
  select(-le200pov) %>% 
  get_srvy_means()

chs03_200pov <- chs03 %>% 
  filter(le200pov == TRUE) %>% 
  select(-le200pov) %>% 
  get_srvy_means()


# 2009 --------------------------------------------------------------------

chs09 <- read_feather("../dropbox/capstone/data_raw/chs2009.feather") %>% 
  transmute(uhf34 = uhf34,
            year = 2009L,
            wt = wt10_dual,
            strata = strata,
            good_health = generalhealth <= 2, # (excelent & very good)
            gen_health = recode(generalhealth, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1), 
            no_care = didntgetcare09 == 1, # this is didn't get care for any reason (more than just doc visit)
            has_pcp = pcp09 == 1,
            insured = insured == 1,
            age65p = agegroup == 4,
            le200pov = newpovgrps <= 2,
            pov = newpovgrps == 1,
            wht = newrace == 1,
            blk = newrace == 2,
            his = newrace == 3,
            asn = newrace == 4,
            forborn = usborn == 2)

chs09_all <- chs09 %>% 
  select(-le200pov) %>% 
  get_srvy_means()

chs09_200pov <- chs09 %>% 
  filter(le200pov == TRUE) %>% 
  select(-le200pov) %>% 
  get_srvy_means()


# Combine Years -----------------------------------------------------------

uhf34_gent_status <- read_feather("../Dropbox/capstone/data_inter/uhf34_gent_status.feather") %>% select(-uhf34)

# combine years, calc lag and change vars, merge in gentrification status, order vars
finalize_chs <- function(datasets = list()) {
  bind_rows(datasets) %>% 
    select(-matches("_se$")) %>% 
    group_by(uhf34) %>% 
    arrange(uhf34, year) %>% 
    mutate_at(vars(-uhf34, -year), funs(lag = lag(.))) %>% 
    mutate_at(vars(-uhf34, -year, -matches("_lag$")), funs(chg = . - lag(.))) %>% 
    filter(year == 2009) %>% 
    select(-no_care_chg, -no_care_lag) %>% 
    left_join(uhf34_gent_status, by = c("uhf34" = "chs_uhf34")) %>% 
    select(uhf34, uhf34_name, year, gent, nongent, hiinc, everything())
}

chs_0309_all <- finalize_chs(list(chs03_all, chs09_all))
chs_0309_200pov <- finalize_chs(list(chs03_200pov, chs09_200pov))


write_feather(chs_0309_all, "../dropbox/capstone/data_clean/chs_uhf34_0309_all.feather")
haven::write_dta(chs_0309_all, "../dropbox/capstone/data_clean/chs_uhf34_0309_all.dta")
write_feather(chs_0309_200pov, "../dropbox/capstone/data_clean/chs_uhf34_0309_200pov.feather")
