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
            gen_health = generalhealth, 
            good_health = generalhealth <= 3,
            # no_care = didntseedr == 1, # this is didn't see doc b/c of COST
            has_pcp = pcp == 1,
            insured = insured == 1,
            age65p = agegroup == 4,
            pov = newpovgrps == 1,
            wht = newrace == 1,
            blk = newrace == 2,
            his = newrace == 3,
            asn = newrace == 4,
            forborn = usborn == 2) %>% 
  get_srvy_means()


# 2009 --------------------------------------------------------------------

chs09 <- read_feather("../dropbox/capstone/data_raw/chs2009.feather") %>% 
  transmute(uhf34 = uhf34,
            year = 2009L,
            wt = wt10_dual,
            strata = strata,
            gen_health = generalhealth, 
            good_health = generalhealth <= 3,
            no_care = didntgetcare09 == 1, # this is didn't get care for any reason (more than just doc visit)
            has_pcp = pcp09 == 1,
            insured = insured == 1,
            age65p = agegroup == 4,
            pov = newpovgrps == 1,
            wht = newrace == 1,
            blk = newrace == 2,
            his = newrace == 3,
            asn = newrace == 4,
            forborn = usborn == 2) %>% 
  get_srvy_means()



# Combine Years -----------------------------------------------------------

uhf34_gent_status <- read_feather("../Dropbox/capstone/data_inter/uhf34_gent_status.feather") %>% select(-uhf34)

chs_0309 <- bind_rows(chs03, chs09) %>% 
  select(-matches("_se$")) %>% 
  group_by(uhf34) %>% 
  arrange(uhf34, year) %>% 
  mutate_at(vars(-uhf34, -year), funs(chg = . - lag(.))) %>% 
  select(-no_care_chg) %>% 
  left_join(uhf34_gent_status, by = c("uhf34" = "chs_uhf34")) %>% 
  select(uhf34, uhf34_name, year, gent, nongent, hiinc, everything())

write_feather(chs_0309, "../dropbox/capstone/data_inter/chs_uhf34_0309.feather")
