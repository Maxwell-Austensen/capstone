**************
**** CHS Descriptive statistics
* Author: A. Ganz 
* project: Capstone

global data "/Users/amy/Dropbox/1. NYU Wagner/Spring 2017/capstone1/capstone/data_clean/"
cd "$data"

u "chs_uhf34_0309_all.dta"

sum gent - no_care
corr gent nongent hiinc pov 
corr gent nongent good_health gen_health has_pcp insured no_care
corr wht blk his asn forborn age65p 
