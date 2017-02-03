*******************
*******************
* Author: A. Ganz
* Project: Capstone project/ block overlay for crosswalk
* Description: the input is a file that contains the overlay (union) of 2000 
* 	census blocks, 2010 pcsa's, and 2000 zctas. This script assigns one zcta and 
*	pcsa per census block, which is determined by the size of overlap. 
* input: block_pcsa block_zcta
* output: blk_pcsa_zcta_xwalk.csv
*******************
*******************

clear all 
set more off, perm
*global data "C:\Users\alg638\Documents\capstone"
global dir "/Users/amy/Dropbox/1. NYU Wagner/Fall 2016/capstone1"
cd "$dir"


u block_pcsa 
rename *, lower

*drop census blocks outside of NYC
drop if fid_nycb20==-1

*calculate the total geographic size of the census block
bys fid_nycb20: egen block_area=sum(shape_area)

*calculate share of census block contained in pcsa 
gen pcsa_shr= shape_area/block_area

*calculate maximum share for each block
bys fid_nycb20: egen pcsa_max=max(pcsa_shr)
gen pcsa_max1=0
replace pcsa_max1=1 if pcsa_max==pcsa_shr

*identify pcsas with at least a majority of census block within them
gen pcsa_major=0
replace pcsa_major=1 if pcsa_shr>.5

*keep the pcsas with the most census block area. 
keep if pcsa_max1==1
drop pcsa_max1

keep bctcb2000 pcsa
save block_pcsa_clean, replace

******
*zcta's

clear all 

u block_zcta

rename *, lower

drop if fid_nycb20==-1

bys fid_nycb20: egen block_area=sum(shape_area)

gen zcta_shr= shape_area/block_area

bys fid_nycb20: egen zcta_max=max(zcta_shr)
gen zcta_max1=0
replace zcta_max1=1 if zcta_max==zcta_shr

gen zcta_major=0
replace zcta_major=1 if zcta_shr>.5

keep if zcta_max1==1
drop zcta_max1

keep bctcb2000 zcta

save block_zcta_clean, replace

********
*Merge together pcsa & zcta files

merge 1:1 bctcb2000 using block_pcsa_clean
drop _merge

save blk_pcsa_zcta_xwalk, replace

outsheet bctcb2000 zcta pcsa using blk_pcsa_zcta_xwalk.csv, comma






