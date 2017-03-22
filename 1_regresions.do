**** changes dataset ******

global capstone "/Users/amy/Dropbox/1. NYU Wagner/Fall 2016/capstone1/capstone"
 
cd "$capstone"

clear all 

import delimited using "health_facilities.csv"
drop pcsa_name 
save healthfacilities, replace 


merge 1:1 pcsa using dall
save dall, replace 


drop ch_allimg_2010-ch2_sh_wht_2010

tabstat sh_lt5_2000 sh_5_17_2000 sh_20_34_2000 sh_35_54_2000 sh_55p_2000, by(gent_status)

*summary statistics
tabstat sh_lt5_2010 sh_5_17_2010 sh_20_34_2010 sh_35_54_2010 sh_55p_2010, by(gent_status)

tabstat sh_nohs_ed_2010 sh_hs_ed_2010  sh_col_ed_2010, by(gent_status)

tabstat sh_rent_burd_2010 sh_sev_rent_burd_2010 sh_rent_vac_2010 sh_sev_crowd_2010  sh_sev_crowd_2010 sh_renter_2010, by(gent_status) 

tabstat avg_inc_adj_2010 sh_hh_ssinc_2010 avg_rent_adj_2010 sh_pov_2010, by(gent_status)

tabstat ch_allpcp_p1000_2010 ch_img_p1000_2010 ch_obstets_p1000_2010 ch_pa_p1000_2010 ch_phys_p1000_2010 ch_specs_p1000_2010, by(gent_status)

tabstat allpcp_p1000_2010 img_p1000_2010 obstets_p1000_2010 phys_p1000_2010 specs_p1000_2010 np_p1000_2010 pa_p1000_2010, by(gent_status)

cd "/Users/amy/Dropbox/1. NYU Wagner/Fall 2016/capstone1/graphs"


* histograms 

hist ch_phys_p1000_2010, normal xtitle("Change in number of physicians per 1000 (2000-2010)")  color(ltblue)
graph export "phys_p1000.png", replace 

hist ch_allpcp_p1000_2010, normal xtitle("Change in number of primary care providers per 1000 (2000-2010)")  color(ltblue)
graph export "ch_pcp_p1000.png", replace

hist ch_obstets_p1000_2010, normal xtitle("Change in number of obstetrics providers per 1000 (2000-2010)")  color(ltblue)
graph export "ch_obst_p1000.png", replace

hist ch_img_p1000_2010, normal xtitle("Change in IMG per 1000 (2000-2010)")  color(ltblue)
graph export "ch_img_p1000.png", replace

hist ch_pa_p1000_2010, normal xtitle("Change in number of physicians assistants per 1000 (2000-2010)")  color(ltblue)
graph export "ch_pa_p1000.png", replace 

hist ch_specs_p1000_2010, normal xtitle("Change in number of specialists per 1000 (2000-2010)")  color(ltblue)
graph export "ch_spec_p1000.png", replace 


hist allpcp_p1000_2010, normal xtitle("Distribution of PCPs per 1000 (2010)")  color(ltblue)
graph export "pcp_p1000.png", replace 


** regressions 
hist allpcp_p1000_2010
gen log_pcp_p1000 = log(allpcp_p1000_2010)

hist img_p1000_2010
gen log_img_p1000 = log(img_p1000_2010)

hist obstets_p1000_2010
gen log_ob_p1000 = log(obstets_p1000_2010)

hist specs_p1000_2010
gen log_sp_p1000 = log(specs_p1000_2010)

encode(gent_status), generate(gent_num)
save dall, replace 

cd "$capstone"
drop if gent_status=="High Income"

save gent_only, replace 

* 2010 levels

/*
reg allpcp_p1000_2010 i.gent_num
reg allpcp_p1000_2010 avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010
reg allpcp_p1000_2010 i.gent_num avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010
*/

* all pcps
clear all 

u gent_only 

reg log_pcp_p1000 i.gent_num
outreg2 using "2010levels_pcp.docx", replace 

reg log_pcp_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_pcp.docx", append 

reg log_pcp_p1000 i.gent_num n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_pcp.docx", append


clear all 
u dall

reg log_pcp_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_pcp.docx", append

*IMG providers
clear all 

u gent_only 

reg log_img_p1000 i.gent_num
outreg2 using "2010levels_img.docx", replace 

reg log_img_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_img.docx", append 

reg log_img_p1000 i.gent_num n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_img.docx", append

clear all 
u dall

reg log_img_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_img.docx", append



*ob's

clear all 
u gent_only 

reg log_ob_p1000 i.gent_num
outreg2 using "2010levels_ob.docx", replace 

reg log_ob_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_ob.docx", append 

reg log_ob_p1000 i.gent_num n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_ob.docx", append

clear all 
u dall

reg log_ob_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_ob.docx", append

*specialists


clear all 
u gent_only 

reg log_sp_p1000 i.gent_num
outreg2 using "2010levels_spec.docx", replace 

reg log_sp_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_spec.docx", append 

reg log_sp_p1000 i.gent_num n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_spec.docx", append

clear all 
u dall

reg log_sp_p1000 n_facilities avg_inc_adj_2010 sh_blk_2010 sh_col_ed_2010 sh_forborn_2010 sh_hisp_2010 sh_pov_2010 sh_renter_2010 sh_55p_2010
outreg2 using "2010levels_spec.docx", append


* changes in pcps

clear all 
u gent_only

reg ch_allpcp_p1000_2010 i.gent_num
outreg2 using "2010chg_pcp.docx", replace 

reg ch_allpcp_p1000_2010 i.gent_num ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_pcp.docx", append

clear all 
u dall 

reg ch_allpcp_p1000_2010 ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_pcp.docx", append

* changes in img
clear all 
u gent_only

reg ch_img_p1000_2010 i.gent_num
outreg2 using "2010chg_img.docx", replace 

reg ch_img_p1000_2010 i.gent_num ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_img.docx", append

clear all 
u dall 

reg ch_img_p1000_2010 ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_img.docx", append



* changes in ob's
clear all 
u gent_only

reg ch_obstets_p1000_2010 i.gent_num
outreg2 using "2010chg_ob.docx", replace 

reg ch_obstets_p1000_2010 i.gent_num ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_ob.docx", append

clear all 
u dall 

reg ch_obstets_p1000_2010 ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_ob.docx", append

*changes in specialists
clear all 
u gent_only

reg ch_specs_p1000_2010 i.gent_num
outreg2 using "2010chg_sp.docx", replace 

reg ch_specs_p1000_2010 i.gent_num ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_sp.docx", append

clear all 
u dall 

reg ch_specs_p1000_2010 ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_col_ed_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_renter_2010 ch_sh_55p_2010
outreg2 using "2010chg_sp.docx", append






















