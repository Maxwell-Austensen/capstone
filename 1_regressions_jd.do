**** changes dataset ******


/*
LHS:
Primary Care Physicians log_pcp_p1000
International Docs Log_img_p1000
OBGYNs log_ob_p1000
Specialists  log_sp_p1000
All pcp (includes non-physicians, al providers) ch_allpcp_p1000_2010 
*/


global capstone "/Users/amy/Dropbox/1. NYU Wagner/Spring 2017/capstone1/capstone/data_clean"
global tables "/Users/amy/Dropbox/1. NYU Wagner/Spring 2017/capstone1/capstone/Tables"
global graphs "/Users/amy/Dropbox/1. NYU Wagner/Spring 2017/capstone1/graphs"
clear all 


cd "$capstone"
u all_data

*summary statistics
tabstat sh_lt5_2010 sh_5_17_2010 sh_20_34_2010 sh_35_54_2010 sh_55p_2010, by(gent_status)

tabstat sh_nohs_ed_2010 sh_hs_ed_2010 sh_col_ed_2010, by(gent_status)

tabstat sh_renter_2010 sh_rent_burd_2010 sh_sev_rent_burd_2010 sh_rent_vac_2010 sh_sev_crowd_2010, by(gent_status) 

tabstat avg_inc_adj_2010 sh_hh_ssinc_2010 avg_rent_adj_2010 sh_pov_2010, by(gent_status)

tabstat ch_pcphys_p1000_2010 ch_img_p1000_2010 ch_obgyn_p1000_2010 ch_specs_p1000_2010 ch_allpcp_p1000_2010, by(gent_status)

tabstat allpcp_p1000_2010 img_p1000_2010 obgyn_p1000_2010 specs_p1000_2010 pcphys_p1000_2010, by(gent_status)

tabstat sh_blk_2010 sh_hisp_2010 sh_wht_2010 sh_asian_2010, by(gent_status)

tabstat sh_forborn_2010, by(gent_status)


* histograms 

cd "$graphs"

hist ch_pcphys_p1000_2010, normal xtitle("Change in number of pc phys per 1000 (2000-2010)")  color(ltblue)
graph export "ch_phys_p1000.png", replace 

hist pcphys_p1000_2000, normal xtitle("Number of pc phys per 1000, 2010")  color(ltblue)
graph export "phys_p1000.png", replace 

*all pcp
hist ch_allpcp_p1000_2010, normal xtitle("Change in number of primary care providers per 1000 (2000-2010)")  color(ltblue)
graph export "ch_pcp_p1000.png", replace

hist allpcp_p1000_2010, normal xtitle("Distribution of PCPs per 1000 (2010)")  color(ltblue)
graph export "pcp_p1000.png", replace 

*obstets

hist obgyn_p1000_2010, normal xtitle("Change in number of obstetrics providers per 1000 (2000-2010)")  color(ltblue)
graph export "ch_obst_p1000.png", replace

*int'l
hist img_p1000_2010, normal xtitle("Change in IMG per 1000 (2000-2010)")  color(ltblue)
graph export "ch_img_p1000.png", replace

*specialists
hist specs_p1000_2010, normal xtitle("Change in number of specialists per 1000 (2000-2010)")  color(ltblue)
graph export "ch_spec_p1000.png", replace 



** regressions 
hist allpcp_p1000_2010
gen log_pcp_p1000 = log(allpcp_p1000_2010)

hist img_p1000_2010
gen log_img_p1000 = log(img_p1000_2010)

*USE OBGYNS, NOT OBSTETS
hist obgyn_p1000_2010
gen log_ob_p1000 = log(obgyn_p1000_2010)

hist specs_p1000_2010
gen log_sp_p1000 = log(specs_p1000_2010)

hist pcphys_p1000_2000
gen log_phys_p1000 = log(pcphys_p1000_2010)



* 2010 levels

/*
Covariates: 
n_facilities (only hospitals 1,0) *don’t need cite
avg_inc_adj_2010 
sh_forborn_2010 *need cite 
sh_blk_2010 * race - need cite
sh_hisp_2010 
sh_asian
Poverty sh_pov_2010 *need cite-- Gopal
Age sh_55p_2010 *need cite--Gopal
*/



* 2010 cross section levels: only gentrification var. 
cd "$tables"
local replace replace 
foreach v in log_pcp_p1000 log_img_p1000 log_ob_p1000 log_sp_p1000 log_phys_p1000{
reg `v' ib2.gent_status, robust
outreg2 using "gent_only.docx", `replace'
local replace append 
}

 

 
 *2010 cross section in levels, with covariates
cd "$tables"
local replace replace
foreach v in log_pcp_p1000 log_img_p1000 log_ob_p1000 log_sp_p1000 log_phys_p1000 {
reg `v' ib2.gent_status hospital avg_inc_adj_2010 sh_forborn_2010 sh_blk_2010 sh_hisp_2010 sh_asian_2010 sh_pov_2010 sh_55p_2010, robust
outreg2 using "2010docs_levels.docx", `replace'
local replace append
}
 
 
 
 
 
 
 *2010 cross section in levels, with covariates w/o income 
cd "$tables"
local replace replace
foreach v in log_pcp_p1000 log_img_p1000 log_ob_p1000 log_sp_p1000 log_phys_p1000 {
reg `v' ib2.gent_status hospital sh_forborn_2010 sh_blk_2010 sh_hisp_2010 sh_asian_2010 sh_pov_2010 sh_55p_2010, robust
outreg2 using "2010docs_levels_noinc.docx", `replace'
local replace append
}


* poisson regressions: 


cd "$tables"
local replace replace
foreach v in allpcp_p1000_2010 img_p1000_2010 obgyn_p1000_2010 specs_p1000_2010 pcphys_p1000_2010 {
	g `v' ib2.gent_status hospital avg_inc_adj_2010 sh_forborn_2010 sh_blk_2010 sh_hisp_2010 sh_asian_2010 sh_pov_2010 sh_55p_2010, robust
	outreg2 using "poisson_2010.docx", `replace'
local replace append
}


/* poisson w/ glm to check dispersion add "scale(x2)" to end of code to correct the SEs for each one - would also 
correct the slight overdispersion for specs
*/
foreach v in allpcp_p1000_2010 img_p1000_2010 obgyn_p1000_2010 specs_p1000_2010 pcphys_p1000_2010 {
	glm `v' ib2.gent_status hospital avg_inc_adj_2010 sh_forborn_2010 sh_blk_2010 sh_hisp_2010 sh_asian_2010 sh_pov_2010 sh_55p_2010, family(poisson) link(log)
}




* changes regressions: 
clear all 
cd "$capstone"
u all_data
cd "$tables"
local replace replace
foreach v in ch_allpcp_p1000_2010 ch_obgyn_p1000_2010 ch_img_p1000_2010 ch_pcphys_p1000_2010 ch_specs_p1000_2010 {
	reg `v' hospital ib2.gent_status ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_55p_2010, robust
outreg2 using "changes_docs.docx", `replace'
local replace append
}


*ambulatory sensitive conditions, primary care visit rates & ED visit rates - 2010 cross section 
clear all 
cd "$capstone"
u all_data
cd "$tables"
local replace replace
foreach v in acscd_rt_2010 edvt_rt_2010 pcpvt_rt_2010 {
	reg `v' ib2.gent_status hospital avg_inc_adj_2010 sh_forborn_2010 sh_blk_2010 sh_hisp_2010 sh_asian_2010 sh_pov_2010 sh_55p_2010, robust
	outreg2 using "acs_pc_ed_results.docx", `replace'
local replace append
}

*Run regression for primary care visit rates & ED visit rates, changes 2000 - 2010 
clear all 
cd "$capstone"
u all_data
cd "$tables"
local replace replace
foreach v in ch_edvt_rt_2010 ch_pcpvt_rt_2010 {
	reg `v'  ib2.gent_status ch_avg_inc_adj_2010 ch_sh_blk_2010 ch_sh_hisp_2010 ch_sh_forborn_2010 ch_sh_pov_2010 ch_sh_55p_2010, robust
	outreg2 using "ch_pc_ed_results.docx", `replace'
local replace append
}




/*		
* run dif in dif
* regress on treatment and interaction of treatment and indep var. 
* Keep only vars for regression and reshape. 

clear all 
cd "$capstone"
u all_data

keep zcta2010 gent_status allpcp_p1000* img_p1000* obgyn_p1000* specs_p1000* pcphys_p1000* avg_inc_adj_* sh_forborn_* sh_blk_* sh_hisp_* sh_asian_* sh_pov_* sh_55p_*
drop *1990

reshape long allpcp_p1000_ img_p1000_ obgyn_p1000_ specs_p1000_ pcphys_p1000_ avg_inc_adj_ sh_forborn_ sh_blk_ sh_hisp_ sh_asian_ sh_pov_ sh_55p_, i(zcta2010) j(year)
*limit to only tracts that were initially low income
drop if gent_status==3
replace gent_status=1 if year==2000 
gen gent_year = gent_status*year
save diff_vars, replace 


* diff in diff: regress doc level on time, treatment, and time * treat
cd "/Users/amy/Dropbox/1. NYU Wagner/Fall 2016/capstone1/capstone/Tables"
local replace replace
foreach v in specs_p1000_ pcphys_p1000_ obgyn_p1000_ img_p1000_ allpcp_p1000_{
reg `v' gent_status year gent_year, robust
outreg2 using "diff.docx", `replace' 
local replace append
}
*/





