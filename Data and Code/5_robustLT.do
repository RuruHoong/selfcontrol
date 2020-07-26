/******************************************************************************
Author: Ruru Hoong
File Name: 5_robustLT.do
Description: Regressions for Effects of App Intervention, Long Term (robustness to exclusion of manually overidden)
******************************************************************************/

use "applimit_intermediate.dta", clear
drop if _merge3==1
drop if _merge4==1
drop if manual4==1
gen id=_n

* Drop day 7 values 
drop q43_act2d_phone_7 q43_act3d_phone_7 q43_act4d_phone_7 q44_act2d_fb_7 q44_act3d_fb_7 q44_act4d_fb_7 q45_act2d_insta_7 q45_act3d_insta_7 q45_act4d_insta_7 
save "applimit_filtered.dta", replace

/* 1. Phone */
use "applimit_filtered.dta",replace
keep id q43_act2d_phone_* apptreat privtreat at_23 q15_col q13_enc q16_enc q3_ selfcontrols beta pmean2 at_23_4 
reshape long q43_act2d_phone_, i(id) j(day)
gen dow = day
rename q43 time
save "Merged/act2dmanipulatedbin.dta", replace

clear all
use "applimit_filtered.dta",replace
keep id q43_act3d_phone_* apptreat privtreat at_23 q15_col q13_enc q16_enc q3_ selfcontrols beta pmean2 at_23_4 
reshape long q43_act3d_phone_, i(id) j(day)
gen dow = day+7
rename q43 time
save "Merged/act3dmanipulatedbin.dta", replace

use "applimit_filtered.dta",replace
keep id q43_act4d_phone_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta pmean2 
reshape long q43_act4d_phone_, i(id) j(day)
gen dow = day+35
rename q43 time
save "Merged/act4dmanipulatedbin.dta", replace

use "Merged/act2dmanipulatedbin.dta"
append using "Merged/act3dmanipulatedbin.dta"
append using "Merged/act4dmanipulatedbin.dta"

* Generate Variables
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
replace at_23=at_23_4 if dow >14
gen logtime = log(time)
bysort id: gen countid = _n
replace countid=. if countid!=1


* Regressions, LT effects
*ITT
xtset id dow
xtreg time apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTrobustM.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobustM.xls", append
*ITT, logtime
xtreg logtime apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlogrobustM.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobustM.xls", append


/* 2. Facebook */
use "applimit_filtered.dta",replace
keep id q44_act2d_fb_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta  fbmean2 
reshape long q44_act2d_fb_, i(id) j(day)
gen dow = day
rename q44 time
save "Merged/fbact2dmanipulatedbin.dta", replace

clear all
use "applimit_filtered.dta",replace
keep id q44_act3d_fb_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta  fbmean2 
reshape long q44_act3d_fb_, i(id) j(day)
gen dow = day+7
rename q44 time
save "Merged/fbact3dmanipulatedbin.dta", replace

use "applimit_filtered.dta", replace
keep id q44_act4d_fb_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta  fbmean2 
reshape long q44_act4d_fb_, i(id) j(day)
gen dow = day+35
rename q44 time
save "Merged/fbact4dmanipulatedbin.dta", replace

use "Merged/fbact2dmanipulatedbin.dta", replace
append using "Merged/fbact3dmanipulatedbin.dta"
append using "Merged/fbact4dmanipulatedbin.dta"

* Generate Variables
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
replace at_23=at_23_4 if dow >14
gen logtime = log(time)
bysort id: gen countid = _n
replace countid=. if countid!=1

* Regressions, LT effects
*ITT
xtset id dow
xtreg time apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTrobustM.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobustM.xls", append
*ITT, logtime
xtreg logtime apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlogrobustM.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobustM.xls", append


/* 3. Instagram */
use "applimit_filtered.dta",replace
keep id q45_act2d_insta_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta  instamean2 
reshape long q45_act2d_insta_, i(id) j(day)
gen dow = day
rename q45 time
save "Merged/instaact2dmanipulatedbin.dta", replace

clear all
use "applimit_filtered.dta"
keep id q45_act3d_insta_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta  instamean2 
reshape long q45_act3d_insta_, i(id) j(day)
gen dow = day+7
rename q45 time
save "Merged/instaact3dmanipulatedbin.dta", replace

use "applimit_filtered.dta", replace
keep id q45_act4d_insta_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta instamean2 
reshape long q45_act4d_insta_, i(id) j(day)
gen dow = day+35
rename q45 time
save "Merged/instaact4dmanipulatedbin.dta", replace

use "Merged/instaact2dmanipulatedbin.dta"
append using "Merged/instaact3dmanipulatedbin.dta"
append using "Merged/instaact4dmanipulatedbin.dta"

* Generate Variables
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
replace at_23=at_23_4 if dow >14
gen logtime = log(time)
bysort id: gen countid = _n
replace countid=. if countid!=1

* Regressions, LT Effects
*ITT
xtset id dow
xtreg time apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTrobustM.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobustM.xls", append
*ITT, logtime
xtset id dow
xtreg logtime apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlogrobustM.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobustM.xls", append
