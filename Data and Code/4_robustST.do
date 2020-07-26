/******************************************************************************
Author: Ruru Hoong
File Name: 4_robustST.do
Description: Regressions for Effects of App Intervention, Short Term (robustness to exclusion of manually overidden)
******************************************************************************/

use "applimit_intermediate.dta", clear
drop if _merge3==1
drop if manual4==1
gen id=_n
save "applimit_filtered.dta", replace

/* 1. Phone */
use "applimit_filtered.dta", replace
keep id q43_act2d_phone_* apptreat privtreat at_23 q15_encoded q13_encoded q16_encoded q3_
reshape long q43_act2d_phone_, i(id) j(day)
gen dow = day
rename q43 time
save "Merged/act2dmanipulated.dta", replace

clear all
use "applimit_filtered.dta", replace
keep id q43_act3d_phone_* apptreat privtreat at_23 q15_encoded q13_encoded q16_encoded q3_
reshape long q43_act3d_phone_, i(id) j(day)
gen dow = day+7
rename q43 time
save "Merged/act3dmanipulated.dta", replace

use "Merged/act2dmanipulated.dta"
append using "Merged/act3dmanipulated.dta"
gen apptreatb=apptreat
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
drop if missing(time)
gen logtime = log(time)

*Regressions
*ITT
xtset id dow
xtreg time apptreat i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitITTrobustM.xls", replace
*ToT
xtivreg time (at_23=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobustM.xls", replace
*ITT, logtime
xtreg logtime apptreat i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlogrobustM.xls", replace
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobustM.xls", replace

/* 2. Facebook */
use "applimit_filtered.dta", replace
keep id q44_act2d_fb_* apptreat privtreat at_23 q15_encoded q13_encoded q16_encoded q3_
reshape long q44_act2d_fb_, i(id) j(day)
gen dow = day
rename q44 time
save "Merged/fbact2dmanipulated.dta", replace

clear all
use "applimit_filtered.dta", replace
keep id q44_act3d_fb_* apptreat privtreat at_23 q15_encoded q13_encoded q16_encoded q3_
reshape long q44_act3d_fb_, i(id) j(day)
gen dow = day+7
rename q44 time
save "Merged/fbact3dmanipulated.dta", replace

use "Merged/fbact2dmanipulated.dta"
append using "Merged/fbact3dmanipulated.dta"
gen apptreatb=apptreat
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
drop if missing(time)
gen logtime = log(time)

*Regressions
*ITT
xtset id dow
xtreg time apptreat i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitITTrobustM.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobustM.xls", append
*ITT, logtime
xtreg logtime apptreat i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlogrobustM.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobustM.xls", append


/* 3. Instagram */
use "applimit_filtered.dta", replace
keep id q45_act2d_insta_* apptreat privtreat at_23 q15_encoded q13_encoded q16_encoded q3_
reshape long q45_act2d_insta_, i(id) j(day)
gen dow = day
rename q45 time
save "Merged/instaact2dmanipulated.dta", replace

clear all
use "applimit_filtered.dta", replace
keep id q45_act3d_insta_* apptreat privtreat at_23 q15_encoded q13_encoded q16_encoded q3_
reshape long q45_act3d_insta_, i(id) j(day)
gen dow = day+7
rename q45 time
save "Merged/instaact3dmanipulated.dta", replace

use "Merged/instaact2dmanipulated.dta"
append using "Merged/instaact3dmanipulated.dta"
gen apptreatb=apptreat
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
drop if missing(time)
gen logtime = log(time)

*Regressions
xtset id dow
xtreg time apptreat i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitITTrobustM.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobustM.xls", append
*ITT, logtime
xtreg logtime apptreat i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlogrobustM.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobustM.xls", append
