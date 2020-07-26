/******************************************************************************
Author: Ruru Hoong
File Name: 3_analysisLT_HTE.do
Description: Regressions for Effects of App Intervention, Long Term, & Heterogeneous Treatment Effects, Robustness
******************************************************************************/

use "applimit_intermediate.dta", clear
drop if _merge3==1
drop if _merge4==1
drop if app2_query==2
gen id=_n
* Generate Difference between Week 1 Actual and Ideal for Heterogeneity Analysis
gen baseline_ideal_p = pmean2- q36_id1
gen baseline_ideal_fb = fbmean2- q38_id1
gen baseline_ideal_insta = instamean2- q41_id1
* Generate treatment variable for robustness check to the inclusion of participants that already have an app limit prior to experiment
gen at_23robust = at_23 
replace at_23robust = 1 if app2_query==2
* Drop day 7 values 
drop q43_act2d_phone_7 q43_act3d_phone_7 q43_act4d_phone_7 q44_act2d_fb_7 q44_act3d_fb_7 q44_act4d_fb_7 q45_act2d_insta_7 q45_act3d_insta_7 q45_act4d_insta_7 
save "applimit_filtered.dta", replace

/* 1. Phone */
use "applimit_filtered.dta",replace
keep id q43_act2d_phone_* apptreat privtreat at_23 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_p pmean2 at_23_4 at_23robust
reshape long q43_act2d_phone_, i(id) j(day)
gen dow = day
rename q43 time
save "Merged/act2dmanipulatedbin.dta", replace

clear all
use "applimit_filtered.dta",replace
keep id q43_act3d_phone_* apptreat privtreat at_23 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_p pmean2 at_23_4 at_23robust
reshape long q43_act3d_phone_, i(id) j(day)
gen dow = day+7
rename q43 time
save "Merged/act3dmanipulatedbin.dta", replace

use "applimit_filtered.dta",replace
keep id q43_act4d_phone_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_p pmean2 at_23robust
reshape long q43_act4d_phone_, i(id) j(day)
gen dow = day+35
rename q43 time
save "Merged/act4dmanipulatedbin.dta", replace

use "Merged/act2dmanipulatedbin.dta"
append using "Merged/act3dmanipulatedbin.dta"
append using "Merged/act4dmanipulatedbin.dta"

* Generate Variables
gen apptreat_FS= apptreat
gen at_23_FS = at_23
gen at_23_4_FS = at_23_4
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
replace at_23=at_23_4 if dow >14
replace at_23robust=0 if dow <= 7
replace at_23robust=at_23_4 if dow >14
gen logtime = log(time)
bysort id: gen countid = _n
replace countid=. if countid!=1
* Generate Bins
gen bin_p = floor(baseline_ideal_p/100)
replace bin_p=2 if bin_p>=2 & bin_p<.
replace bin_p=-1 if bin_p<=-1
* Generate Interactions
gen apptreat_age = apptreat*q3_
gen apptreat_selfcontrols = apptreat*selfcontrols
gen medianselfcontrols = 0
replace medianselfco = 1 if  selfcontrols>=3.230769
gen apptreat_medianselfcontrols = apptreat*medianselfcontrols
gen apptreat_beta = apptreat*beta
gen medianbeta = 0
summ beta if beta <=1, detail
replace medianbeta = 1 if  beta>=.8888889
gen apptreat_medianbeta = apptreat*medianbeta
gen medianage = 0
replace medianage = 1 if q3_age >=23
gen apptreat_medianage = apptreat*medianage
gen apptreat_gender = apptreat*q13_enc
gen apptreat_pmean2 = apptreat*pmean2
gen medianpmean2 = 0 
replace medianpmean2=1 if pmean2>=293
gen apptreat_medianpmean2 = apptreat*medianpmean2
gen apptreat_baseline = apptreat*baseline_ideal
gen medianbaseline = 0
replace medianbaseline = 1 if  baseline_ideal>=137.5
gen apptreat_medianbaseline = apptreat*medianbaseline

* HTEs by Difference between Week 1 Actual and Ideal
xtset id dow
xtreg logtime apptreat baseline_ideal apptreat_baseline i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_continuous.xls", replace
xtreg logtime apptreat baseline_ideal apptreat_baseline i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_continuous.xls", append
xtreg logtime apptreat medianbaseline apptreat_medianbaseline i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_medianbinary.xls", replace
xtreg logtime apptreat medianbaseline apptreat_medianbaseline i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_medianbinary.xls", append
quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & bin_p==-1 , fe vce(cluster id)
estimate store binreg_minus_1_0_phone
quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & bin_p==-1 , fe vce(cluster id)
estimate store binreg_minus_1_0_phone_LT
quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & bin_p==-1, fe vce(cluster id)
estimate store binreg_minus_1_1_phone
quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & bin_p==-1, fe vce(cluster id)
estimate store binreg_minus_1_1_phone_LT
quietly reg at_23_FS apptreat_FS if countid==1 & bin_p==-1
estimate store binreg_minus_1_2_phone
quietly reg at_23_4_FS apptreat_FS if countid==1 & bin_p==-1
estimate store binreg_minus_1_2_phone_LT
forv i = 0(1)2 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & bin_p==`i' , fe vce(cluster id)
  estimate store binreg_`i'_0_phone
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & bin_p==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & bin_p==`i' , fe vce(cluster id)
  estimate store binreg_`i'_0_phone_LT
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & bin_p==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_LT
  quietly reg at_23_FS apptreat_FS if countid==1 & bin_p==`i'
  estimate store binreg_`i'_2_phone
  quietly reg at_23_4_FS apptreat_FS if countid==1 & bin_p==`i'
  estimate store binreg_`i'_2_phone_LT
}
estimates dir
coefplot (binreg_minus_1_0_phone binreg_minus_1_2_phone binreg_minus_1_1_phone, label(ST)) (binreg_minus_1_0_phone_LT binreg_minus_1_2_phone_LT binreg_minus_1_1_phone_LT, label(LT)) || (binreg_0_0_phone binreg_0_2_phone binreg_0_1_phone, label(ST)) (binreg_0_0_phone_LT binreg_0_2_phone_LT binreg_0_1_phone_LT, label(LT)) ||  (binreg_1_0_phone binreg_1_2_phone binreg_1_1_phone, label(ST)) (binreg_1_0_phone_LT binreg_1_2_phone_LT binreg_1_1_phone_LT, label(LT)) ||  (binreg_2_0_phone binreg_2_2_phone binreg_2_1_phone, label(ST)) (binreg_2_0_phone_LT binreg_2_2_phone_LT binreg_2_1_phone_LT, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Baseline Actual - Ideal (mins)") ylabel( 1 "<0" 2 "[0,100)" 3 "[100,200)" 4 "> 200", noticks) legend(off)
graph save Graph "Output/TE_phone.gph", replace

* HTEs by Tangney Self-Control Score
xtset id dow
xtreg logtime apptreat selfcontrols apptreat_selfcontrols i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_continuous.xls", replace
xtreg logtime apptreat selfcontrols apptreat_selfcontrols i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_continuous.xls", append
xtreg logtime apptreat medianselfcontrols apptreat_medianselfcontrols i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_medianbinary.xls", replace
xtreg logtime apptreat medianselfcontrols apptreat_medianselfcontrols i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_scs
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_LT_scs
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_scs
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_LT_scs
  quietly reg at_23_FS apptreat_FS if countid==1 & medianselfcontrols==`i'
  estimate store binreg_`i'_2_phone_scs
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianselfcontrols==`i'
  estimate store binreg_`i'_2_phone_LT_scs
}
coefplot (binreg_0_0_phone_scs binreg_0_2_phone_scs binreg_0_1_phone_scs, label(ST)) (binreg_0_0_phone_LT_scs binreg_0_2_phone_LT_scs binreg_0_1_phone_LT_scs, label(LT)) || (binreg_1_0_phone_scs binreg_1_2_phone_scs binreg_1_1_phone_scs, label(ST)) (binreg_1_0_phone_LT_scs binreg_1_2_phone_LT_scs binreg_1_1_phone_LT_scs, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Tangney Self-Control Score") ylabel( 1 "Low" 2 "High", noticks) legend(off)
graph save Graph "Output/TE_phone_scs.gph", replace

* HTEs by Beta
xtset id dow
xtreg logtime apptreat beta apptreat_beta i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_continuous.xls", replace
xtreg logtime apptreat beta apptreat_beta i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_continuous.xls", append
xtreg logtime apptreat medianbeta apptreat_medianbeta i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_medianbinary.xls", replace
xtreg logtime apptreat medianbeta apptreat_medianbeta i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_beta
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_LT_beta
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_beta
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_LT_beta
  quietly reg at_23_FS apptreat_FS if countid==1 & medianbeta==`i'
  estimate store binreg_`i'_2_phone_beta
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianbeta==`i'
  estimate store binreg_`i'_2_phone_LT_beta
}
coefplot (binreg_0_0_phone_beta binreg_0_2_phone_beta binreg_0_1_phone_beta, label(ST)) (binreg_0_0_phone_LT_beta binreg_0_2_phone_LT_beta binreg_0_1_phone_LT_beta, label(LT)) || (binreg_1_0_phone_beta binreg_1_2_phone_beta binreg_1_1_phone_beta, label(ST)) (binreg_1_0_phone_LT_beta binreg_1_2_phone_LT_beta binreg_1_1_phone_LT_beta, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Beta") ylabel( 1 "Low" 2 "High", noticks) legend(off)
graph save Graph "Output/TE_phone_beta.gph", replace

* HTEs by Gender
xtset id dow
xtreg logtime apptreat q13_enc apptreat_gender i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_gender", replace
xtreg logtime apptreat q13_enc apptreat_gender i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_gender.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_gender
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_LT_gender
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_gender
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_LT_gender
  quietly reg at_23_FS apptreat_FS if countid==1 & q13_enc==`i'
  estimate store binreg_`i'_2_phone_gender
  quietly reg at_23_4_FS apptreat_FS if countid==1 & q13_enc==`i'
  estimate store binreg_`i'_2_phone_LT_gender
}
coefplot (binreg_0_0_phone_gender binreg_0_2_phone_gender binreg_0_1_phone_gender, label(ST)) (binreg_0_0_phone_LT_gender binreg_0_2_phone_LT_gender binreg_0_1_phone_LT_gender, label(LT)) || (binreg_1_0_phone_gender binreg_1_2_phone_gender binreg_1_1_phone_gender, label(ST)) (binreg_1_0_phone_LT_gender binreg_1_2_phone_LT_gender binreg_1_1_phone_LT_gender, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Gender") ylabel( 1 "Male" 2 "Female", noticks) legend(off)
graph save Graph "Output/TE_phone_gender.gph", replace

* HTEs by Age
xtset id dow
xtreg logtime apptreat q3_age apptreat_age i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_age_continuous.xls", replace
xtreg logtime apptreat q3_age apptreat_age i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_age_continuous.xls", append
xtreg logtime apptreat medianage apptreat_medianage i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_age_medianbinary.xls", replace
xtreg logtime apptreat medianage apptreat_medianage i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_age_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_age
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_LT_age
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_age
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_LT_age
  quietly reg at_23_FS apptreat_FS if countid==1 & medianage==`i'
  estimate store binreg_`i'_2_phone_age
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianage==`i'
  estimate store binreg_`i'_2_phone_LT_age
}
coefplot (binreg_0_0_phone_age binreg_0_2_phone_age binreg_0_1_phone_age, label(ST)) (binreg_0_0_phone_LT_age binreg_0_2_phone_LT_age binreg_0_1_phone_LT_age, label(LT)) || (binreg_1_0_phone_age binreg_1_2_phone_age binreg_1_1_phone_age, label(ST)) (binreg_1_0_phone_LT_age binreg_1_2_phone_LT_age binreg_1_1_phone_LT_age, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Age") ylabel( 1 "Below Median" 2 "Above Median", noticks) legend(off)
graph save Graph "Output/TE_phone_age.gph", replace

* HTEs by Week 1 Actual Usage
xtset id dow
xtreg logtime apptreat pmean2 apptreat_pmean2 i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_continuous.xls", replace
xtreg logtime apptreat pmean2 apptreat_pmean2 i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_continuous.xls", append
xtreg logtime apptreat medianpmean2 apptreat_medianpmean2 i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_medianbinary.xls", replace
xtreg logtime apptreat medianpmean2 apptreat_medianpmean2 i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianpmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_pmean2
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianpmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_phone_LT_pmean2
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianpmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_pmean2
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianpmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_phone_LT_pmean2
  quietly reg at_23_FS apptreat_FS if countid==1 & medianpmean2==`i'
  estimate store binreg_`i'_2_phone_pmean2
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianpmean2==`i'
  estimate store binreg_`i'_2_phone_LT_pmean2
}
coefplot (binreg_0_0_phone_pmean2 binreg_0_2_phone_pmean2 binreg_0_1_phone_pmean2, label(ST)) (binreg_0_0_phone_LT_pmean2 binreg_0_2_phone_LT_pmean2 binreg_0_1_phone_LT_pmean2, label(LT)) || (binreg_1_0_phone_pmean2 binreg_1_2_phone_pmean2 binreg_1_1_phone_pmean2, label(ST)) (binreg_1_0_phone_LT_pmean2 binreg_1_2_phone_LT_pmean2 binreg_1_1_phone_LT_pmean2, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) ytitle("Baseline Usage") ylabel( 1 "Light User" 2 "Heavy User", noticks) legend(off)
graph save Graph "Output/TE_phone_pmean2.gph", replace

* Regressions, LT effects
*ITT
xtset id dow
xtreg time apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITT.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToT.xls", append
*ITT, logtime
xtreg logtime apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlog.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlog.xls", append

* Robustness check
xtset id dow
xtivreg time (at_23robust=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobust.xls", replace
xtivreg time (at_23robust=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobust.xls", append
xtivreg logtime (at_23robust=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobust.xls", replace
xtivreg logtime (at_23robust=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobust.xls", append


/* 2. Facebook */
use "applimit_filtered.dta",replace
keep id q44_act2d_fb_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_fb fbmean2 at_23robust
reshape long q44_act2d_fb_, i(id) j(day)
gen dow = day
rename q44 time
save "Merged/fbact2dmanipulatedbin.dta", replace

clear all
use "applimit_filtered.dta",replace
keep id q44_act3d_fb_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_fb fbmean2 at_23robust
reshape long q44_act3d_fb_, i(id) j(day)
gen dow = day+7
rename q44 time
save "Merged/fbact3dmanipulatedbin.dta", replace

use "applimit_filtered.dta", replace
keep id q44_act4d_fb_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_fb fbmean2 at_23robust
reshape long q44_act4d_fb_, i(id) j(day)
gen dow = day+35
rename q44 time
save "Merged/fbact4dmanipulatedbin.dta", replace

use "Merged/fbact2dmanipulatedbin.dta", replace
append using "Merged/fbact3dmanipulatedbin.dta"
append using "Merged/fbact4dmanipulatedbin.dta"

* Generate Variables
gen apptreat_FS= apptreat
gen at_23_FS = at_23
gen at_23_4_FS = at_23_4
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
replace at_23=at_23_4 if dow >14
replace at_23robust=0 if dow <= 7
replace at_23robust=at_23_4 if dow >14
gen logtime = log(time)
bysort id: gen countid = _n
replace countid=. if countid!=1
* Generate Bins
gen bin_fb = floor(baseline_ideal_fb/100)
replace bin_fb=1 if bin_fb>=1 & bin_fb<.
replace bin_fb=-1 if bin_fb<=-1
* Generate Interactions
gen apptreat_age = apptreat*q3_
gen apptreat_selfcontrols = apptreat*selfcontrols
gen medianselfcontrols = 0
replace medianselfco = 1 if  selfcontrols>=3.230769
gen apptreat_medianselfcontrols = apptreat*medianselfcontrols
gen apptreat_beta = apptreat*beta
gen medianbeta = 0
summ beta if beta <=1, detail
replace medianbeta = 1 if  beta>=.8888889
gen apptreat_medianbeta = apptreat*medianbeta
gen medianage = 0
replace medianage = 1 if q3_age >=23
gen apptreat_medianage = apptreat*medianage
gen apptreat_gender = apptreat*q13_enc
gen medianfbmean2 = 0 
replace medianfbmean2=1 if fbmean2>=40.33333
gen apptreat_medianfbmean2 = apptreat*medianfbmean2
gen apptreat_fbmean2 = apptreat*fbmean2
gen apptreat_baseline = apptreat*baseline_ideal
gen medianbaseline = 0
replace medianbaseline = 1 if  baseline_ideal>=11.58333
gen apptreat_medianbaseline = apptreat*medianbaseline

* HTEs by Difference between Week 1 Actual and Ideal
xtset id dow
xtreg logtime apptreat baseline_ideal apptreat_baseline i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_continuous.xls", append
xtreg logtime apptreat baseline_ideal apptreat_baseline i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_continuous.xls", append
xtreg logtime apptreat medianbaseline apptreat_medianbaseline i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_medianbinary.xls", append
xtreg logtime apptreat medianbaseline apptreat_medianbaseline i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_medianbinary.xls", append
quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & bin_fb==-1 , fe vce(cluster id)
estimate store binreg_minus_1_0_fb
quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & bin_fb==-1 , fe vce(cluster id)
estimate store binreg_minus_1_0_fb_LT
quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & bin_fb==-1, fe vce(cluster id)
estimate store binreg_minus_1_1_fb
quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & bin_fb==-1, fe vce(cluster id)
estimate store binreg_minus_1_1_fb_LT
quietly reg at_23_FS apptreat_FS if countid==1 & bin_fb==-1
estimate store binreg_minus_1_2_fb
quietly reg at_23_4_FS apptreat_FS if countid==1 & bin_fb==-1
estimate store binreg_minus_1_2_fb_LT
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & bin_fb==`i' , fe vce(cluster id)
  estimate store binreg_`i'_0_fb
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & bin_fb==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & bin_fb==`i' , fe vce(cluster id)
  estimate store binreg_`i'_0_fb_LT
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & bin_fb==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_LT
  quietly reg at_23_FS apptreat_FS if countid==1 & bin_fb==`i'
  estimate store binreg_`i'_2_fb
  quietly reg at_23_4_FS apptreat_FS if countid==1 & bin_fb==`i'
  estimate store binreg_`i'_2_fb_LT
}
estimates dir
coefplot (binreg_minus_1_0_fb binreg_minus_1_2_fb binreg_minus_1_1_fb, label(ST)) (binreg_minus_1_0_fb_LT binreg_minus_1_2_fb_LT binreg_minus_1_1_fb_LT, label(LT)) || (binreg_0_0_fb binreg_0_2_fb binreg_0_1_fb, label(ST)) (binreg_0_0_fb_LT binreg_0_2_fb_LT binreg_0_1_fb_LT, label(LT)) ||  (binreg_1_0_fb binreg_1_2_fb binreg_1_1_fb, label(ST)) (binreg_1_0_fb_LT binreg_1_2_fb_LT binreg_1_1_fb_LT, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Baseline Actual - Ideal (mins)") ylabel( 1 "< 0" 2 "[0,100)" 3 "> 100", noticks) legend(off)
graph save Graph "Output/TE_fb.gph", replace

* HTEs by Tangney
xtset id dow
xtreg logtime apptreat selfcontrols apptreat_selfcontrols i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_continuous.xls", append
xtreg logtime apptreat selfcontrols apptreat_selfcontrols i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_continuous.xls", append
xtreg logtime apptreat medianselfcontrols apptreat_medianselfcontrols i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_medianbinary.xls", append
xtreg logtime apptreat medianselfcontrols apptreat_medianselfcontrols i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_scs
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_LT_scs
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_scs
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_LT_scs
  quietly reg at_23_FS apptreat_FS if countid==1 & medianselfcontrols==`i'
  estimate store binreg_`i'_2_fb_scs
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianselfcontrols==`i'
  estimate store binreg_`i'_2_fb_LT_scs
}
coefplot (binreg_0_0_fb_scs binreg_0_2_fb_scs binreg_0_1_fb_scs, label(ST)) (binreg_0_0_fb_LT_scs binreg_0_2_fb_LT_scs binreg_0_1_fb_LT_scs, label(LT)) || (binreg_1_0_fb_scs binreg_1_2_fb_scs binreg_1_1_fb_scs, label(ST)) (binreg_1_0_fb_LT_scs binreg_1_2_fb_LT_scs binreg_1_1_fb_LT_scs, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Tangney Self-Control Score") ylabel( 1 "Low" 2 "High", noticks) legend(off)
graph save Graph "Output/TE_fb_scs.gph", replace

* HTEs by Beta
xtset id dow
xtreg logtime apptreat beta apptreat_beta i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_continuous.xls", append
xtreg logtime apptreat beta apptreat_beta i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_continuous.xls", append
xtreg logtime apptreat medianbeta apptreat_medianbeta i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_medianbinary.xls", append
xtreg logtime apptreat medianbeta apptreat_medianbeta i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_beta
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_LT_beta
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_beta
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_LT_beta
  quietly reg at_23_FS apptreat_FS if countid==1 & medianbeta==`i'
  estimate store binreg_`i'_2_fb_beta
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianbeta==`i'
  estimate store binreg_`i'_2_fb_LT_beta
}
coefplot (binreg_0_0_fb_beta binreg_0_2_fb_beta binreg_0_1_fb_beta, label(ST)) (binreg_0_0_fb_LT_beta binreg_0_2_fb_LT_beta binreg_0_1_fb_LT_beta, label(LT)) || (binreg_1_0_fb_beta binreg_1_2_fb_beta binreg_1_1_fb_beta, label(ST)) (binreg_1_0_fb_LT_beta binreg_1_2_fb_LT_beta binreg_1_1_fb_LT_beta, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Beta") ylabel( 1 "Low" 2 "High", noticks) legend(off)
graph save Graph "Output/TE_fb_beta.gph", replace

* HTEs by Gender
xtset id dow
xtreg logtime apptreat q13_enc apptreat_gender i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_gender", append
xtreg logtime apptreat q13_enc apptreat_gender i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_gender.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_gender
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_LT_gender
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_gender
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_LT_gender
  quietly reg at_23_FS apptreat_FS if countid==1 & q13_enc==`i'
  estimate store binreg_`i'_2_fb_gender
  quietly reg at_23_4_FS apptreat_FS if countid==1 & q13_enc==`i'
  estimate store binreg_`i'_2_fb_LT_gender
}
coefplot (binreg_0_0_fb_gender binreg_0_2_fb_gender binreg_0_1_fb_gender, label(ST)) (binreg_0_0_fb_LT_gender binreg_0_2_fb_LT_gender binreg_0_1_fb_LT_gender, label(LT)) || (binreg_1_0_fb_gender binreg_1_2_fb_gender binreg_1_1_fb_gender, label(ST)) (binreg_1_0_fb_LT_gender binreg_1_2_fb_LT_gender binreg_1_1_fb_LT_gender, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Gender") ylabel( 1 "Male" 2 "Female", noticks) legend(off)
graph save Graph "Output/TE_fb_gender.gph", replace

* HTEs by Age
xtset id dow
xtreg logtime apptreat q3_age apptreat_age i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_age_continuous.xls", append
xtreg logtime apptreat q3_age apptreat_age i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_age_continuous.xls", append
xtreg logtime apptreat medianage apptreat_medianage i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_age_medianbinary.xls", append
xtreg logtime apptreat medianage apptreat_medianage i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_age_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_age
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_LT_age
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_age
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_LT_age
  quietly reg at_23_FS apptreat_FS if countid==1 & medianage==`i'
  estimate store binreg_`i'_2_fb_age
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianage==`i'
  estimate store binreg_`i'_2_fb_LT_age
}
coefplot (binreg_0_0_fb_age binreg_0_2_fb_age binreg_0_1_fb_age, label(ST)) (binreg_0_0_fb_LT_age binreg_0_2_fb_LT_age binreg_0_1_fb_LT_age, label(LT)) || (binreg_1_0_fb_age binreg_1_2_fb_age binreg_1_1_fb_age, label(ST)) (binreg_1_0_fb_LT_age binreg_1_2_fb_LT_age binreg_1_1_fb_LT_age, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Age") ylabel( 1 "Below Median" 2 "Above Median", noticks) legend(off)
graph save Graph "Output/TE_fb_age.gph", replace

* HTEs by Week 1 Actual Usage
xtset id dow
xtreg logtime apptreat fbmean2 apptreat_fbmean2 i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_continuous.xls", append
xtreg logtime apptreat fbmean2 apptreat_fbmean2 i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_continuous.xls", append
xtreg logtime apptreat medianfbmean2 apptreat_medianfbmean2 i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_medianbinary.xls", append
xtreg logtime apptreat medianfbmean2 apptreat_medianfbmean2 i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianfbmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_fbmean2
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianfbmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_fb_LT_fbmean2
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianfbmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_fbmean2
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianfbmean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_fb_LT_fbmean2
  quietly reg at_23_FS apptreat_FS if countid==1 & medianfbmean2==`i'
  estimate store binreg_`i'_2_fb_fbmean2
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianfbmean2==`i'
  estimate store binreg_`i'_2_fb_LT_fbmean2
}
coefplot (binreg_0_0_fb_fbmean2 binreg_0_2_fb_fbmean2 binreg_0_1_fb_fbmean2, label(ST)) (binreg_0_0_fb_LT_fbmean2 binreg_0_2_fb_LT_fbmean2 binreg_0_1_fb_LT_fbmean2, label(LT)) || (binreg_1_0_fb_fbmean2 binreg_1_2_fb_fbmean2 binreg_1_1_fb_fbmean2, label(ST)) (binreg_1_0_fb_LT_fbmean2 binreg_1_2_fb_LT_fbmean2 binreg_1_1_fb_LT_fbmean2, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) ytitle("Baseline Usage") ylabel( 1 "Light User" 2 "Heavy User", noticks) legend(off)
graph save Graph "Output/TE_phone_fbmean2.gph", replace

* Regressions, LT effects
*ITT
xtset id dow
xtreg time apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITT.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToT.xls", append
*ITT, logtime
xtreg logtime apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlog.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlog.xls", append

* Robustness check
xtset id dow
xtivreg time (at_23robust=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobust.xls", append
xtivreg time (at_23robust=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobust.xls", append
xtivreg logtime (at_23robust=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobust.xls", append
xtivreg logtime (at_23robust=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobust.xls", append


/* 3. Instagram */
use "applimit_filtered.dta",replace
keep id q45_act2d_insta_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_insta instamean2 at_23robust
reshape long q45_act2d_insta_, i(id) j(day)
gen dow = day
rename q45 time
save "Merged/instaact2dmanipulatedbin.dta", replace

clear all
use "applimit_filtered.dta"
keep id q45_act3d_insta_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_insta instamean2 at_23robust
reshape long q45_act3d_insta_, i(id) j(day)
gen dow = day+7
rename q45 time
save "Merged/instaact3dmanipulatedbin.dta", replace

use "applimit_filtered.dta", replace
keep id q45_act4d_insta_* apptreat privtreat at_23 at_23_4 q15_col q13_enc q16_enc q3_ selfcontrols beta baseline_ideal_insta instamean2 at_23robust
reshape long q45_act4d_insta_, i(id) j(day)
gen dow = day+35
rename q45 time
save "Merged/instaact4dmanipulatedbin.dta", replace

use "Merged/instaact2dmanipulatedbin.dta"
append using "Merged/instaact3dmanipulatedbin.dta"
append using "Merged/instaact4dmanipulatedbin.dta"

* Generate Variables
gen apptreat_FS= apptreat
gen at_23_FS = at_23
gen at_23_4_FS = at_23_4
replace apptreat=0 if dow <= 7
replace at_23=0 if dow <= 7
replace at_23=at_23_4 if dow >14
replace at_23robust=0 if dow <= 7
replace at_23robust=at_23_4 if dow >14
gen logtime = log(time)
bysort id: gen countid = _n
replace countid=. if countid!=1
* Generate Bins
gen bin_insta = floor(baseline_ideal_insta/100)
replace bin_insta=0 if bin_insta>=0  & bin_insta<.
replace bin_insta=-1 if bin_insta<=-1
* Generate Interactions
gen apptreat_age = apptreat*q3_
gen apptreat_selfcontrols = apptreat*selfcontrols
gen medianselfcontrols = 0
replace medianselfco = 1 if  selfcontrols>=3.230769
gen apptreat_medianselfcontrols = apptreat*medianselfcontrols
gen apptreat_beta = apptreat*beta
gen medianbeta = 0
summ beta if beta <=1, detail
replace medianbeta = 1 if  beta>=.8888889
gen apptreat_medianbeta = apptreat*medianbeta
gen medianage = 0
replace medianage = 1 if q3_age >=23
gen apptreat_medianage = apptreat*medianage
gen apptreat_gender = apptreat*q13_enc
gen medianinstamean2 = 0 
replace medianinstamean2=1 if instamean2>=21.25
gen apptreat_medianinstamean2 = apptreat*medianinstamean2
gen apptreat_instamean2 = apptreat*instamean2
gen apptreat_baseline = apptreat*baseline_ideal
gen medianbaseline = 0
replace medianbaseline = 1 if  baseline_ideal>=1.333
gen apptreat_medianbaseline = apptreat*medianbaseline

* HTEs by Difference between Week 1 Actual and Ideal
xtset id dow
xtreg logtime apptreat baseline_ideal apptreat_baseline i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_continuous.xls", append
xtreg logtime apptreat baseline_ideal apptreat_baseline i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_continuous.xls", append
xtreg logtime apptreat medianbaseline apptreat_medianbaseline i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_medianbinary.xls", append
xtreg logtime apptreat medianbaseline apptreat_medianbaseline i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_baseline_medianbinary.xls", append
quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & bin_insta==-1 , fe vce(cluster id)
estimate store binreg_minus_1_0_insta
quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & bin_insta==-1 , fe vce(cluster id)
estimate store binreg_minus_1_0_insta_LT
quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & bin_insta==-1, fe vce(cluster id)
estimate store binreg_minus_1_1_insta
quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & bin_insta==-1, fe vce(cluster id)
estimate store binreg_minus_1_1_insta_LT
quietly reg at_23_FS apptreat_FS if countid==1 & bin_insta==-1
estimate store binreg_minus_1_2_insta
quietly reg at_23_4_FS apptreat_FS if countid==1 & bin_insta==-1
estimate store binreg_minus_1_2_insta_LT
forv i = 0(1)0 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & bin_insta==`i' , fe vce(cluster id)
  estimate store binreg_`i'_0_insta
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & bin_insta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & bin_insta==`i' , fe vce(cluster id)
  estimate store binreg_`i'_0_insta_LT
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & bin_insta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_LT
  quietly reg at_23_FS apptreat_FS if countid==1 & bin_insta==`i'
  estimate store binreg_`i'_2_insta
  quietly reg at_23_4_FS apptreat_FS if countid==1 & bin_insta==`i'
  estimate store binreg_`i'_2_insta_LT
}
estimates dir
coefplot (binreg_minus_1_0_insta binreg_minus_1_2_insta binreg_minus_1_1_insta, label(ST)) (binreg_minus_1_0_insta_LT binreg_minus_1_2_insta_LT binreg_minus_1_1_insta_LT, label(LT)) || (binreg_0_0_insta binreg_0_2_insta binreg_0_1_insta, label(ST)) (binreg_0_0_insta_LT binreg_0_2_insta_LT binreg_0_1_insta_LT, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Baseline Actual - Ideal (mins)") ylabel( 1 "< 0" 2 "> 0", noticks) legend(off)
graph save Graph "Output/TE_insta.gph", replace

* HTEs by Week 1 Actual Usage
xtset id dow
xtreg logtime apptreat instamean2 apptreat_instamean2 i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_continuous.xls", append
xtreg logtime apptreat instamean2 apptreat_instamean2 i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_continuous.xls", append
xtreg logtime apptreat medianinstamean2 apptreat_medianinstamean2 i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_medianbinary.xls", append
xtreg logtime apptreat medianinstamean2 apptreat_medianinstamean2 i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_mean2_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianinstamean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_instamean2
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianinstamean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_LT_instamean2
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianinstamean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_instamean2
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianinstamean2==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_LT_instamean2
  quietly reg at_23_FS apptreat_FS if countid==1 & medianinstamean2==`i'
  estimate store binreg_`i'_2_instamean2
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianinstamean2==`i'
  estimate store binreg_`i'_2_LT_instamean2
}
coefplot (binreg_0_0_instamean2 binreg_0_2_instamean2 binreg_0_1_instamean2, label(ST)) (binreg_0_0_LT_instamean2 binreg_0_2_LT_instamean2 binreg_0_1_LT_instamean2, label(LT)) || (binreg_1_0_instamean2 binreg_1_2_instamean2 binreg_1_1_instamean2, label(ST)) (binreg_1_0_LT_instamean2 binreg_1_2_LT_instamean2 binreg_1_1_LT_instamean2, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) ytitle("Baseline Usage") ylabel( 1 "Light User" 2 "Heavy User", noticks) legend(off)
graph save Graph "Output/TE_phone_instamean2.gph", replace

* HTEs by Tangney
xtset id dow
xtreg logtime apptreat selfcontrols apptreat_selfcontrols i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_continuous.xls", append
xtreg logtime apptreat selfcontrols apptreat_selfcontrols i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_continuous.xls", append
xtreg logtime apptreat medianselfcontrols apptreat_medianselfcontrols i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_medianbinary.xls", append
xtreg logtime apptreat medianselfcontrols apptreat_medianselfcontrols i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_scs_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_scs
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_LT_scs
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_scs
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianselfcontrols==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_LT_scs
  quietly reg at_23_FS apptreat_FS if countid==1 & medianselfcontrols==`i'
  estimate store binreg_`i'_2_insta_scs
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianselfcontrols==`i'
  estimate store binreg_`i'_2_insta_LT_scs
}
coefplot (binreg_0_0_insta_scs binreg_0_2_insta_scs binreg_0_1_insta_scs, label(ST)) (binreg_0_0_insta_LT_scs binreg_0_2_insta_LT_scs binreg_0_1_insta_LT_scs, label(LT)) || (binreg_1_0_insta_scs binreg_1_2_insta_scs binreg_1_1_insta_scs, label(ST)) (binreg_1_0_insta_LT_scs binreg_1_2_insta_LT_scs binreg_1_1_insta_LT_scs, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Tangney Self-Control Score") ylabel( 1 "Low" 2 "High", noticks) legend(off)
graph save Graph "Output/TE_insta_scs.gph", replace

* HTEs by Beta
xtset id dow
xtreg logtime apptreat beta apptreat_beta i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_continuous.xls", append
xtreg logtime apptreat beta apptreat_beta i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_continuous.xls", append
xtreg logtime apptreat medianbeta apptreat_medianbeta i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_medianbinary.xls", append
xtreg logtime apptreat medianbeta apptreat_medianbeta i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_beta_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_beta
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_LT_beta
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_beta
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianbeta==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_LT_beta
  quietly reg at_23_FS apptreat_FS if countid==1 & medianbeta==`i'
  estimate store binreg_`i'_2_insta_beta
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianbeta==`i'
  estimate store binreg_`i'_2_insta_LT_beta
}
coefplot (binreg_0_0_insta_beta binreg_0_2_insta_beta binreg_0_1_insta_beta, label(ST)) (binreg_0_0_insta_LT_beta binreg_0_2_insta_LT_beta binreg_0_1_insta_LT_beta, label(LT)) || (binreg_1_0_insta_beta binreg_1_2_insta_beta binreg_1_1_insta_beta, label(ST)) (binreg_1_0_insta_LT_beta binreg_1_2_insta_LT_beta binreg_1_1_insta_LT_beta, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Beta") ylabel( 1 "Low" 2 "High", noticks) legend(off)
graph save Graph "Output/TE_insta_beta.gph", replace

* HTEs by Gender
xtset id dow
xtreg logtime apptreat q13_enc apptreat_gender i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_gender", append
xtreg logtime apptreat q13_enc apptreat_gender i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_gender.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_gender
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_LT_gender
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_gender
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & q13_enc==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_LT_gender
  quietly reg at_23_FS apptreat_FS if countid==1 & q13_enc==`i'
  estimate store binreg_`i'_2_insta_gender
  quietly reg at_23_4_FS apptreat_FS if countid==1 & q13_enc==`i'
  estimate store binreg_`i'_2_insta_LT_gender
}
coefplot (binreg_0_0_insta_gender binreg_0_2_insta_gender binreg_0_1_insta_gender, label(ST)) (binreg_0_0_insta_LT_gender binreg_0_2_insta_LT_gender binreg_0_1_insta_LT_gender, label(LT)) || (binreg_1_0_insta_gender binreg_1_2_insta_gender binreg_1_1_insta_gender, label(ST)) (binreg_1_0_insta_LT_gender binreg_1_2_insta_LT_gender binreg_1_1_insta_LT_gender, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Gender") ylabel( 1 "Male" 2 "Female", noticks) legend(off)
graph save Graph "Output/TE_insta_gender.gph", replace

* HTEs by Age
xtset id dow
xtreg logtime apptreat q3_age apptreat_age i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_age_continuous.xls", append
xtreg logtime apptreat q3_age apptreat_age i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_age_continuous.xls", append
xtreg logtime apptreat medianage apptreat_medianage i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "TE_age_medianbinary.xls", append
xtreg logtime apptreat medianage apptreat_medianage i.day if (dow <=7 | dow>14) & day <=6, fe vce(cluster id)
outreg2 using "TE_age_medianbinary.xls", append
forv i = 0(1)1 {
  quietly xtreg logtime apptreat i.day if dow <=14 & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_age
  quietly xtreg logtime apptreat i.day if (dow <=7 | dow>14) & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_0_insta_LT_age
  quietly xtivreg logtime (at_23=apptreat) i.day if dow <=14 & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_age
  quietly xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow>14) & day <=6 & medianage==`i', fe vce(cluster id)
  estimate store binreg_`i'_1_insta_LT_age
  quietly reg at_23_FS apptreat_FS if countid==1 & medianage==`i'
  estimate store binreg_`i'_2_insta_age
  quietly reg at_23_4_FS apptreat_FS if countid==1 & medianage==`i'
  estimate store binreg_`i'_2_insta_LT_age
}
coefplot (binreg_0_0_insta_age binreg_0_2_insta_age binreg_0_1_insta_age, label(ST)) (binreg_0_0_insta_LT_age binreg_0_2_insta_LT_age binreg_0_1_insta_LT_age, label(LT)) || (binreg_1_0_insta_age binreg_1_2_insta_age binreg_1_1_insta_age, label(ST)) (binreg_1_0_insta_LT_age binreg_1_2_insta_LT_age binreg_1_1_insta_LT_age, label(LT)) ,  keep(at_23 apptreat at_23_FS apptreat_FS) xline(0) bycoefs byopts(xrescale row(1)) xtitle("Treatment Effect") ytitle("Age") ylabel( 1 "Below Median" 2 "Above Median", noticks) legend(off)
graph save Graph "Output/TE_insta_age.gph", replace

* Regressions, LT Effects
*ITT
xtset id dow
xtreg time apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITT.xls", append
*ToT
xtivreg time (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToT.xls", append
*ITT, logtime
xtset id dow
xtreg logtime apptreat i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitITTlog.xls", append
*ToT, logtime
xtivreg logtime (at_23=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlog.xls", append

* Robustness check
xtset id dow
xtivreg time (at_23robust=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobust.xls", append
xtivreg time (at_23robust=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTrobust.xls", append
xtivreg logtime (at_23robust=apptreat) i.day if dow <=14 & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobust.xls", append
xtivreg logtime (at_23robust=apptreat) i.day if (dow <=7 | dow > 14) & day <=6, fe vce(cluster id)
outreg2 using "applimitToTlogrobust.xls", append
