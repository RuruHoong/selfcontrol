/******************************************************************************
Author: Ruru Hoong
File Name: 0_generatevar.do
Description: Generate and encode variables for use in analysis
******************************************************************************/

* Generate dependent variables
egen fbmean1 = rmean(q44_act1d_fb_1 - q44_act1d_fb_6)
egen fbmean2 = rmean(q44_act2d_fb_1 - q44_act2d_fb_6)
egen fbmean3 = rmean(q44_act3d_fb_1 - q44_act3d_fb_6)
egen fbmean4 = rmean(q44_act4d_fb_1 - q44_act4d_fb_6)
egen instamean1 = rmean(q45_act1d_insta_1 - q45_act1d_insta_6)
egen instamean2 = rmean(q45_act2d_insta_1 - q45_act2d_insta_6)
egen instamean3 = rmean(q45_act3d_insta_1 - q45_act3d_insta_6)
egen instamean4 = rmean(q45_act4d_insta_1 - q45_act4d_insta_6)
egen pmean1 = rmean(q43_act1d_phone_1 - q43_act1d_phone_6)
egen pmean2 = rmean(q43_act2d_phone_1 - q43_act2d_phone_6)
egen pmean3 = rmean(q43_act3d_phone_1 - q43_act3d_phone_6)
egen pmean4 = rmean(q43_act4d_phone_1 - q43_act4d_phone_6)

gen diffact2id1 = pmean2 - q36_id1
gen diffact2id1perc = diffact2id1/ q36_id1
gen difffbact2id1 = fbmean2 - q38_id1
gen difffbact2id1perc = difffbact2id1/ q38_id1
gen diffinstaact2id1 = instamean2 - q41_id1
gen diffinstaact2id1perc = diffinstaact2id1/ q41_id1

gen at_23 = 0 if _merge2==3
replace at_23= 1 if app2_query==1
replace at_23= (7-app3_off)/7 if app3_off<.

gen at_23_4 = 0 if _merge4==3
replace at_23_4= 1 if app4_change_2=="No"
replace at_23_4= (7-app4_off)/7 if app4_off<.

gen manual4 = 1 if manual1==1 | manual2==1 | manual3==1 //participants with "Screen Time" responses manually overridden

* Beta
gen beta= 20*bdm2/bdm1 /bdm1
gen betacheck =0
replace betacheck=1 if beta<=1 //Check for reasonable beta value

* Tangney self-control score
foreach var of varlist selfcontrol_1 - selfcontrol_13{
gen s`var'=`var'
}
foreach var of varlist sselfcontrol_2 - sselfcontrol_5{
replace `var'=6-`var'
}
foreach var of varlist sselfcontrol_9 - sselfcontrol_10{
replace `var'=6-`var'
}
foreach var of varlist sselfcontrol_12 - sselfcontrol_13{
replace `var'=6-`var'
}
replace sselfcontrol_7=6-sselfcontrol_7
egen selfcontrols = rmean(sselfcontrol_1-sselfcontrol_13) //Tangney SCS

* Encode variables  
* Education
label define q15_ed 0 "Less than high school degree" 1 "High school graduate (high school diploma or equivalent including GED)" 2 "Some college but no degree" 3 "Associate degree in college (2-year)" 4 "Bachelor's degree in college (4-year)" 5 "Master's degree" 6 "Doctoral degree" 7 "Professional degree (JD, MD)"
encode q15_education, gen (q15_ed)
gen q15_encoded=0
replace q15_encoded=1 if q15_encoded==2
replace q15_encoded=2 if q15_encoded==3 | q15_encoded==4
replace q15_encoded=3 if q15_encoded==5 |q15_encoded==6 | q15_encoded==7
gen q15_college=1
replace q15_college=0 if q15_ed==0 |  q15_ed==1

* Instagram
label define q5_encoded 0 "No" 1 "Yes"
encode q5, gen (q5_encoded)

* Gender
label define q13_encoded 0 "Male" 1 "Female" 3 "Trans" 3 "Other"
encode q13_gender, gen (q13_encoded)
replace q13_encoded = . if q13_encoded==3 //for simplicity of analysis and due to small sample size, limit to male and female

* Race
gen q16_encoded=1
replace q16_encoded=0 if q16_race!= "White" //Binary variable 

save "applimit_intermediate.dta", replace
