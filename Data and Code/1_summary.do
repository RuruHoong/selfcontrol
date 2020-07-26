/******************************************************************************
Author: Ruru Hoong
File Name: 1_summary.do
Description: Summary stats of baseline participants, time usage data
******************************************************************************/

use "applimit_intermediate.dta", clear

* Summary demographics of baseline participants
sum q3_age
sum q13_encoded
sum q5_encoded
table q15_ed

* Summary Statistics for Time Usage Data 
ci means q17 q20 pmean1 q36_id1 q37_pred1 pmean2 q36_id2 q37_pred2 pmean3 q36_id3 q37_pred3 pmean4
ci means q18 q21 fbmean1 q38_id1 q39_pred1 fbmean2 q38_id2 q39_pred2 fbmean3 q38_id3 q39_pred3 fbmean4
ci means q19 q22 instamean1 q41_id1 q42_pred1 instamean2 q41_id2 q42_pred2 instamean3 q41_id3 q42_pred3 instamean4

* Difference in Means for Time Usage Data
ttest pmean1==q17
ttest pmean2==q37_pred1
ttest pmean3==q37_pred2
ttest pmean4==q37_pred3

ttest pmean2==q36_id1
ttest pmean3 == q36_id2
ttest pmean4 == q36_id3

ttest fbmean1==q18
ttest fbmean2==q39_pred1
ttest fbmean3==q39_pred2
ttest fbmean4==q39_pred3

ttest fbmean2==q38_id1
ttest fbmean3 == q38_id2
ttest fbmean4 == q38_id3

ttest instamean1==q19
ttest instamean2==q42_pred1
ttest instamean3==q42_pred2
ttest instamean4==q42_pred3

ttest instamean2==q41_id1
ttest instamean3 == q41_id2
ttest instamean4 == q41_id3
