clear

cd /Users/rafaelfrade/arquivos/mestrado/tese/applications/uruguai/anonymized_data

use  peso4_anonymized.dta

cd ..
cd programs

do 0.prepare0.do
do 7.table1.do

tabulate instm, generate (educ_m)
tab material_pisos, gen(floor)
tab material_paredes, gen(wall)

drop educ_m5 floor4 wall6

rename newind score_m
rename edadm age_m
ren peso weight

drop artef_cant_autos

foreach var of varlist artef* {
   replace `var' = `var' - 1 if `var' != .
   //drop if `var' == .
}

//drop if score_m == .
drop if age_m == .
drop if weight == .

gen treated = 0
replace treated = 1 if tmp == 1
gen applied = 1
replace applied = 0 if tmp == 3

replace casa_saneamiento = casa_saneamiento - 1

//score_m
keep weight age_m treated applied score_m bajo2500 numemban educ_m* artef* floor* wall* es_jubilado es_pensionista es_rentista es_estudiante salud_mutualista casa_saneamiento


//drop if newind >= 0

logit treated numemban educ_m* artef* floor* wall* es_jubilado es_pensionista es_rentista es_estudiante salud_mutualista casa_saneamiento if applied == 1
predict p_hat


logit bajo2500 numemban educ_m* artef* floor* wall* if applied == 1
predict p_low_weight


order weight age_m treated applied p_hat score_m p_low_weight numemban educ_m* artef* floor* wall* 

drop if applied == 1 & p_hat == .

export delimited using "/Users/rafaelfrade/arquivos/mestrado/tese/my_code/matlab/data_uruguai.csv", replace


//twoway scatter yhat1 age_m numemban educ_m* artef* floor* wall*, connect(l i) msymbol(i O) sort ylabel(0 1)
