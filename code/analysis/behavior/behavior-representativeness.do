**********
* CREATE REPRESENTATIVENESS TABLE: COMPARE BEHAVIOR SURVEY TO GALLUP
**********

*Define variables and labels
local sumvar male age whitenohisp high_school_higher bachelor_higher fulltime annual_inc

*Define labels
local male_label				"Male"
local age_label 				"Age"
local high_school_higher_label	"At least high school degree"
local bachelor_higher_label 	"Bachelor degree or above"
local fulltime_label			"Employed full-time"
local annual_inc_label 			"Annual household income (USD)"
local whitenohisp_label 		"Race: White"


***CALCULATE SUMMARY STATS FOR BEHAVIOR SURVEY
*Use data
use "data/working/behavior-survey.dta", clear 

* Restriction to Fox viewers
keep if foxviewer == 1

* Calculate summary stats
foreach var of varlist `sumvar' {
	*Summarize
	sum `var'
	local `var'_mean_svy: di %3.2f `r(mean)'
	*Total		
	count
	local count_svy `r(N)'	
}
	
	
***CALCULATE SUMMARY STATS FOR AL
*Use data
use "data/working/gallup.dta", clear

* sample restriction: republican aged above 54 
keep if age > 54 & republican == 1 

* calculate summary stats
svyset [pweight = weights]

foreach var of varlist `sumvar' {
	svy: mean `var'
	*Summarize
	local `var'_mean_gallup: di %3.2f _b[`var']
	*Total
	count
	local count_gallup `r(N)'
}


**CREATE SUMMARY STATS TABLE
foreach var of varlist `sumvar' {

	//cap file close table 
	file open table using "output/tables/behavior-representativeness.tex", write replace

	local fwt "file write table"
		
	local caption "Sample representativeness"
	local cols 2
	local header "\begin{table}[H] \centering \caption{`caption'} \label{t:representativeness} \begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular} \end{table}"

	`fwt' "`header'" _n
	`fwt' "Variables: & Survey & Gallup \\" _n 
	`fwt' "\midrule" _n
	`fwt' "\midrule" _n

	foreach var in male age whitenohisp {
		`fwt' "``var'_label' & ``var'_mean_svy' & ``var'_mean_gallup' \\" _n
	}
	`fwt' "\addlinespace" _n 
	
	foreach var in high_school_higher bachelor_higher {
		`fwt' "``var'_label' & ``var'_mean_svy' & ``var'_mean_gallup' \\" _n
	}
	`fwt' "\addlinespace" _n 
	
	foreach var in fulltime annual_inc {
		`fwt' "``var'_label' & ``var'_mean_svy' & ``var'_mean_gallup' \\" _n
	}
	`fwt' "\midrule" _n
	`fwt' "Observations  & `count_svy' & `count_gallup' \\" _n
	`fwt' "\bottomrule" _n
	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n
		
	file close table
} 

