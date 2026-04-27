**********
* Create F-stats for instrument versions table
**********

*Use main panel
use "data/working/TuckerCarlsonIV_County_PANEL", replace
 
*Define
global CrossSection_Mar28   "if elapdate==22002"

global HEALTH 		"uninsured_raw poor_physical_days_raw " 
global ECON			"perc_poor2018 lmed_hh_inc2018 urate_bls"
global EDUC			"edushare_male_noHS edushare_female_noHS edushare_male_noCOLLEGE edushare_female_noCOLLEGE"
global AGE			"pop_65_plus_perc"
global POPRACE		"CensusPercentRural pop_white_perc pop_hispanic_perc "
global POLITICS		"repshare2016 ltotvotes2016"
global GEO			"popweighted_lat popweighted_lon"		
global healthcap 	"beds nurses personnel"

forvalues i = 1/3{
	global X_0_v`i'	"fox_shr_of_cable fox_shr_Jan2020 pop_density_2019 msnbc_shr_of_cable lpop_2019 pr_hutput_V`i'_tucker pr_hutput_V`i'_hannity pr_hutput_V`i'_ingraham"
}


preserve
	clear all
	eststo clear
	estimates drop _all

	set obs 10
	qui gen x = 1
	qui gen y = 1

	loc columns = 0

	forvalues i=1/6 {
		qui eststo col`i': reg x y
	}
restore

	
ivreg2 ldeaths 	(zdiff_viewersHvT = IV_V1_hannity)              $X_0_v1 $GEO $POPRACE $AGE $ECON $EDUC $HEALTH $POLITICS $healthcap i.state_fips $CrossSection_Mar28, cl(geography) first 
	mat firststage = e(first)
	estadd loc fstat = string(firststage[8,1], "%9.2f"): col1	
ivreg2 ldeaths 	(zdiff_viewersHvT = IV_V1_hannity IV_V1_tucker) $X_0_v1 $GEO $POPRACE $AGE $ECON $EDUC $HEALTH $POLITICS $healthcap i.state_fips $CrossSection_Mar28, cl(geography) first 
	mat firststage = e(first)
	estadd loc fstat = string(firststage[8,1], "%9.2f"): col2		
ivreg2 ldeaths 	(zdiff_viewersHvT = IV_V2_hannity)              $X_0_v2 $GEO $POPRACE $AGE $ECON $EDUC $HEALTH $POLITICS $healthcap i.state_fips $CrossSection_Mar28, cl(geography) first 
	mat firststage = e(first)
	estadd loc fstat = string(firststage[8,1], "%9.2f"): col3	
ivreg2 ldeaths 	(zdiff_viewersHvT = IV_V2_hannity IV_V2_tucker) $X_0_v2 $GEO $POPRACE $AGE $ECON $EDUC $HEALTH $POLITICS $healthcap i.state_fips $CrossSection_Mar28, cl(geography) first 
	mat firststage = e(first)
	estadd loc fstat = string(firststage[8,1], "%9.2f"): col4	
ivreg2 ldeaths 	(zdiff_viewersHvT = IV_V3_hannity)              $X_0_v3 $GEO $POPRACE $AGE $ECON $EDUC $HEALTH $POLITICS $healthcap i.state_fips $CrossSection_Mar28, cl(geography) first 
	mat firststage = e(first)
	estadd loc fstat = string(firststage[8,1], "%9.2f"): col5	
ivreg2 ldeaths 	(zdiff_viewersHvT = IV_V3_hannity IV_V3_tucker) $X_0_v3 $GEO $POPRACE $AGE $ECON $EDUC $HEALTH $POLITICS $healthcap i.state_fips $CrossSection_Mar28, cl(geography) first 
	mat firststage = e(first)
	estadd loc fstat = string(firststage[8,1], "%9.2f"): col6	
						

esttab * using "data/working/firststageFstat.tex", replace cells(none) booktabs	///
	   nonotes nomtitles compress alignment(c) nogap noobs nobaselevels label	///
	   stats(fstat, labels(" "))
