************************************************************
******************** YLILI COMPUTATION *********************

local x "SOYN YSMR YWPR_EPMP SYIE SYEO SYSAF YTRUR YVER SYNSE YIR"

* rescale indicators "

foreach y of local x {
gen n_`y'= 100-`y'
} 

gen n_RUR= 100-(((RUR-1)/(10-1))*100) 
replace n_RUR=100 if RUR<1
replace n_RUR=0 if RUR>10
replace n_RUR=. if missing(RUR) 

gen n_HTS= (((HTS-300)/(625-300))*100) 

local z "SOYN YSMR YWPR_EPMP SYIE SYEO SYSAF YTRUR YVER SYNSE YIR RUR HTS"

foreach a of local z {
gen y_`a'= 1 if !missing(`a')
} /* create a dummy variable for each indicator = 1 if not missing, 0 otherwise. */

egen transition= rmean(n_SOYN n_RUR n_YSMR)
egen y_transition= rowtotal(y_SOYN y_RUR y_YSMR)
replace transition=. if y_transition<2 /* replace to missing if number of indicators < 2 */

egen working_conditions= rmean(n_YWPR_EPMP n_SYIE n_SYEO n_SYSAF n_YTRUR n_YVER)
egen y_working_conditions= rowtotal(y_YWPR_EPMP y_SYIE y_SYEO y_SYSAF y_YTRUR y_YVER)
replace working_conditions=. if y_working_conditions<3 /* replace to missing if number of indicators < 3 */

egen education= rmean(n_SYNSE n_YIR n_HTS)
egen y_education= rowtotal(y_SYNSE y_YIR y_HTS)
replace education=. if y_education<2 /* replace to missing if number of indicators < 2 */

egen YLILI= rmean(transition working_conditions education)
replace YLILI=. if (missing(transition)|missing(working_conditions)|missing(education)) /* replace to missing if missing in at least one of the 3 dimensions  */
