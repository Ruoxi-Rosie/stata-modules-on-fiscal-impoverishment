** FISCAL IMPOVERISHMENT AND GAINS OF THE POOR FOR PARTICULAR TAXES OR TRANSFERES A USER SUPPLIES
** Rosie Li, rli4@tulane.edu

** VERSION AND NOTES (changes between versions described under CHANGES)

*! v1.0 nov202016 
*! (beta version)

** CHANGES

** NOTES

** TO DO

************************
** PRELIMINARY PROGRAMS *
************************
// BEGIN returncol (Higgins 2015) 
//  Returns Excel column corresponding to a number
cap program drop returncol
program define returncol, rclass
	confirm integer number `1'
	mata: st_strscalar("col",numtobase26(`1'))
	return local col = col
end // END returncol

// BEGIN _fifgp (Higgins 2015)
//  Calculates fiscal impoverishemnt and fiscal gains of the poor
//   measures for a specific poverty line and two income concepts
capture program drop _fi
program define _fi
	syntax varlist [aweight], ///
		fimatrices(string) fgmatrices(string) ///
		z(real) row(real) col(real) char(varname) ///
		[HEADcount HEADCOUNTPoor TOTal PERcapita NORMalized ]
	local y0 = word("`varlist'",1)
	local y1 = word("`varlist'",2)
	local mat_list headcount headcountpoor total percapita normalized 
	

	local i=0
	foreach mat of local mat_list {
		if "``mat''"!="" {
			local ++i
			local fi_`mat' = word("`fimatrices'",`i')
		}
	}
		
	tempvar h_fi h_fg d_fi d_fg p_fi p_fg i_fi i_fg
	qui gen `h_fi' = (`y1' < `y0' & `y1' < `z')    // identify people who experience FI
	qui gen `d_fi' = min(`y0',`z') - min(`y0',`y1',`z')    // the amount of fi
	qui gen `p_fi' = (`y1' < `z') // for FI it's post fisc poverty
	

	qui levelsof `char', local(level_`char')   // get the label values of variable `char'
	
	qui replace onevar = 1
	qui summ onevar [`weight' `exp']    // the total population
	local pop = r(sum)
	
	
	qui summ onevar if `p_fi' == 1 [`weight' `exp']   // the total population that are poor
	local poor_pop = r(sum)

    
	if "`headcount'"!="" {
		local rowh = `row'
		foreach le of local level_`char' {
			qui summ `h_fi' if `char'==`le' [`weight' `exp']
			if r(sum)!=. matrix `fi_headcount'[`rowh',`col'] = r(sum) / `pop'
			else matrix `fi_headcount'[`rowh',`col'] = -1
			local ++rowh
		}
	}
	if "`headcountpoor'"!="" {
		local rowp = `row'
		foreach le of local level_`char' {
			qui summ `h_fi' if `p_fi'==1 & `char'==`le' [`weight' `exp'] 
			matrix `fi_headcountpoor'[`rowp',`col'] = r(sum) / `poor_pop'
			local ++rowp
		}
	}
	if "`total'`percapita'`normalized'"!="" {
		local rowt = `row'
		foreach le of local level_`char' {
			qui summ `d_fi' if `char'==`le' [`weight' `exp']
			if "`total'"!="" matrix `fi_total'[`rowt',`col'] = r(sum)
			if "`percapita'"!="" matrix `fi_percapita'[`rowt',`col'] = r(mean)
			if "`normalized'"!="" matrix `fi_normalized'[`rowt',`col'] = r(mean)/`z'
			local ++rowt
		}
	}
	
	
end // END _fifgp

capture program drop _fgp
program define _fgp
	syntax varlist [aweight], ///
		fimatrices(string) fgmatrices(string) ///
		z(real) row(real) col(real) char(varname) ///
		[HEADcount HEADCOUNTPoor TOTal PERcapita NORMalized ]
	local y0 = word("`varlist'",1)
	local y1 = word("`varlist'",2)
	local mat_list headcount headcountpoor total percapita normalized 
	
	local i=0
	foreach mat of local mat_list {
		if "``mat''"!="" {
			local ++i
			local fg_`mat' = word("`fgmatrices'",`i')
		}
	}
	
		
	tempvar h_fi h_fg d_fi d_fg p_fi p_fg i_fi i_fg
	qui gen `h_fg' = (`y0' < `y1' & `y0' < `z')    // identify people who experience FGP
	qui gen `d_fg' = min(`y1',`z') - min(`y0',`y1',`z')    // the amount of fpg
	qui gen `p_fg' = (`y0' < `z') 

	qui levelsof `char', local(level_`char')   // get the label values of variable `char'
	
	qui replace onevar = 1
	qui summ onevar [`weight' `exp']    // the total population
	local pop = r(sum)
	
	qui summ onevar if `p_fg' == 1 [`weight' `exp']   // the total population that are poor
	local poor_pop = r(sum)
	       
	if "`headcount'"!="" {
		local rowh = `row'
		foreach le of local level_`char' {
			qui summ `h_fg' if `char'==`le' [`weight' `exp']
			matrix `fg_headcount'[`rowh',`col'] = r(sum) / `pop'
			local ++rowh
		}
	}
	if "`headcountpoor'"!="" {
		local rowp = `row'
		foreach le of local level_`char' {
			qui summ `h_fg' if `p_fg'==1 & `char' == `le' [`weight' `exp'] 
			matrix `fg_headcountpoor'[`rowp',`col'] = r(sum) / `poor_pop'
			local ++rowp
		}
	}
	if "`total'`percapita'`normalized'"!="" {
		local rowt = `row'
		foreach le of local level_`char' {
			qui summ `d_fg' if `char' == `le' [`weight' `exp'], meanonly
			if "`total'"!="" matrix `fg_total'[`rowt',`col'] = r(sum)
			if "`percapita'"!="" matrix `fg_percapita'[`rowt',`col'] = r(mean)
			if "`normalized'"!="" matrix `fg_normalized'[`rowt',`col'] = r(mean)/`z'
			local ++rowt
		}
	}
	
end // END _fifgp


*****************
** ceqfi PROGRAM *
*****************
** For sheet E6. Fisc. Impoverishment
// BEGIN ceqfi (Higgins 2015)
capture program drop ceqfi_by
program define ceqfi_by, rclass 
	version 13.0
	#delimit ;
	syntax 
		[using/]
		[if] [in] [pweight/] 
		[, 
			/** INCOME AND FICAL INTERVENTIONS CONCEPTS: */
			INCome(varname)
			TAXES(varlist)
			TRANSFERS(varlist) 
			
			/** PPP CONVERSION */
			PPP(real -1)
			CPISurvey(real -1)
			CPIBase(real -1)
			YEARly
			MOnthly
			DAily
			/** SURVEY INFORMATION */
			HHid(varname)
			HSize(varname) 
			PSU(varname) 
			Strata(varname)  
			/** EXPORTING TO CEQ MASTER WORKBOOK: */
			SHEETFI(string)
			SHEETFG(string)
			OPEN
			/** POVERTY LINES */
			PL1(real 1.25)
			PL2(real 2.50)
			PL3(real 4.00)
			NATIONALExtremepl(string)   
			NATIONALModeratepl(string)  
			/** INFORMATION CELLS */
			COUNtry(string)
			SURVeyyear(string) /** string because could be range of years */
			AUTHors(string)
			BASEyear(real -1)
			/** OTHER OPTIONS (left all even though NOBin is only relevant one, 
				just to avoid error message if they accidentally specify others as well) */
			NODecile
			NOGroup
			NOCentile
			NOBin
			HEADcount HEADCOUNTPoor TOTal PERcapita NORMalized 
			/** DROP MISSING VALUES */
			IGNOREMissing
			/** DUMMY VARIABLES */
			CHARacteristics(varlist)
		]
	;
	#delimit cr
	
	**********
	** LOCALS *
	**********
	** general programming locals
	local dit display as text in smcl
	local die display as error in smcl
	local command ceqfi_by
	local version 1.0
	`dit' "Running version `version' of `command' on `c(current_date)' at `c(current_time)'" _n "  "

	
	************************
	** PRESERVE AND MODIFY *
	************************
	preserve
	if wordcount("`if' `in'")!=0 quietly keep `if' `in'
	
	** print warning messages 
	local warning "Warnings"
	
	** characteristics
	if "`characteristics'"=="" `die' "At least one characteristic variable must be specified."
	foreach var in `characteristics' {
		local d_`var': var label `var'
		su `var'
		qui levelsof `var', local(level_`var') 
	}
	
	** income, taxes and transfers
	if "`income'"=="" `die' "The income variable must be specified."
	if "`taxes'" =="" & "`transfers'"=="" ///
	`die' "At least one tax or transfer needs to be specfied for results to be produced."
	
	local fi_fiscinter
	local fi_fiscinter
	
	foreach ta in `taxes' {
		local d_`ta' : var label `ta'
		if "`d_`ta''"=="" {  // ie, if the var didnt have a label
			local d_`ta' `ta'
			`dit' "Warning: variable `ta' not labeled"
			local warning `warning' "Warning: variable `pr' not labeled."
		}
		
		gen inc_`ta' = 0
		label var inc_`ta' "`d_`ta''"
		qui summ `ta'
		// Subtract if taxes or transfers are stored as postive values
		if r(mean)>0 {
			qui replace inc_`ta' = `income' - `ta'   
		}
		// Add if taxes or transfers are stored as negative values
		else {
			qui replace inc_`ta' = `income' + `ta' 
		}
		local fi_fiscinter `fi_fiscinter' inc_`ta'
	}
	
	foreach ta in `transfers' {
		local d_`ta' : var label `ta'
		if "`d_`ta''"=="" {  // ie, if the var didnt have a label
			local d_`ta' `ta'
			`dit' "Warning: variable `ta' not labeled"
			local warning `warning' "Warning: variable `pr' not labeled."
		}
		
		gen inc_`ta' = 0
		label var inc_`ta' "`d_`ta''"
		qui summ `ta'
		// Subtract if taxes or transfers are stored as postive values
		if r(mean)>0 {
			qui replace inc_`ta' = `income' + `ta'   
		}
		// Add if taxes or transfers are stored as negative values
		else {
			qui replace inc_`ta' = `income' - `ta' 
		}
		local fg_fiscinter `fg_fiscinter' inc_`ta'
	}

	** poverty lines
	local povlines `pl1' `pl2' `pl3' `nationalextremepl' `nationalmoderatepl' `otherextremepl' `othermoderatepl'
	local plopts pl1 pl2 pl3 nationalextremepl nationalmoderatepl otherextremepl othermoderatepl
	foreach p of local plopts {
		if "``p''"!="" {
			cap confirm number ``p'' // `p' is the option name eg pl125 so ``p'' is what the user supplied in the option
			if !_rc scalar _`p'_isscalar = 1 // !_rc = ``p'' is a number
			else { // if _rc, i.e. ``p'' not number
				cap confirm numeric variable ``p''
				if _rc {
					`die' "Option " in smcl "{opt `p'}" as error " must be specified as a scalar or existing variable."
					exit 198
				}
				else scalar _`p'_isscalar = 0 // else = if ``p'' is numeric variable
			}
		}
	}
	
	** results
	local supercols_all headcount headcountpoor total percapita normalized 
	if "`headcount'`headcontpoor'`total'`percapita'`normalized'"=="" {
		foreach x of local supercols_all {
			local `x' "`x'"
		}
	}
	local supercols `headcount' `headcountpoor' `total' `percapita' `normalized' 


	*****************
	** PARSE OPTIONS *
	*****************	
	** ado file specific
	if "`sheetfi'"=="" local sheetfi "FI. By" // default name of sheet in Excel files
	if "`sheetfg'"=="" local sheetfg "FGP. By"
	
	
	
	** ppp conversion
	if (`ppp'==-1 & `cpisurvey'==-1 & `cpibase'==-1) {
		local _ppp = 0
		`dit' "Option {bf:ppp} required."
		exit 198
	}
	else local _ppp = 1
	if (`_ppp' & min(`ppp',`cpisurvey',`cpibase')==-1) {
		`die' "To convert to PPP, must provide {bf:ppp}, {bf:cpisurvey}, and {bf:cpibase} options"
		exit 198
	}
	if (`_ppp'==0 & wordcount("`daily' `monthly' `yearly'")>0) {
		`die' "{bf:daily}, {bf:monthly}, or {bf:yearly} options require use of {bf:ppp}, {bf:cpisurvey}, and {bf:cpibase}"
		exit 198
	}
	if (`_ppp' & wordcount("`daily' `monthly' `yearly'")==0) {
		`dit' "Warning: {bf:daily}, {bf:monthly}, or {bf:yearly} options not specified; variables assumed to be in {bf:yearly} local currency units"
		local warning `warning' "Warning: daily, monthly, or yearly options not specified; variables assumed to be in yearly local currency units."
		local yearly yearly
	}
	if (wordcount("`daily' `monthly' `yearly'")>1) {
		`die' "{bf:daily}, {bf:monthly}, and {bf:yearly} options are exclusive"
		exit 198
	}
	if ("`daily'"!="")        local divideby = 1
	else if ("`monthly'"!="") local divideby = 365/12
	else if ("`yearly'"!="")  local divideby = 365
	
	** NO... options
	if wordcount("`nogroup' `nobin'")==2 {
		`die' "Both options {bf:nogroup} and {bf:nobin} specified; no results to produce"
		exit 198
	}
	
	if "`nodecile'"=="" local _dec dec
	if "`nogroup'"=="" local _group group
	if "`nocentile'"=="" local _cent cent
	if "`nobin'"=="" local _bin bin
	
	** make sure using is xls or xlsx
	cap putexcel clear
	if `"`using'"'!="" {
		qui di " // for Notepad++ syntax highlighting
		if !strpos(`"`using'"' /* " */ , ".xls") {
			`die' "File extension must be .xls or .xlsx to write to an existing CEQ Master Workbook (requires Stata 13 or newer)"
			exit 198
		}
		confirm file `"`using'"'
		qui di "
	}
	else { // if "`using'"==""
		`dit' "Warning: No file specified with {bf:using}; results saved in {bf:return list} but not exported to Output Tables"
	}
	if strpos(`"`using'"'," ")>0 & "`open'"!="" { // has spaces in filename
		qui di "
		`dit' `"Warning: `"`using'"' contains spaces; {bf:open} option will not be executed. File can be opened manually after `command' runs."'
		local open "" // so that it won't try to open below
	}	

	** negative incomes
	qui count if `income'<0 // note `v' is e.g. m, ``v'' is varname
	if r(N) `dit' "Warning: `r(N)' negative values of ``v''"
	if r(N) local warning `warning' "Warning: `r(N)' negative values of ``v''"

	
	** weight (if they specified hhsize*hhweight type of thing)
	if strpos("`exp'","*")> 0 { // TBD: what if they premultiplied w by hsize?
		`die' "Please use the household weight in {weight}; this will automatically be multiplied by the size of household given by {bf:hsize}"
		exit
	}
	
	** hsize and hhid
	if wordcount("`hsize' `hhid'")!=1 {
		`die' "Must exclusively specify {bf:hsize} (number of household members for household-level data) or "
		`die' "{bf:hhid} (unique household identifier for individual-level data)"
		exit 198
	}
	
	************************
	** PRESERVE AND MODIFY *
	************************
	
	** collapse to hh-level data
	if "`hsize'"=="" { // i.e., it is individual-level data
		tempvar members
		sort `hhid', stable
		qui by `hhid': gen `members' = _N // # members in hh 
		qui by `hhid': drop if _n>1 // faster than duplicates drop
		local hsize `members'
	}
	
	***********************
	** SVYSET AND WEIGHTS *
	***********************
	cap svydes
	scalar no_svydes = _rc
	if !_rc qui svyset // gets the results saved in return list
	if "`r(wvar)'"=="" & "`exp'"=="" {
		`dit' "Warning: weights not specified in svydes or the command"
		`dit' "Hence, equal weights (simple random sample) assumed"
		local warning `warning' "Warning: weights not specified in svydes or the command. Hence, equal weights (simple random sample) assumed."
	}
	else {
		if "`exp'"=="" & "`r(wvar)'"!="" local w `r(wvar)'
		if "`exp'"!="" local w `exp'
		if "`w'"!="" {
			tempvar weightvar
			qui gen double `weightvar' = `w'*`hsize'
			local w `weightvar'
		}
		else local w "`hsize'"
		
		if "`w'"!="" {
			local pw "[pw = `w']"
			local aw "[aw = `w']"
		}
		if "`exp'"=="" & "`r(wvar)'"!="" {
			local weight "pw"
			local exp "`r(wvar)'"
		}
	}
	else if "`r(su1)'"=="" & "`psu'"=="" {
		di as text "Warning: primary sampling unit not specified in svydes or the `command' command's psu() option"
		di as text "P-values will be incorrect if sample was stratified"
		local warning `warning' "Warning: primary sampling unit not specified in svydes or the `command' command's psu() option. P-values will be incorrect if sample was stratified."
	}
	if "`psu'"=="" & "`r(su1)'"!="" {
		local psu `r(su1)'
	}
	if "`strata'"=="" & "`r(strata1)'"!="" {
		local strata `r(strata1)'
	}
	if "`strata'"!="" {
		local opt strata(`strata')
	}
	** now set it:
	if "`exp'"!="" qui svyset `psu' `pw', `opt'
	else           qui svyset `psu', `opt'
	
	**************************
	** VARIABLE MODIFICATION *
	**************************
	
	** keep the variables used in ceqdes  
	foreach pl of local plopts {
		if "``pl''"!="" {
			if _`pl'_isscalar == 0 {
				local pl_tokeep `pl_tokeep' ``pl''
			}
		}
	}	
	#delimit ;
	local relevar `income' `taxes' `transfers' `fi_fiscinter' `fg_fiscinter' `characteristics'
				  `w' `psu' `strata' `exp'		
				  `pl_tokeep'
	;
	#delimit cr
	quietly keep `relevar' 
	
	** missing income concepts
	foreach var of local varlist {
		qui count if missing(`var')  
		if "`ignoremissing'"=="" {
			if r(N) {
				`die' "Missing values not allowed; `r(N)' missing values of `var' found" 
				exit 198
			}
		}
		else {
			if r(N) {
				qui drop if missing(`var')
				`dit' "Warning: `r(N)' observations that are missing `var' were dropped because the user specified {bf:ignoremissing}"
				local warning `warning' "Warning: `r(N)' observations that are missing `var' were dropped because the user specified the ignoremissing option."
			}
		}
    }
	
	** PPP converted variables
	if (`_ppp') {
		local ppp_calculated = `ppp'*(`cpisurvey'/`cpibase')
		tempvar inc_ppp
		if "`income'"!="" qui gen `inc_ppp' = (`income'/`divideby')*(1/`ppp_calculated')
		foreach sheet in fi fg {
			foreach y1 of local `sheet'_fiscinter {
				tempvar `y1'_ppp
				qui gen ``y1'_ppp' = (`y1'/`divideby')*(1/`ppp_calculated')
			}
		}
	}	

	
	** temporary variables
	tempvar one
	qui gen `one' = 1
	gen onevar = 1
	
	***************************
	** INCOME GROUPS AND BINS *
	***************************

		if "`income'"!="" {
			** bins and groups
			if `_ppp' {
				if "`nobin'"=="" {
					tempvar inc_bin 
					qui gen `inc_bin' = .
					local i=1
					local bl = 0
					while `bl' < 10 {
						local bh = round(`bl' + 0.05, .01) // Stata was introducing rounding errors
						qui replace `inc_bin' = `i' if `inc_ppp' >= `bl' & `inc_ppp' < `bh'
						local bl = `bh'
						local ++i
					}
					while `bl' < 50 { // it is >= 10 from previous loop
						local bh = round(`bl' + 0.25, .01) // Stata was introducing rounding errors
						qui replace `inc_bin' = `i' if `inc_ppp' >= `bl' & `inc_ppp' < `bh'
						local bl = `bh'
						local ++i
					}	
					qui replace `inc_bin' = `i' if `inc_ppp' >= 50 & `inc_ppp' < 100
					local ++i
					qui replace `inc_bin' = `i' if `inc_ppp' >= 100
					local count_bins = `i'
				}
			}
		}
	
	
	local group = 6
	if `_ppp' & "`nobin'"=="" local bin = `count_bins' // need if condition here b/c o.w. `count_bins' doesn't exist	
	
	**********************
	** CALCULATE RESULTS *
	**********************
	local already
	
	// Poverty results frontmatter
	if "`nogroup'"=="" {
		local pov_cols = 1 //wordcount("`taxes'") + wrodcount("`transfers'")  //!
		local pov_rows = 5 // socioeconomic groups
		matrix poverty = J(`pov_rows',`pov_cols',.)   
		local col = 1
		
		if "`income'"!="" {
			local zz=0
			foreach p of local plopts {
				local ++zz
				tempvar poor
				if "``p''"!="" {
					if substr("`p'",1,2)=="pl" { // these are the PPP lines
						local _pline = ``p''
						local y0touse `inc_ppp'
					}
					else if _`p'_isscalar==1 { // if pov line is scalar (not PPP line)
						local _pline = ``p'' // set `_pline' as that scalar and
						local y0touse `income'   // use original income variable
					}
					else if _`p'_isscalar==0 { // if pov line is variable,
						tempvar `y0'_normalized  // create temporary variable that is income...
						qui gen ``y0'_normalized' = `income'/``p'' // normalized by pov line
						local y0touse ``y0'_normalized' // use normalized income in the calculations
						local _pline = 1                       // and normalized pov line is 1
					}
					gen `poor' = (`y0touse' < `_pline')
					summ `poor' `aw', meanonly
					matrix poverty[`zz',`col'] = r(mean)
				}
			}	
		}
	}
	
	// Fiscal impoverishment 
		//local already `already' `y0'
		//local y1list : list alllist_no_t - already // varlist has the variable names
	foreach sheet in fi fg {
		foreach suf in "" "_bins" {
			local `sheet'_mcs`suf' // set locals blank before loop below
		}
	}
		
	// this is outside of if condition because we need blank matrices for the rowsof() later
	if "`fi_fiscinter'"!="" local fi_ncols = wordcount("`fi_fiscinter'")
	else local fi_ncols = 0
	if "`fg_fiscinter'"!="" local fg_ncols = wordcount("`fg_fiscinter'")
	else local fg_ncols = 0
	local ncols = `fi_ncols' + `fg_ncols'
	
	foreach ss of local supercols_all {
		if "`fi_fiscinter'"!="" matrix `ss'_fi_income = J(200,`fi_ncols',.) // a patch since later use rowsof() even if they specify suboptions
		if "`fg_fiscinter'"!="" matrix `ss'_fg_income = J(200,`fg_ncols',.)
	}
	
	local fi_mcs
	local fg_mcs
	if "`income'"!="" & "`fi_fiscinter'"!="" { 
		foreach ss of local supercols_all {
			foreach sheet in fi fg {
				if "``ss''"!="" {
					local `sheet'_mcs ``sheet'_mcs'  `ss'_`sheet'_income
				}
			}
		}
		
		local yy=0
		foreach y1 of local fi_fiscinter {
			local ++yy
			if "`y1'"!="" {
				// GROUPS 
				if "`nogroup'"=="" {
					local zz = 1
					foreach p of local plopts {
						if "``p''"!="" {
							foreach char in `characteristics' {
								local `char'_count = wordcount("`level_`char''")
								if substr("`p'",1,2)=="pl" { // these are the PPP lines
									local _pline = ``p''
									local y0touse `inc_ppp'
									local y1touse ``y1'_ppp'
								}
								else if _`p'_isscalar==1 { // if pov line is scalar (not PPP line)
									local _pline = ``p'' // set `_pline' as that scalar and
									local y0touse `income'   // use original income variable
									local y1touse `y1'
								}
								else if _`p'_isscalar==0 { // if pov line is variable,
									tempvar inc_normalized  // create temporary variable that is income...
									qui gen `inc_normalized' = `income'/``p'' // normalized by pov line
									local y0touse `inc_normalized' // use normalized income in the calculations
									tempvar `y1'_norm
									qui gen ``y1'_norm' = `y1'/``p''
									local y1touse  ``y1'_norm'
									local _pline = 1                       // and normalized pov line is 1
								}
								_fi `y0touse' `y1touse' `aw', ///
								fimatrices(`fi_mcs') fgmatrices(`fg_mcs') ///
								z(`_pline') row(`zz') col(`yy') char(`char') ///
								`supercols'
								local zz = `zz' + ``char'_count' + 1
								local start_`char' = `zz'
							}	
							local zz = `zz' + 1
							//local `p'_row = `zz'    // `zz' is the row number within the matrice, + 7 to get the row number within MWB
						}
					}
				}
			}
		}
	}
	
	if "`income'"!="" & "`fg_fiscinter'"!=""  { 

		local yy=0
		foreach y1 of local fg_fiscinter {
			local ++yy
			if "`y1'"!="" {
				// GROUPS 
				if "`nogroup'"=="" {
					local zz = 1
					foreach p of local plopts {
						if "``p''"!="" {
							foreach char in `characteristics' {
								if substr("`p'",1,2)=="pl" { // these are the PPP lines
									local _pline = ``p''
									local y0touse `inc_ppp'
									local y1touse ``y1'_ppp'
								}
								else if _`p'_isscalar==1 { // if pov line is scalar (not PPP line)
									local _pline = ``p'' // set `_pline' as that scalar and
									local y0touse `income'   // use original income variable
									local y1touse `y1'
								}
								else if _`p'_isscalar==0 { // if pov line is variable,
									tempvar inc_normalized  // create temporary variable that is income...
									qui gen `inc_normalized' = `income'/``p'' // normalized by pov line
									local y0touse `inc_normalized' // use normalized income in the calculations
									tempvar `y1'_norm
									qui gen ``y1'_norm' = `y1'/``p''
									local y1touse  ``y1'_norm'
									local _pline = 1                       // and normalized pov line is 1
								}
								_fgp `y0touse' `y1touse' `aw', ///
								fimatrices(`fi_mcs') fgmatrices(`fg_mcs') ///
								z(`_pline') row(`zz') col(`yy') char(`char') ///
								`supercols'
								local zz = `zz' + ``char'_count' + 1
								local start_`char' = `zz'
							}	
							local zz = `zz' + 1
						     
						}
					}
				}
			}
		}
	}


	*****************
	** SAVE RESULTS *
	*****************
	if `"`using'"'!="" {
		qui di "
		`dit' `"Writing to "`using'"; may take several minutes"'
		local startcol_o = 4 // this one will stay fixed (column D)

		// Print information
		local date `c(current_date)'		
		local titlesprint
		local titlerow = 3
		local titlecol = 1
		local titlelist country surveyyear authors date ppp baseyear cpibase cpisurvey ppp_calculated
		foreach title of local titlelist {
			returncol `titlecol'
			if "``title''"!="" & "``title''"!="-1" ///
				local  titlesprint `titlesprint' `r(col)'`titlerow'=("``title''")
			local titlecol = `titlecol' + 2
		}

		// Print version number on Excel sheet
		local versionprint A4=("Results produced by version `version' of `command' on `c(current_date)' at `c(current_time)'")
		
		// Export to Excel (matrices)
		local startcol_o = 4
		
		** Poverty results
		if "`nogroup'"=="" {
			local startrow = 8
			returncol `startcol_o'
			foreach sheet in fi fg {
				local resultset_`sheet' `resultset_`sheet'' `r(col)'`startrow'=matrix(poverty)
			}
		}
		
		** Fiscal impoverishment results
		local vertincrement = 8
		local horzincrement = 3
		local rgroup = 17 // row where first group starts (note no deciles on this sheet)
		local rbin = `rgroup' + `vertincrement'
				
		if "`fi_fiscinter'"!="" {
			local startcol = `startcol_o'
			foreach ss of local supercols_all {
				returncol `startcol'
				if "`nogroup'"=="" & "``ss''"!="" local resultset_fi `resultset_fi' `r(col)'`rgroup'=matrix(`ss'_fi_income) 
				local startcol = `startcol' + `fi_ncols' + `horzincrement'
			}
		}
		
		if "`fg_fiscinter'"!="" {
			local startcol = `startcol_o'
			foreach ss of local supercols_all {
				returncol `startcol'
				if "`nogroup'"=="" & "``ss''"!="" local resultset_fg `resultset_fg' `r(col)'`rgroup'=matrix(`ss'_fg_income) 
				local startcol = `startcol' + `fg_ncols' + `horzincrement'
			}
		}
		
		// Export to Excel (intervention names)
		local i = 0
		foreach int of local fi_fiscinter {
			local d_`int' : var label `int'
			local startcol = `startcol_o' + `i'
			foreach ss of local supercols_all {
				returncol `startcol'
				local fiscinter_fi `fiscinter_fi' `r(col)'`=`rgroup'-1' = ("`d_`int''") 
				local startcol = `startcol' + `fi_ncols' + `horzincrement'
			}
			local ++i
		}
		
		local i = 0
		foreach int of local fg_fiscinter {
			local d_`int' : var label `int'
			local startcol = `startcol_o' + `i'
			foreach ss of local supercols_all {
				returncol `startcol'
				local fiscinter_fg `fiscinter_fg' `r(col)'`=`rgroup'-1' = ("`d_`int''") 
				local startcol = `startcol' + `fg_ncols' + `horzincrement'
			}
			local ++i
		}
		
		// Export to Excel  (indicator titles)
		
		local startcol = `startcol_o' 
		foreach ss of local supercols_all {
			returncol `startcol'
			local indicator_fi `indicator_fi' `r(col)'`=`rgroup'-2' = ("`ss'") 
			local startcol = `startcol' + `fi_ncols' + `horzincrement'
		}
		
		local startcol = `startcol_o' 
		foreach ss of local supercols_all {
			returncol `startcol'
			local indicator_fg `indicator_fg' `r(col)'`=`rgroup'-2' = ("`ss'") 
			local startcol = `startcol' + `fg_ncols' + `horzincrement'
		}
			

		// Export to Excel (poverty lines)
	
		local hicol = 1
		
		local totalchar = 0
		foreach char in `characteristics' {
			local totalchar = `totalchar' + ``char'_count'
		}
		foreach p of local plopts {
			if "``p''"!="" {
				local first_pl = word("`plopts'",1)
				local sec_pl = word("`plopts'",2)
				if "`p'" == "`first_pl'" local `p'_row = 17
				if "`p'" == "`first_pl'" local start_prow = 17
				if "`p'" != "`first_pl'" local `p'_row = `start_prow' + `totalchar' + 1 + wordcount("`characteristics'")
				if "`p'" != "`first_pl'" local start_prow = ``p'_row'
			}
		}
		
		local pl_fi
		local pl_fg
		foreach sheet in fi fg {
			foreach p of local plopts {
				if "``p''"!="" {
					returncol `hicol'
					if substr("`p'",1,2)=="pl" local pl_`sheet' `pl_`sheet'' `r(col)'``p'_row' = (``p'')
					else local pl_`sheet' `pl_`sheet'' `r(col)'``p'_row' = ("`p'")
				}
			}
		}
		
		// Export to Excel (characteristics variable label values)
		local char_fi
		local char_fg
		local charcol = 3
		returncol `charcol'
		foreach sheet in fi fg {
			foreach p of local plopts {
				if "``p''"!="" {
					local count_char = 0
					local start_char = ``p'_row' 
					foreach char in `characteristics' {
						local start_`char' = `start_char' 
						local charname_`sheet' `charname_`sheet''  B`start_`char'' = ("`d_`char''")
						local start_char = `start_char' + wordcount("`level_`char''") + 1
						local count_`char' = `start_`char''
						foreach le of local level_`char' {
							local char_`sheet' `char_`sheet''   `r(col)'`count_`char'' = (`le')
							local ++count_`char'
						}
						//local count_char = ``char'_count' + 1      // this is the number of rows (number of label values) needed for one particular characteristic
					}
				}
			}
		}
		
		
		// Print warning message on Excel sheet 
		local warningrow = 387
		local warningcount = -1
		foreach x of local warning {
			local warningprint `warningprint' A`warningrow'=("`x'")
			local ++warningrow
			local ++warningcount
		}
		// overwrite the obsolete warning messages if there are any
		forval i=0/100 {
			local warningprint `warningprint' A`=`warningrow'+`i''=("")
		}
		// count warning messages and print at the top of MWB
		local warningprint `warningprint' A5=("`warningcount' important warning messages are printed starting on row 387.") 

		// putexcel
		foreach sheet in fi fg {
			qui putexcel `titlesprint' `versionprint' `resultset_`sheet'' `warningprint' `fiscinter_`sheet'' `pl_`sheet'' `char_`sheet'' `charname_`sheet'' `indicator_`sheet'' ///
			using `"`using'"', modify keepcellformat sheet("`sheet`sheet''")
			qui di "
		}
	}

	*********
	** OPEN *
	*********
	if "`open'"!="" & "`c(os)'"=="Windows" {
		shell start `using' // doesn't work with "" or `""' so I already changed `open' to "" if using has spaces, 
	}
	else if "`open'"!="" & "`c(os)'"=="MacOSX" {
		shell open `using'
	}
	else if "`open'"!="" & "`c(os)'"=="Unix" {
		shell xdg-open `using'
	}
	
	*************
	** CLEAN UP *
	*************
	quietly putexcel clear
	restore // note this also restores svyset
	
end	// END ceqfi
