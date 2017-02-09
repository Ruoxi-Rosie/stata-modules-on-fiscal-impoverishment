** FISCAL IMPOVERISHMENT AND GAINS OF THE POOR POLICY SIMULATION, ADAPTED FROM CEQFI

** VERSION AND NOTES (changes between versions described under CHANGES)

*! v1.0 jan05016 
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
capture program drop _fifgp, rclass
program define _fifgp, rclass
	syntax varlist [aweight], ///
		fimatrices(string) fgmatrices(string) ///
		z(real) row(real) col(real) ///
		[HEADcount HEADCOUNTPoor TOTal PERcapita NORMalized ]
	local y0 = word("`varlist'",1)
	local y1 = word("`varlist'",2)
	local mat_list headcount headcountpoor total percapita normalized 
	foreach sheet in fi fg {
		local i=0
		foreach mat of local mat_list {
			if "``mat''"!="" {
				local ++i
				local `sheet'_`mat' = word("``sheet'matrices'",`i')
			}
		}
	}
		
	tempvar h_fi h_fg d_fi d_fg p_fi p_fg i_fi i_fg
	qui gen `h_fi' = (`y1' < `y0' & `y1' < `z') 
	qui gen `h_fg' = (`y0' < `y1' & `y0' < `z')
	qui gen `d_fi' = min(`y0',`z') - min(`y0',`y1',`z')
	qui gen `d_fg' = min(`y1',`z') - min(`y0',`y1',`z')
	qui gen `p_fi' = (`y1' < `z') // for FI it's post fisc poverty
	qui gen `p_fg' = (`y0' < `z') 
	qui gen `i_fi' = `d_fi'/`y0'
	qui gen `i_fg' = `d_fg'/`y0'
	
	foreach sheet in fi fg {        
		if "`headcount'"!="" {
			qui summ `h_`sheet'' [`weight' `exp'], meanonly
			matrix ``sheet'_headcount'[`row',`col'] = r(mean)
			//return scalar `sheet'headcount = r(mean)
		}
		if "`headcountpoor'"!="" {
			qui summ `h_`sheet'' if `p_`sheet''==1 [`weight' `exp'] 
			matrix ``sheet'_headcountpoor'[`row',`col'] = r(mean)
			//return scalar headcountp = r(mean)
		}
		if "`total'`percapita'`normalized'"!="" {
			qui summ `d_`sheet'' [`weight' `exp'], meanonly
			if "`total'"!="" matrix ``sheet'_total'[`row',`col'] = r(sum)
			//return scalar total = r(sum)
			if "`percapita'"!="" matrix ``sheet'_percapita'[`row',`col'] = r(mean)
			//return scalar percapita = r(mean)
			if "`normalized'"!="" matrix ``sheet'_normalized'[`row',`col'] = r(mean)/`z'
			//return scalar normalized r(mean)/`z'
		}
	}
end // END _fifgp

*****************
** ceqfi PROGRAM *
*****************
** For sheet E6. Fisc. Impoverishment
// BEGIN ceqfi (Higgins 2015)
capture program drop ceqfis_sim
program define ceqfis_sim, rclass 
	version 13.0
	#delimit ;
	syntax 
		[using/]
		[if] [in] [pweight/] 
		[, 
			/** INCOME CONCEPTS: */
			INCome(varname)
			TAX1(varname)
			TAX2(varname)
			TAX3(varname)
			TRANSFER(varname)
            TAXPROP1(real -5)			
			TAXPROP2(real -5)
			TAXPROP3(real -5)
			TRANSFERScale(real -5)
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

			/** DROP MISSING VALUES */
			IGNOREMissing
		]
	;
	#delimit cr
	
	**********
	** LOCALS *
	**********
	** general programming locals
	local dit display as text in smcl
	local die display as error in smcl
	local command ceqfis_sim
	local version 2.5
	`dit' "Running version `version' of `command' on `c(current_date)' at `c(current_time)'" _n "   (please report this information if reporting a bug to rli4@tulane.edu)"
	
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

	** print warning messages 
	local warning "Warnings"
	
	** create a local with original and new fiscal intervention
	local fiscinter `transfer' `tax1' `tax2' `tax3'
		
	
	************************
	** PRESERVE AND MODIFY *
	************************
	preserve
	if wordcount("`if' `in'")!=0 quietly keep `if' `in'
	
	** missing income concepts
	foreach var in `fiscinter' `income' {
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
	
	** income, taxes and transfers check
	if "`income'"=="" {
		`die' "The income variable must be specified."
		exit 198
	}
	if "`tax1'" =="" | "`transfer'"=="" {
		`die' "At least one tax and transfer needs to be specfied for results to be produced."
		exit 198
	}
	
	if "`tax2'" =="" & "`tax3'" !="" {
		`die' "Tax 2 must be specified before tax3 can be specified."             // so the simulation description can be properly printed to excel
		exit 198
	}
	
	if "`transfer'"!="" & `transferscale'== -5   {
		`die' "The sign and magnitude of transfers need to be specified."
		exit 198
	}
	
	if `transferscale' == 0 `die' "The transfer scale cannot be 0."
	
	qui summ `transfer' if `transfer' < 0     // check whether all observations have positive values of transfers
	local transfercheck = r(mean)
	if "`transfercheck'" != "." {
		`die' "Transfer for all observations must take on zero or positive values."
		exit 198
	}
	
	forval i = 1/3 {
		if "`tax`i''"!="" {
			qui summ `tax`i''
			if r(mean) > 0 {
				qui summ `tax`i'' if `tax`i'' < 0
				local tax`i'check = r(mean)
				if "`tax`i'check'" != "." {
					`die' "Taxes for all observations must take on zero or positive values."
					exit 198
				}
			}
			if r(mean) < 0 {
				qui summ `tax`i'' if `tax`i'' > 0
				local tax`i'check = r(mean)
				if "`tax`i'check'" != "." {
					`die' "Taxes for all observations must take on zero or negative values."
					exit 198
				}
				replace `tax`i'' = - `tax`i''             // doesn't matter since we preserved. Replace to make calculations uniform
			}
		}
	}
	
	** funding allocation
	if `transferscale' == 0 `die' "Transfer scale must be specified as a nonzero real number."
	if `transferscale' > 0  {                    /* if the transfer program is to expand */
		tempvar budgetvar
		qui gen double `budgetvar' = `transfer' * `transferscale'
		qui summ `budgetvar'
		local budget  = r(sum)
		tempvar ntransfer
		qui gen double `ntransfer' = `transfer' * (1 + `transferscale')                     
		local fiscinter `fiscinter' `ntransfer'
		forval i = 1/3 {
			if "`tax`i''"!="" {
				local budget`i' = `budget' * `taxprop`i''   // this calculates the total amount that a particular tax will increase or decrease by
				qui summ `tax`i''
				local rate`i' = `budget`i'' / `r(sum)'
				tempvar ntax`i'
				qui gen double `ntax`i'' = `tax`i'' * (1 + `rate`i'')
				local fiscinter `fiscinter' `ntax`i''
			}
		}
	}
	if `transferscale' < 0 {
		tempvar budgetvar
		qui gen double `budgetvar' = `transfer' * (-`transferscale' )  // here we make the budget variable positive by adding a negative sign
		qui summ `budgetvar'
		local budget = r(sum) 
		tempvar ntransfer		
		qui gen double `ntransfer' = `transfer' * (1 + `transferscale')  
		local fiscinter `fiscinter' `ntransfer'
		forval i = 1/3 {
			if "`tax`i''"!="" {
				local budget`i' = `budget' * `taxprop`i''   // this calculates the total amount that a particular tax will increase or decrease by
				qui summ `tax`i''             // we previously convert all tax variables to positive values
				local rate`i' = `budget`i'' / `r(sum)'   
				tempvar ntax`i'				
				qui gen double `ntax`i'' = `tax`i'' * (1 - `rate`i'')         // - because tax is simulated to decrease when a transfer is defunded
				local fiscinter `fiscinter' `ntax`i''
			}
		}
	}


	*****************
	** PARSE OPTIONS *
	*****************	
	** ado file specific
	if "`sheetfi'"=="" local sheetfi "E5. Fisc. Impoverishment" // default name of sheet in Excel files
	if "`sheetfg'"=="" local sheetfg "E6. Fisc. Gains to the Poor"
	
	
	
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
	foreach v of local alllist {
		if "``v''"!="" {
			qui count if ``v''<0 // note `v' is e.g. m, ``v'' is varname
			if r(N) `dit' "Warning: `r(N)' negative values of ``v''"
			if r(N) local warning `warning' "Warning: `r(N)' negative values of ``v''"
		}
	}	
	
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
	local relevar `fiscinter' `income'     
				  `w' `psu' `strata' `exp'		
				  `pl_tokeep'
	;
	#delimit cr
	quietly keep `relevar' 

	
	** temporary variables
	tempvar one
	qui gen `one' = 1
	
	
	
	**********************
	** CALCULATE RESULTS *
	**********************
	local already
	
	// Generate the new post-policy income concept
	tempvar nincome
	qui gen `nincome' = `income' + `tax1' - `transfer' - `ntax1' + `ntransfer'
	if "`tax2'"!="" {            /* if the user specified the second tax option  */
		qui replace `nincome' = `nincome' + `tax2' - `ntax2'
	}
	if "`tax3'"!="" {
		qui replace `nincome' = `nincome' + `tax3' - `ntax3'
	}	
	
	** PPP converted variables
	if (`_ppp') {
		local ppp_calculated = `ppp'*(`cpisurvey'/`cpibase')
		tempvar income_ppp
		tempvar nincome_ppp
		qui gen `income_ppp' = (`income'/`divideby')*(1/`ppp_calculated')
		qui gen `nincome_ppp' = (`nincome'/`divideby')*(1/`ppp_calculated')
	
	}	
	
	// Poverty results frontmatter
	if "`nogroup'"=="" {
		local pov_cols = 2
		local pov_rows = 5 // socioeconomic groups
		matrix poverty = J(`pov_rows',`pov_cols',.)
		local col = 0
		
		local inclist income nincome 
		foreach y0 in income nincome {
			local ++col
			if "``y0''"!="" {
				local zz=0
				foreach p of local plopts {
					local ++zz
					tempvar poor
					if "``p''"!="" {
						if substr("`p'",1,2)=="pl" { // these are the PPP lines
							local _pline = ``p''
							local y0touse ``y0'_ppp'
						}
						else if _`p'_isscalar==1 { // if pov line is scalar (not PPP line)
							local _pline = ``p'' // set `_pline' as that scalar and
							local y0touse ``y0''   // use original income variable
						}
						else if _`p'_isscalar==0 { // if pov line is variable,
							tempvar `y0'_normalized  // create temporary variable that is income...
							qui gen ``y0'_normalized' = ``y0''/``p'' // normalized by pov line
							local y0touse ``y0'_normalized' // use normalized income in the calculations
							local _pline = 1                       // and normalized pov line is 1
						}
						gen `poor' = (`y0touse' < `_pline')
						qui summ `poor' `aw', meanonly
						matrix poverty[`zz',`col'] = `r(mean)'
					}
				}	
			}
		}
	}
	
	// Fiscal impoverishment 
	foreach y0 of local inclist {
		foreach sheet in fi fg {
			foreach suf in "" "_bins" {
				local `sheet'_mcs`suf' // set locals blank before loop below
			}
		}
		
		// this is outside of if condition because we need blank matrices for the rowsof() later
		local ncols = 2
		foreach ss of local supercols_all {
			foreach sheet in fi fg {
				matrix `ss'_`sheet' = J(5,`ncols',.) // a patch since later use rowsof() even if they specify suboptions
			}
		}
	}
			
	if "`income'"!="" & "`nincome'"!="" { 
		foreach ss of local supercols_all {
			foreach sheet in fi fg {
				if "``ss''"!="" {						
					local `sheet'_mcs ``sheet'_mcs' `ss'_`sheet'
				}
			}
		}
		// GROUPS 
		if "`nogroup'"=="" {
			local zz=0
			foreach p of local plopts {
				local ++zz
				if "``p''"!="" {
					if substr("`p'",1,2)=="pl" { // these are the PPP lines
						local _pline = ``p''
						local y0touse `income_ppp'
						local y1touse `nincome_ppp'
					}
					else if _`p'_isscalar==1 { // if pov line is scalar (not PPP line)
						local _pline = ``p'' // set `_pline' as that scalar and
						local y0touse `income'   // use original income variable
						local y1touse `nincome'
					}
					else if _`p'_isscalar==0 { // if pov line is variable,
						tempvar y0_normalized  // create temporary variable that is income...
						qui gen `y0_normalized' = `income'/``p'' // normalized by pov line
						local y0touse `y0_normalized' // use normalized income in the calculations
						
						tempvar y1_normalized  // create temporary variable that is income...
						qui gen `y1_normalized' = `nincome'/``p'' // normalized by pov line
						local y1touse `y1_normalized' // use normalized income in the calculations
						
						local _pline = 1                       // and normalized pov line is 1
					}
					_fifgp `y0touse' `y1touse' `aw', ///
					fimatrices(`fi_mcs') fgmatrices(`fg_mcs') ///
					z(`_pline') row(`zz') col(1) ///
					`supercols'
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
			local startrow = 7
			returncol `startcol_o'
			foreach sheet in fi fg {
				local resultset_`sheet' `resultset_`sheet'' `r(col)'`startrow'=matrix(poverty)
			}
		}
		
		** Fiscal impoverishment results
		local vertincrement = 8
		local horzincrement = 0
		local rgroup = 17 // row where first group starts (note no deciles on this sheet)
		local rbin = `rgroup' + `vertincrement'
				
		foreach sheet in fi fg {
			local startcol = `startcol_o'
			foreach ss of local supercols_all {
				returncol `startcol'
				if "`nogroup'"=="" & "``ss''"!="" local resultset_`sheet' `resultset_`sheet'' `r(col)'`rgroup'=matrix(`ss'_`sheet') 
				// still need to add columns to startcol even if "``ss''"=="":
				local startcol = `startcol' + colsof(`ss'_`sheet') + `horzincrement'
				
						// `ss'_fi_`y0' and `ss'_fi_`y0'_bins will have same # cols, but the if else is becauses the mx won't exist if the user
						//  specified the nogroup option
			}
		}

		// Export to Excel (group cutoffs and poverty lines)
		local lowcol = 1 
		local hicol = 2
		foreach x in low hi {
			returncol ``x'col'
			local _`x'col `r(col)'
		}
		forval i=1/3 {
			foreach rg in 7 17 { // two rows where poverty lines start
				local therow = `rg' + `i' - 1
				local cutoffs `cutoffs' `_lowcol'`therow'=(`pl`i'')
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
		
		// simulation description
		if `transferscale' > 0 {
			if "`tax2'"=="" {
				local simtitle I4=("The transfer `transfer' increases by `transferscale'. The `tax1' tax is used to fund the transfer.")
			}  
			else {
				if "`tax3'"=="" {
					local simtitle I4=("The transfer `transfer' increases by `transferscale'. The `tax1' tax funds `taxprop1' of the expanded transfer and the `tax2' tax funds `taxprop2' of the expanded transfer.")
				}
				if "`tax3'"!="" {
					local simtitle I4=("The transfer `transfer' increases by `transferscale'. The `tax1' tax funds `taxprop1' of the expanded transfer, the `tax2' tax funds `taxprop2' of the expanded transfer and the `tax3' tax funds `taxprop3' of the expanded transfer.")
				}
			}
		}
		
		if `transferscale' < 0 {
			if "`tax2'"=="" {
				local simtitle I4=("The transfer `transfer' decreases by `transferscale'. The saved funding provides cut in tax `tax1'.")
			}  
			else {
				if "`tax3'"=="" {
					local simtitle I4=("The transfer `transfer' decreases by `transferscale'. And `taxprop1' of the saved funding provides cut in tax `tax1'. `taxprop2' of the saved funding provides cut in tax `tax2'.")
				}
				if "`tax3'"!="" {
					local simtitle I4=("The transfer `transfer' decreases by `transferscale'. And `taxprop1' of the saved funding provides cut in tax `tax1'. `taxprop2' of the saved funding provides cut in tax `tax2'. ///
					                    `taxprop3' of the saved funding provides cut in tax `tax3'.")
				}
			}
		}
		
		// putexcel
		foreach sheet in fi fg {
			qui putexcel `titlesprint' `versionprint' `resultset_`sheet'' `warningprint' `simtitle' using `"`using'"', modify keepcellformat sheet("`sheet`sheet''")
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
