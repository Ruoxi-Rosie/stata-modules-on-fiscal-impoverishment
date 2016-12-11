{smcl}
{* 8dec2016}{...}
{cmd:help ceqfi_by} (beta version; please report bugs) {right:Rosie Li}
{hline}

{title:Title}

{p 4 11 2}
{hi:ceqfi} {hline 2} Calculates the measures of fiscal impoverishment (FI) and fiscal gains to the poor (FGP) from Higgins and Lustig (2016) for individual taxes and transfers 
            disaggregated by the categorical variables user supplies. Modified from {bf:ceqfi} (Higgins 2016).

{title:Syntax}

{p 8 11 2}
    {cmd:ceqfi_by} {ifin} {weight} [{cmd:using} {it:filename}] [{cmd:,} {it:options}]{break}

{synoptset 29 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Original Income and Fiscal Intervention}
{synopt :{opth inc:ome(varname)}}Original income{p_end}
{synopt :{opth taxes(varlist)}}Taxes to be applied to the original income{p_end}
{synopt :{opth transfers(varlist)}}Transfers to be applied to the original income{p_end}

{syntab:PPP conversion}
{synopt :{opth ppp(real)}}PPP conversion factor (LCU per international $, consumption-based) from year of PPP (e.g., 2005 or 2011) to year of PPP; do not use PPP factor for year of household survey{p_end}
{synopt :{opth cpib:ase(real)}}CPI of base year (i.e., year of PPP, usually 2005 or 2011){p_end}
{synopt :{opth cpis:urvey(real)}}CPI of year of household survey{p_end}
{synopt :{opt da:ily}}Indicates that variables are in daily currency{p_end}
{synopt :{opt mo:nthly}}Indicates that variables are in monthly currency{p_end}
{synopt :{opt year:ly}}Indicates that variables are in yearly currency (the default){p_end}

{syntab:Survey information}
{synopt :{opth hs:ize(varname)}}Number of members in the household
	(should be used when each observation in the data set is a household){p_end}
{synopt :{opth hh:id(varname)}}Unique household identifier variable
	(should be used when each observation in the data set is an individual){p_end}
{synopt :{opth psu(varname)}}Primary sampling unit; can also be set using {help svyset:svyset}{p_end}
{synopt :{opth s:trata(varname)}}Strata (used with complex sampling designs); can also be set using {help svyet:svyset}{p_end}
   
{syntab:Poverty lines}
{synopt :{opth pl1(real)}}Lowest poverty line in $ PPP (default is $1.25){p_end}
{synopt :{opth pl2(real)}}Second lowest poverty line in $ PPP (default is $2.50){p_end}
{synopt :{opth pl3(real)}}Third lowest poverty line in $ PPP (default is $4){p_end}
{synopt :{opth nationale:xtremepl(string)}}National extreme poverty line in same units as income variables (can be a scalar or {varname}){p_end}
{synopt :{opth nationalm:oderatepl(string)}}National moderate poverty line in same units as income variables (can be a scalar or {varname}){p_end}

{syntab:Produce subset of results}
{synopt :{opt nob:in}}Do not produce results by bin{p_end}

{syntab:Ignore missing values}
{synopt :{opt ignorem:issing}}Ignore any missing values of income concepts and fiscal interventions{p_end}

{syntab:Characteristics}
{synopt :{opth char:acteristics(varlist)}}Specify the categorical variables to produced disaggregated results of FI and FGP.{p_end}
   
{syntab:Export directly to CEQ Master Workbook (requires Stata 13 or newer)}
{synopt :{opth coun:try(string)}}Country{p_end}
{synopt :{opth surv:eyyear(string)}}Year of survey{p_end}
{synopt :{opth auth:ors(string)}}Authors of study{p_end}
{synopt :{opth base:year(real)}}Base year of PPP conversion (e.g., 2005, 2011){p_end}
{synopt :{opth sheetfi(string)}}Name of Excel sheet to write fiscal impoverishment results. Default is "FI. By"{p_end}
{synopt :{opth sheetfg(string)}}Name of Excel sheet to write fiscal gains to the poor results. Default is "FFI. By"{p_end}
{synopt :{opt open}}Automatically open CEQ Master Workbook with new results added{p_end}

{synoptline}		
{p 4 6 2}
{cmd:pweight} allowed; see {help weights}. Alternatively, weights can be specified using {help svyset}. 


{title:Description}

{pstd} 
{cmd:ceqfi_by} calculates the measures of fiscal impoverishment (FI) and fiscal gains to the poor (FGP) 
derived in Higgins and Lustig (2016): the FI and FGP headcounts (where the denominator is the total 
population); the FI and FGP headcounts among the poor (where the denominator is the total number of post-fisc poor for FI or pre-fisc poor for FGP); total FI and FGP (in dollars per day adjusted for purchasing power parity [PPP]); FI and 
FGP per capita (in PPP dollars per day), where k=1/|S| and S is the set of individuals in society with 
cardinality |S| (i.e. total FI or FGP is divided by the total population); normalized FI and FGP, 
where k = 1/(|S|z) and z is the poverty line (i.e., per capita FI or FGP as a proportion of the poverty 
line); FI per impoverished and FGP per gaining poor (in PPP dollars per day); and FI per impoverished person and FGP per gaining poor as a proportion of their pre-fisc incomes.

{pstd}
Poverty lines in PPP dollars per day can be set using the {opth pl1(real)}, {opth pl2(real)},
 and {opth pl3(real)} options; the defaults for these are the commonly-used $1.25, $2.50, and 
 $4 PPP poverty lines. For example, to change the lowest poverty line from $1.25 PPP per day 
 to $1.90 PPP per day, specify {cmd:pl1(1.90)}. Poverty lines in local currency can be entered
 using the {opth nationale:xtremepl(string)}, {opth nationalm:oderatepl(string)}, 
 {opth othere:xtremepl(string)}, {opth otherm:oderatepl(string)} options. Local currency 
 poverty lines can be entered as real numbers (for poverty lines that are fixed for the entire
 population) or variable names (for poverty lines that vary, for example across space), and 
 should be in the same units as the income concept variables (preferably local currency units 
 per year). 
Poverty lines are used for poverty results; for results by income group, the cut-offs of these groups 
can be changed using the {opth cut1(real)} to {opth cut5(real)} options; the default groups are 
ultra-poor ($0 to $1.25 per day in purchasing power parity [PPP] adjusted US dollars), extreme poor 
($1.25 to $2.50 PPP per day), moderate poor ($2.50 to $4 PPP per day), vulnerable ($4 to $10 PPP per 
day), middle class ($10 to $50 PPP per day) and wealthy ($50 and more PPP per day). For example, specify {cmd:cut1(1.90)} to 
change the first cut-off to $1.90 PPP per day (which would cause the lowest group to range from $0 to 
$1.90 PPP per day, and the second group--if {opth cut2(real)} is not specified so the default second 
cut-off is maintained--to range from $1.90 to $2.50 PPP).

{pstd}
{cmd: ceqfi_by} automatically converts local currency variables to PPP dollars, using the PPP conversion 
factor given by {opth ppp(real)}, the consumer price index (CPI) of the year of PPP (e.g., 2005 or 
2011) given by {opth cpib:ase(real)}, and the CPI of the year of the household 
survey used in the analysis given by {opth cpis:urvey(real)}. The year of PPP, also called base year, 
refers to the year of the International Comparison Program (ICP) that is being used, e.g. 2005 or 2011. 
The survey year refers to the year of the household survey used in the analysis. If the year of PPP is 
2005, the PPP conversion factor should be the "2005 PPP conversion factor, private consumption (LCU per 
international $)" indicator from the World Bank's World Development Indicators (WDI). If the year of 
PPP is 2011, use the "PPP conversion factor, private consumption (LCU per international $)" indicator 
from WDI. The PPP conversion factor should convert from year of PPP to year of PPP. In other words, 
when extracting the PPP conversion factor, it is possible to select any year; DO NOT select the year of 
the survey, but rather the year that the ICP was conducted to compute PPP conversion factors (e.g., 
2005 or 2011). The base year (i.e., year of PPP) CPI, which can also be obtained from WDI, should match 
the base year chosen for the PPP conversion factor. The survey year CPI should match the year of the 
household survey. Finally, for the PPP conversion, the user can specify whether the original variables 
are in local currency units per day ({opt da:ily}), per month ({opt mo:nthly}), or per year 
({opt year:ly}, the default assumption).

{pstd}
If the data set is at the individual level (each observation is an individual), the variable with the 
identification code of each household (i.e., it takes the same value for all members within a 
household) should be specified in the {opth hh:id(varname)} option; the {opth hs:ize(varname)} option 
should not be specified. If the data set is at the household level, the number of members in the 
household should be specified in {opth hs:ize(varname)}; the {opth hh:id(varname)} option should not be 
specified. In either case, the weight used should be the household sampling weight and should {it:not} 
be multiplied by the number of members in the household since the program will do this multiplication 
automatically in the case of household-level data. 

{pstd}
There are two options for including information about weights and survey sample design for accurate
estimates and statistical inference. The sampling weight can be entered using 
{weight} or {help svyset}. Information about complex stratified sample designs can also be entered 
using {help svyset} since {cmd:ceqfi_by} automatically uses the information specified using {help svyset}. 
Alternatively, the primary sampling unit can be entered using the {opth psu(varname)} option and strata 
can be entered using the {opth s:trata(varname)} option.

{pstd}
The program disaggregates the FI or FGP results by categorical variables specified by users using the 
{opth char:acteristics(varlist)} option. The results within a characteristic add up to the original FI 
or FGP result for headcount, headcount among the poor and total amount indicators. The variables need not be binary 
ones. For example, one can education-level variables stored as 1 primariy 2 secondary 3 tertiary., and the program
will produce results disaggregated by these three categories. 

{pstd}
By default, {cmd: ceqfi_by} does not allow income concept or fiscal intervention variables to have missing 
values: if a household has 0 income for an income concept, receives 0 from a transfer or a subsidy, 
or pays 0 of a tax, the household should have 0 rather than a missing value. If one of these variables has 
missing values, the command will produce an error. For flexibility, however, the command includes an 
{opt ignorem:issing} option that will drop observations with missing values for any of these variables, thus 
allowing the command to run even if there are missing values.

{pstd}
Negative incomes are allowed, but a warning is issued for each core income concept that 
has negative values (or positive values when a fiscal intervention is stored as negative values). This is because 
various measures are no longer well-behaved when negative values are included (for example, the Gini coefficient, 
concentration coefficient, or squared poverty gap can exceed 1, and other desirable properties of these measures 
when incomes are non-negative no longer hold when negative values are allowed).

{pstd}
Results are automatically exported to the CEQ Master Workbook if 
{cmd:using} {it:filename} is specifed in the command, where {it:filename} is the Master Workbook. By 
default, {cmd:ceqfi_by} prints fiscal impoverishment results to a sheet titled "E5. Fisc. Impov." 
and fiscal gains to the poor results to a sheet titled "Fisc. Gains to the Poor"; the user can 
override the sheet names using the {opth sheetfi(string)} and {opth sheetfg(string)} options, 
respectively. Exporting directly to the Master Workbook requires Stata 13 or newer. The Master Workbook 
populated with results from {cmd:ceqfi_by} can be automatically opened if the {opt open} option is 
specified (in this case, {it:filename} cannot have spaces). 

{title:Examples}

{pstd}Locals for PPP conversion (obtained from WDI through the {cmd: wbopendata} command){p_end}
{phang} {cmd:. local ppp = 1.5713184 // 2005 Brazilian reais per 2005 $ PPP}{p_end}
{phang} {cmd:. local cpi = 95.203354 // CPI for Brazil for 2009}{p_end}
{phang} {cmd:. local cpi05 = 79.560051 // CPI for Brazil for 2005}{p_end}

{pstd}Individual-level data (each observation is an individual){p_end}
{phang} {cmd:. ceqfi_by [pw=w] using C:/Output_Tables.xlsx, hhid(hh_code) psu(psu_var) strata(stra_var) income(ym) taxes(personal_inc_tax property_tax) transfers(scholarship) char(gender ed_level) ppp(`ppp') cpibase(`cpi05') cpisurvey(`cpi') country("Brazil") surveyyear("2008-2009") authors("Sean Higgins and Claudiney Pereira") baseyear(2005) open}{p_end}

{pstd}Household-level data (each observation is a household){p_end}
{phang} {cmd:. ceqfi_by [pw=w] using C:/Output_Tables.xlsx, hsize(members) psu(psu_var) strata(stra_var) income(ym) taxes(personal_inc_tax property_tax) transfers(scholarship) char(gender ed_level) ppp(`ppp') cpibase(`cpi05') cpisurvey(`cpi') country("Brazil") surveyyear("2008-2009") authors("Sean Higgins and Claudiney Pereira") baseyear(2005) open}{p_end}


{title:References}

{pstd}Commitment to Equity (CEQ) {browse "http://www.commitmentoequity.org":website}.{p_end}

{phang}
Higgins, Sean and Nora Lustig. 2016. {browse "http://www.sciencedirect.com/science/article/pii/S0304387816300220":"Can a Poverty-Reducing and Progressive Tax and Transfer System Hurt the Poor?"} Journal of Development Economics 122, 63-75.{p_end}

{phang}
Lustig, N. and S. Higgins. 2013. "Commitment to Equity Assessment (CEQ): Estimating the Incidence of Social Spending, Subsidies and Taxes Handbook." {browse "http://www.commitmentoequity.org/publications_files/Methodology/CEQWPNo1%20Handbook%20Edition%20Sept%202013.pdf":CEQ Working Paper 1.}{p_end}
{phang}
Higgins, Sean. 2016. "{bf:ceqfi}" Part of the {bf:ceq} Stata package.
