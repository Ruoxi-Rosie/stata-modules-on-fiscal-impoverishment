# Stata modules on fiscal impoverishment
ceqfi_taxtransfer and ceqfi_by are two exercise commands I wrote to provide additional information on fiscal impoverishment and fiscal gains to the poor (Higgins and Lustig 2016), modified from Stata ceqfi command in the ceq Stata package (Higgins 2016).


Compatibiliy: Stata 13.0 or newer is required to write results to the excel file.


ceqfi_taxtransfer calculates the fiscal impoverishment and fiscal gain to the poor indicators (headcount, headcount among the poor, total amount, per capita amount and per capita amount normalized) by individual taxes and transfers specified by the user. In addition, it automatically graphs the level of FI/FGP indicator for a specific fiscal intervention given the value of the poverty line used. The user can choose whether to produce graphs at all, the interval of poverty as the x-axis, the interventions to be graphed. The main options needed in order to produce results using ceqfi_taxtransfer are income(varname), taxes(varlist) and transfers(varlist). This gives the users flexibility to simulate the fiscal impoverishment or fiscal gains to the poor that will occur after the institution of any tax or transfer. 


ceqfi_by disaggregates the FI or FGP results by categorical variables specified by users using the characteristics(varlist) option. The results within a characteristic add up to the original total FI or FGP result for headcount, headcount among the poor and total amount indicators. The variables need not be binary ones. For example, one can supply an education-level variable stored as 1. primary 2. secondary 3. tertiary., and the program will produce results disaggregated by these three categories. These results provide a glance at the distributional impact in terms of fiscal impoverishment and fiscal gains to the poor. 


Citation: 

Higgins, Sean and Nora Lustig. 2016. "Can a Poverty-Reducing and Progressive Tax and Transfer System Hurt the Poor?" Journal of Development Economics 122, 63-75.

Higgins, Sean and Rodrigo Aranda. 2016. CEQ Stata Package.
