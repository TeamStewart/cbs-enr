********************************************************************************
********************************************************************************
***
*** Analysis of Georgia runoff, June 21, 2022
***
*** Modeling the following races:
*** GA:

*** Lt. gov, Dem
*** SoS, Dem
*** Insurance commissioner, Dem
*** Labor commissioner, Dem
*** CD 1, Dem
*** CD 2, Rep [McCormick has Trump endorsement]
*** CD 6, Rep [Evans has Trump endorsement]
*** CD 7, Rep [Neither seems to have Trump endorsement]
*** CD10, Rep [Collins has Trump endorsement]
********************************************************************************
********************************************************************************

global dir = "d"

global primary_data = "$dir:\Dropbox (MIT)\2022 ElectionZone\ElectionNight\GA\data\ga_allraces.csv"
global runoff_data = "$dir:\Dropbox (MIT)\2022 ElectionZone\ElectionNight\GA\data\ga_runoff_allraces.csv"

clear

********************************************************************************
**
** Map candidate names to last names
**
********************************************************************************
clear

input str30 candidates str30 lastname
"ERICK E. ALLEN" "ALLEN"
"CHARLIE BAILEY" "BAILEY"
"RAPHAEL BAKER" "BAKER"
"TIMOTHY BARR" "BARR"
"WILLIAM ''WILL'' BODDIE, JR." "BODDIE"
"TYRONE BROOKS, JR." "BROOKS"
"PAUL BROUN" "BROUN"
"TONY BROWN" "BROWN"
"VIVIAN L. CHILDS" "CHILDS"
"MIKE COLLINS" "COLLINS"
"MICHAEL CORBIN" "CORBIN"
"DAVID CURRY" "CURRY"
"DEE DAWKINS-HAIGLER" "DAWKINS"
"THOMAS DEAN" "DEAN"
"JOHN EAVES" "EAVES"
"JAKE EVANS" "EVANS"
"JESSICA ALLISON FORE" "FORE"
"BYRON GATEWOOD" "GATEWOOD"
"MARK GONSALVES" "GONSALVES"
"FLOYD GRIFFIN" "GRIFFIN"
"JOYCE MARIE GRIGGS" "GRIGGS"
"KWANZA HALL" "HALL"
"MEAGAN HANSON" "HANSON"
"BLAKE HARBIN" "HARBIN"
"PHYLLIS HATCHER" "HATCHER"
"JASON T. HAYES" "HAYES"
"WADE HERRING" "HERRING"
"NICOLE HORN" "HORN"
"JEREMY HUNT" "HUNT"
"DERRICK L. JACKSON" "JACKSON_D"
"LESTER G. JACKSON III" "JACKSON_J"
"TABITHA JOHNSON-GREEN" "JOHNSON_T"
"WAYNE JOHNSON" "JOHNSON_W"
"VERNON J. JONES" "JONES"
"R. MALIK" "MALIK"
"RICH MCCORMICK" "MCCORMICK"
"LISA MCCOY" "MCCOY"
"MARC MCMAIN" "MCMAIN"
"MICHELLE L. MUNROE" "MUNROE"
"BEE NGUYEN" "NGUYEN"
"YG NYGHTSTORM" "NYGHTSTORM"
"FEMI ODUWOLE" "ODUWOLE"
"MICHAEL OWENS" "OWENS"
"RICH ROBERTSON" "ROBERTSON"
"JANICE LAWS ROBINSON" "ROBINSON"
"RENITTA SHANNON" "SHANNON"
"ALAN SIMS" "SIMS"
"PAULETTE SMITH" "SMITH"
"MALLORY STAPLES" "STAPLES"
"NADIA SURRENCY" "SURRENCY"
"MITCHELL SWAN" "SWAN"
"SUZI VOYLES" "VOYLES"
"PAUL WALTON" "WALTON"
"CHRIS WEST" "WEST_C"
"MARY WEST" "WEST_M"
"PAUL WHITEHEAD" "WHITEHEAD"
"MATTHEW WILSON" "WILSON"
"EUGENE YU" "YU"
end

compress

save $dir:\scratch\candidates_lastname, replace
/*
********************************************************************************
**
** Read in primary data
**
********************************************************************************

import delimited using "$primary_data", clear bindquote(strict) stringcols(_all)
destring votes, replace force

replace race = upper(race)
replace candidates = trim(upper(candidates))
replace county = trim(upper(county))
replace precinct = trim(upper(precinct))
replace type = trim(upper(type))

replace race = "LT_GOV" if strpos(race,"LIEUTENANT") > 0 & party == "DEM"
replace race = "LT_GOV" if strpos(race,"LIUTENANT") > 0 & party == "DEM"
replace race = "SOS" if strpos(race,"SECRETARY OF STATE") > 0 & party == "DEM"
replace race = "INSUR" if strpos(race,"INSURANCE") > 0 & party == "DEM"
replace race = "LABOR" if strpos(race,"LABOR") > 0 & party == "DEM"
replace race = "CD1" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 1 ") > 0 & party == "DEM"
replace race = "CD2" if (strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 2 ") > 0 & party == "REP") | (strpos(race," US HOUSE") > 0 & strpos(race, "DIST 2") > 0 & party == "REP")
replace race = "CD6" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & (strpos(race,"DISTRICT 6 ") > 0 | strpos(race,"DISTRICT 6/")) > 0 & party == "REP"
replace race = "CD6" if strpos(race,"US HOUSE") > 0 & strpos(race,"DIST 6") > 0 & party == "REP"
replace race = "CD7" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & (strpos(race,"DISTRICT 7 ") > 0 | strpos(race,"DISTRICT 7/")) > 0 & party == "REP"
replace race = "CD7" if strpos(race,"US HOUSE") > 0 & strpos(race,"DIST 7") > 0 & party == "REP"
replace race = "CD10D" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 10") > 0 & strpos(race," - DEM") > 0 & party == "DEM"
replace race = "CD10R" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 10") > 0 & strpos(race," - REP") > 0 & party == "REP"

keep if inlist(race,"LT_GOV","SOS","INSUR","LABOR","CD1","CD2","CD6","CD7","CD10D") | inlist(race,"CD10R") == 1

drop if precinct == "-1"

replace candidates = "TABITHA JOHNSON-GREEN" if candidates == "TABITHA JOHNSON- GREEN"

replace precinct = "WINOKUR" if county == "CHARLTON" & precinct == "WINKOUR"

replace precinct = "1-05C" if county == "CHATHAM" & strpos(precinct,"1-05C ") > 0
replace precinct = "1-16C" if county == "CHATHAM" & strpos(precinct,"1-16C ") > 0
replace precinct = "7-11C" if county == "CHATHAM" & strpos(precinct,"7-11C ") > 0

replace precinct = "11E2" if county == "FULTON" & precinct == "1.10E+02"
replace precinct = "11E3" if county == "FULTON" & precinct == "1.10E+03"
replace precinct = "11E4" if county == "FULTON" & precinct == "1.10E+04"
replace precinct = "12E2" if county == "FULTON" & precinct == "1.20E+02"

keep race candidates county precinct type votes

compress

rename votes votes_primary

save $dir:\scratch\primary_votes, replace


*/
********************************************************************************
**
** Read in runoff data
**
********************************************************************************

import delimited using "$runoff_data", clear bindquote(strict) stringcols(_all)
destring votes, replace force

replace race = upper(race)
replace candidates = trim(upper(candidates))
replace county = trim(upper(county))
replace precinct = trim(upper(precinct))
replace type = trim(upper(type))

replace race = "LT_GOV" if strpos(race,"LIEUTENANT") > 0
replace race = "SOS" if strpos(race,"SECRETARY OF STATE") > 0
replace race = "INSUR" if strpos(race,"INSURANCE") > 0
replace race = "LABOR" if strpos(race,"LABOR") > 0
replace race = "CD1" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 1 ") > 0
replace race = "CD2" if (strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 2 ") > 0) | (strpos(race," US HOUSE") > 0 & strpos(race, "DIST 2") > 0)
replace race = "CD6" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & (strpos(race,"DISTRICT 6 ") > 0 | strpos(race,"DISTRICT 6/")) > 0
replace race = "CD6" if strpos(race,"US HOUSE") > 0 & strpos(race,"DIST 6") > 0
replace race = "CD7" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & (strpos(race,"DISTRICT 7 ") > 0 | strpos(race,"DISTRICT 7/")) > 0
replace race = "CD7" if strpos(race,"US HOUSE") > 0 & strpos(race,"DIST 7") > 0 & party == "REP"
replace race = "CD10D" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 10") > 0 & strpos(race," - DEM") > 0
replace race = "CD10R" if strpos(race,"US HOUSE OF REPRESENTATIVES") > 0 & strpos(race,"DISTRICT 10") > 0 & strpos(race," - REP") > 0

keep if inlist(race,"LT_GOV","SOS","INSUR","LABOR","CD1","CD2","CD6","CD7","CD10D") | inlist(race,"CD10R") == 1

drop if precinct == "-1"

replace candidates = "TABITHA JOHNSON-GREEN" if candidates == "TABITHA JOHNSON- GREEN"

replace precinct = "WINOKUR" if county == "CHARLTON" & precinct == "WINKOUR"

replace precinct = "1-05C" if county == "CHATHAM" & strpos(precinct,"1-05C ") > 0
replace precinct = "1-16C" if county == "CHATHAM" & strpos(precinct,"1-16C ") > 0
replace precinct = "7-11C" if county == "CHATHAM" & strpos(precinct,"7-11C ") > 0

replace candidates = "VERNON J. JONES" if candidates == "VERNON JONES" & race == "CD10R"

keep race candidates county precinct type votes

compress

rename votes votes_runoff

save $dir:\scratch\runoff_votes, replace


********************************************************************************
**
** Merge the primary dataset with the last names and then produce vote shares
** for each candidate in each precinct
**
** Note: There are some precincts from Fulton, Liberty that can't be merged
**
********************************************************************************

clear
use $dir:\scratch\primary_votes, clear
merge m:1 candidates using $dir:\scratch\candidates_lastname
sort county precinct race type candidate
egen tvotes = sum(votes),by(county precinct race type)

gen pct = votes/tvotes
keep race county precinct type tvotes pct lastname

rename tvotes ptvotes
rename pct ppct

reshape wide ppct,i(race county precinct type ptvotes) j(lastname) string

save $dir:\scratch\primary_pct, replace


********************************************************************************
**
** Merge the runoff dataset with the last names and then produce vote shares
** for each candidate in each precinct
**
** Note: There are some precincts from Fulton, Liberty that can't be merged
**
********************************************************************************

clear
use $dir:\scratch\runoff_votes, clear
merge m:1 candidates using $dir:\scratch\candidates_lastname
drop if _merge < 3
drop _merge

sort county precinct race type candidate
egen tvotes = sum(votes),by(county precinct race type)

gen pct = votes/tvotes
keep race county precinct type tvotes pct lastname

rename tvotes rtvotes
rename pct rpct

reshape wide rpct,i(race county precinct type rtvotes) j(lastname) string

save $dir:\scratch\runoff_pct, replace


********************************************************************************
**
** Merge the two previous datasets
**
********************************************************************************

clear

use $dir:\scratch\primary_pct
merge 1:1 race county precinct type using $dir:\scratch\runoff_pct


********************************************************************************
**
** Regress early voting
**
** Start with CD2, CD6, and CD10R + Lt Gov
**
********************************************************************************


*This is to allow for testing
/*
foreach v of varlist rpct* {
    replace `v' = runiform()
}
replace ptvotes = runiform(0,1000)
replace rtvotes = runiform(0,1000)
*/

* Vote share regressions
reg rpctHUNT ppctCHILDS ppctHUNT ppctJOHNSON_W ppctROBERTSON ppctWEST_C /*ppctWHITEHEAD*/ [aw=rtvotes] if type == "ADVANCED VOTING" // CD2
predict py if race == "CD2" & type == "ADVANCED VOTING"
local candidate = "HUNT"
gen est`candidate' = py if race == "CD2" & type == "ADVANCED VOTING"
replace est`candidate' = rpct`candidate' if rpct`candidate' ~= . &  race == "CD2" & type == "ADVANCED VOTING"
drop py

reg rpctMCCORMICK ppctEVANS ppctGATEWOOD ppctHANSON ppctHARBIN ppctMCCORMICK ppctSMITH ppctSTAPLES /*ppctVOYLES*/ ppctYU [aw=rtvotes] if type == "ADVANCED VOTING" // CD6
predict py if race == "CD6" & type == "ADVANCED VOTING"
local candidate = "MCCORMICK"
gen est`candidate' = py if race == "CD6" & type == "ADVANCED VOTING"
replace est`candidate' = rpct`candidate' if rpct`candidate' ~= . &  race == "CD6" & type == "ADVANCED VOTING"
drop py

reg rpctCOLLINS ppctBARR ppctBROUN ppctCOLLINS ppctCURRY ppctJONES ppctMCMAIN ppctSIMS ppctSWAN [aw=rtvotes] if type == "ADVANCED VOTING" // CD10R
predict py if race == "CD10R" & type == "ADVANCED VOTING"
local candidate = "COLLINS"
gen est`candidate' = py if race == "CD10R" & type == "ADVANCED VOTING"
replace est`candidate' = rpct`candidate' if rpct`candidate' ~= . & race == "CD10R" & type == "ADVANCED VOTING"
drop py

reg rpctHALL ppctALLEN ppctBAILEY ppctBROOKS ppctBROWN ppctHALL ppctHAYES ppctJACKSON_D /*ppctMALIK*/ ppctSHANNON [aw=rtvotes] if type == "ADVANCED VOTING" // Lt. gov
predict py if race == "LT_GOV" & type == "ADVANCED VOTING"
local candidate = "HALL"
gen est`candidate' = py if race == "LT_GOV" & type == "ADVANCED VOTING"
replace est`candidate' = rpct`candidate' if rpct`candidate' ~= . &  race == "LT_GOV" & type == "ADVANCED VOTING"
drop py

*turnout regressions
local race = "CD2"
reg rtvotes ptvotes if type == "ADVANCED VOTING" & race == "`race'"
predict py if race == "`race'" & type == "ADVANCED VOTING"
gen est_turnout_`race' = py if race == "`race'" & type == "ADVANCED VOTING"
replace est_turnout_`race' = rtvotes if rtvotes ~= . &  race == "`race'" & type == "ADVANCED VOTING"
drop py

local race = "CD6"
reg rtvotes ptvotes if type == "ADVANCED VOTING" & race == "`race'"
predict py if race == "`race'" & type == "ADVANCED VOTING"
gen est_turnout_`race' = py if race == "`race'" & type == "ADVANCED VOTING"
replace est_turnout_`race' = rtvotes if rtvotes ~= . &  race == "`race'" & type == "ADVANCED VOTING"
drop py

local race = "CD10R"
reg rtvotes ptvotes if type == "ADVANCED VOTING" & race == "`race'"
predict py if race == "`race'" & type == "ADVANCED VOTING"
gen est_turnout_`race' = py if race == "`race'" & type == "ADVANCED VOTING"
replace est_turnout_`race' = rtvotes if rtvotes ~= . &  race == "`race'" & type == "ADVANCED VOTING"
drop py

local race = "LT_GOV"
reg rtvotes ptvotes if type == "ADVANCED VOTING" & race == "`race'"
predict py if race == "`race'" & type == "ADVANCED VOTING"
gen est_turnout_`race' = py if race == "`race'" & type == "ADVANCED VOTING"
replace est_turnout_`race' = rtvotes if rtvotes ~= . &  race == "`race'" & type == "ADVANCED VOTING"
drop py

*multiply things out
gen est_votesHUNT = estHUNT*est_turnout_CD2
gen est_votesMCCORMICK = estMCCORMICK*est_turnout_CD6
gen est_votesCOLLINS = estCOLLINS*est_turnout_CD10R
gen est_votesHALL = estHALL*est_turnout_LT_GOV
