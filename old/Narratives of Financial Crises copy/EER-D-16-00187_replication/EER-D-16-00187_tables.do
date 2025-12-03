


/**********************************************************************************/
* Going to Extremes: Politics after Financial Crises, 1870-2014 
* Manuel Funke, Christoph Trebesch, Moritz Schularick 

* Replicates version that has been accepted at EER, March 2016 


* Replication code for Table 1 to 6 in the main text 

		
use C:\...\EER-D-16-00187_data.dta, clear

sort ccode year
tsset ccode year, yearly
estimates clear


* Table 1: Far-right and far-left vote shares: post-crisis years vs. normal years

qui{
	* Generate global war dummy
		gen warx=0
		replace warx=1 if year>=1914 & year<=1918
		replace warx=1 if year>=1939 & year<=1949
	
	* Generate post-crisis windows for ols
		gen crisis_ols = crisisJST
		replace crisis_ols = 0 if ((crisis_ols[_n-1] == 1 & ccode[_n-1] == ccode) | (crisis_ols[_n-2] == 1 & ccode[_n-2] == ccode) | (crisis_ols[_n-3] == 1 & ccode[_n-3] == ccode) | ( crisis_ols[_n-4] == 1 & ccode[_n-4] == ccode) | (crisis_ols[_n-5] == 1 & ccode[_n-5] == ccode)) 
		gen postcrisis = 0
		replace postcrisis = 1 if ((crisis_ols[_n-5]==1 & ccode==ccode[_n-5]) | (crisis_ols[_n-4]==1 & ccode==ccode[_n-4]) | (crisis_ols[_n-3]==1 & ccode==ccode[_n-3]) | (crisis_ols[_n-2]==1 & ccode==ccode[_n-2]) | (crisis_ols[_n-1]==1 & ccode==ccode[_n-1])) 

	* Generate econ controls
		gen lrgdp = log(rgdp)
		gen dlrgdp =100*d.lrgdp
		gen lcpi = log(cpi)
		gen dlcpi = 100*d.lcpi
							
	* Make table (inclusion of econ controls produces similar results)
		local extrols right left
		label variable right "Far-right vote share"
		label variable left "Far-left vote share"
		foreach e of local extrols{
		local mylabel_`e': variable label `e'
		capture eststo `e'_full: qui xtreg `e' postcrisis  /*dlrgdp dlcpi*/  if warx!=1, fe vce(robust)
		capture eststo `e'_prewar: qui xtreg `e' postcrisis  /*dlrgdp dlcpi*/  if warx!=1 & year>=1919 & year<=1938, fe vce(robust)
		capture eststo `e'_postwar: qui xtreg `e' postcrisis  /*dlrgdp dlcpi*/  if warx!=1 & year>=1950 & year<=2014, fe vce(robust) 
		noisily: esttab `e'_full `e'_prewar `e'_postwar, keep(postcrisis) r2 star(* 0.10 ** 0.05 *** 0.01) title("`mylabel_`e''") mtitles("Full sample" "Pre-WW2" "Post-WW2") nonumbers nonotes b(3) se(3) sfmt(3) obslast 
		}
}


* Table 2: Parliamentary variables: post-crisis years vs. normal years

qui{
	* Setup table
		local parl govvote oppvote frac partycount
		label variable govvote `"Government vote share"' 
		label variable oppvote `"Opposition vote share"'
		label variable frac `"Fractionalization of parliament"'
		label variable partycount `"No. of parties in parliament"'
		foreach p of local parl{
		local mylabel_`p': variable label `p'
		}
		
	* Make table (inclusion of econ controls produces similar results)
		foreach p of local parl{
		capture eststo `p'_full: qui xtreg `p' postcrisis  /*dlrgdp dlcpi*/  if warx!=1, fe vce(robust)
		capture eststo `p'_prewar: qui xtreg `p' postcrisis  /*dlrgdp dlcpi*/  if warx!=1 & year>=1870 & year<=1938, fe vce(robust)
		capture eststo `p'_postwar: qui xtreg `p' postcrisis  /*dlrgdp dlcpi*/  if warx!=1 & year>=1950 & year<=2014, fe vce(robust)
		noisily esttab `p'_full `p'_prewar `p'_postwar, keep(postcrisis) r2 coeflabels(postcrisis) star(* 0.10 ** 0.05 *** 0.01) title("`mylabel_`p''") mtitles("Full sample" "Pre-WW2" "Post-WW2") nonumbers nonotes b(3) se(3) sfmt(3) obslast
		}
}


* Table 3: Political instability in the post-crisis period

qui{
	*ATTENTION: Logit takes long
		
    * OLS regressions	
		foreach p of local parl{
		xtreg govcris `p' i.year if postcrisis==1, fe vce(robust)
		estimates store govcris_`p'
	
	* Logit regressions 
		xtlogit turnover `p' i.year if postcrisis==1, fe  		
		estimates store turnover_`p'
		}
	
	* Make table
		noisily: esttab govcris_govvote govcris_oppvote govcris_frac govcris_partycount, keep(govvote oppvote frac partycount) cells(b(star fmt(4) label(Gov.Cris.)) se(par fmt(4))) starlevels( * 0.10 ** 0.05 *** 0.01) stats(r2 N, labels("R-Sq" "Obs") fmt(3 0)) nomtitles nonumbers nonotes
		noisily: esttab turnover_govvote turnover_oppvote turnover_frac turnover_partycount, keep(govvote oppvote frac partycount) cells(b(star fmt(4) label(Turnover)) se(par fmt(4))) starlevels( * 0.10 ** 0.05 *** 0.01) stats(r2 N, labels("R-Sq" "Obs") fmt(3 0)) nomtitles nonumbers nonotes
}


* Table 4: Street protest variables: post-crisis years vs. normal years

qui{
	* Local street 
		local street strikesdev riotsdev demosdev protestsdev
		label variable strikesdev "General strikes" 
		label variable riotsdev "Violent riots" 
		label variable demosdev "Anti-government demonstrations"
		label variable protestsdev "Street protests"
		
	*Generate samples for street
		gen full = 0
		replace full = 1 if (year >=1870 & year <= 1913) | (year >= 1919 & year <= 1939) | (year>=1950 & year<=2014) 
		gen pre = 0
		replace pre = 1 if (year >=1870 & year <= 1913) | (year >= 1919 & year <= 1939)
		gen post = 0
		replace post = 1 if (year>=1950 & year<=2014) 
		local samples full pre post 

	* Make table 
		foreach s of local street{
		local mylabel_`s': variable label `s'
		capture eststo `s'_full: xi: xtreg `s' postcrisis dlrgdp dlcpi if full == 1, fe vce(robust)
		capture eststo `s'_pre:   xi: xtreg `s' postcrisis dlrgdp dlcpi if pre== 1, fe vce(robust)
		capture eststo `s'_post:  xi: xtreg `s' postcrisis dlrgdp dlcpi if post==1 & year<=2012, fe vce(robust)
		noisily: esttab `s'_full `s'_pre `s'_post, keep(postcrisis) r2 nonotes star(* 0.10 ** 0.05 *** 0.01) title("`mylabel_`s''") mtitles("Full sample" "Pre-WW2" "Post-WW2") nonumbers  b(3) se(3) sfmt(3) obslast
		}
}
		

* Table 5: Local projections of far-right vote shares

qui{
	* Transform variables for LPs
		gen logright = log(1+right)
		gen loggovvote = log(1+govvote)
		gen logoppvote = log(1+oppvote)
		gen logfrac = log(1+frac)
		gen lpartycount = log(1+partycount)
		local vlags logright loggovvote logoppvote logfrac lpartycount dlrgdp dlcpi protestsdev  
		foreach v of local vlags {
		gen l`v' = l.`v'
		}	
		
	* Generate centered country dummies
		forvalues n=1/20 {
		gen cendum`n' = .
		replace cendum`n' = 1 - 1/20 if ccode == `n'
		replace cendum`n' = 0 - 1/20 if ccode != `n'
		}
	
	* Generate year columns
		gen Year = 0 in 1/11
		replace Year = 1 in 2/11
		replace Year = 2 in 3/11
		replace Year = 3 in 4/11
		replace Year = 4 in 5/11
		replace Year = 5 in 6/11
		replace Year = 6 in 7/11
		replace Year = 7 in 8/11
		replace Year = 8 in 9/11
		replace Year = 9 in 10/11
		replace Year = 10 in 11/11
		gen zero = 0 in 1/11
	
	* Generate dependent variables	
		local fragment logright loggovvote logoppvote logfrac lpartycount protestsdev
		forvalues i=1/10 {
		foreach f of local fragment {
		gen `f'`i' = 100*(f`i'.`f' - `f')
		}
		}
	
	* Generate columns for coefficients	
		foreach s of local samples{
		foreach f of local fragment{
		gen b_f_`f'_`s'=0 in 1/11
		gen se_f_`f'_`s'=0 in 1/11
		}
		}                             
	
	* Treatment labels 
		label variable pk_fin "Financial recession"
		label variable pk_norm "Normal recession"
		label variable pk_dis "Non-fin disaster" 
		
	* Setup table
		local tab5 logright
		label variable logright "FAR-RIGHT VOTE SHARES"
		label var logright1 "Year 1"
		label var logright2 "Year 2"
		label var logright3 "Year 3"
		label var logright4 "Year 4"
		label var logright5 "Year 5"
	
	*LP regressions conditional on pk_fin and pk_norm
		foreach s of local samples{
		foreach f of local fragment{
		reg `f'1 pk_fin pk_norm dlrgdp dlcpi l.dlrgdp l.dlcpi `f' l.`f'  cendum1-cendum19 if `s' == 1, noconstant cluster(iso)
		gen `f'sample_`s' = e(sample) 
		local rhslevels dlrgdp dlcpi `f' 
		foreach v of local rhslevels{
		sum `v' if `f'sample_`s'==1  
		gen `f'_`s'_`v' = `v' - r(mean)    
		sum l`v' if `f'sample_`s'==1
		gen l`f'_`s'_`v' = l`v' - r(mean)  
		}
		forvalues i =1/5 {
		reg `f'`i' pk_fin pk_norm `f'_`s'_dlrgdp `f'_`s'_dlcpi `f'_`s'_`f' l`f'_`s'_dlrgdp l`f'_`s'_dlcpi l`f'_`s'_`f' cendum1-cendum19 if `f'sample_`s'==1, noconstant cluster(iso)
		test pk_fin = pk_norm
		estadd scalar NormFin_pdiff = r(p)  
		eststo LP`f'_`s'_`i'_a
		}
		}
		}

	*LP regressions conditional on pk_fin and pk_dis
		foreach s of local samples{
		foreach f of local fragment{
		reg `f'1 pk_fin pk_dis dlrgdp dlcpi l.dlrgdp l.dlcpi `f' l.`f'  cendum1-cendum19 if `s' == 1, noconstant cluster(iso)
		replace `f'sample_`s' = e(sample)
		local rhslevels dlrgdp dlcpi `f' 
		foreach v of local rhslevels{
		sum `v' if `f'sample_`s'==1  
		replace `f'_`s'_`v' = `v' - r(mean)
		sum l`v' if `f'sample_`s'==1
		replace l`f'_`s'_`v' = l`v' - r(mean)
		}
		forvalues i =1/5 {
		reg `f'`i' pk_fin pk_dis `f'_`s'_dlrgdp `f'_`s'_dlcpi `f'_`s'_`f' l`f'_`s'_dlrgdp l`f'_`s'_dlcpi l`f'_`s'_`f' cendum1-cendum19 if `f'sample_`s'==1, noconstant cluster(iso)
		test pk_fin = pk_dis
		estadd scalar DisFin_pdiff = r(p)  
		eststo LP`f'_`s'_`i'_b
		}
		}
		}

	* Setup table
		foreach f of local tab5{
		local mylabel_`f': variable label `f'
	
	* Make table	
		*Full sample
			noisily: esttab LP`f'_full_1_a LP`f'_full_2_a LP`f'_full_3_a LP`f'_full_4_a LP`f'_full_5_a, se nonumbers keep(pk_fin pk_norm) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes title("`mylabel_`f'': FULL SAMPLE") noobs nolines compress
			noisily: esttab LP`f'_full_1_b LP`f'_full_2_b LP`f'_full_3_b LP`f'_full_4_b LP`f'_full_5_b, se nonumbers keep(pk_dis) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes nomtitles nolines noobs nogaps compress
			noisily: esttab LP`f'_full_1_a LP`f'_full_2_a LP`f'_full_3_a LP`f'_full_4_a LP`f'_full_5_a, cells(none) stat(NormFin_pdiff, labels("Fin=norm") fmt(2)) nonumbers nomtitles nonotes noobs nolines nogaps 
			noisily: esttab LP`f'_full_1_b LP`f'_full_2_b LP`f'_full_3_b LP`f'_full_4_b LP`f'_full_5_b, cells(none) stat(DisFin_pdiff, labels("Fin=dis") fmt(2)) nonumbers nomtitles nonotes noobs nolines nogaps 
			noisily: esttab LP`f'_full_1_a LP`f'_full_2_a LP`f'_full_3_a LP`f'_full_4_a LP`f'_full_5_a, cells(none) stat(r2 N, labels("R-sq." "Obs.") fmt(3 0)) nonumbers nomtitles nonotes noobs nolines nogaps 				
		*Pre-WW2 sample
			noisily: esttab LP`f'_pre_1_a LP`f'_pre_2_a LP`f'_pre_3_a LP`f'_pre_4_a LP`f'_pre_5_a, se nonumbers keep(pk_fin pk_norm) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes title("`mylabel_`f'': PRE-WW2 SAMPLE") noobs nolines compress
			noisily: esttab LP`f'_pre_1_b LP`f'_pre_2_b LP`f'_pre_3_b LP`f'_pre_4_b LP`f'_pre_5_b, se nonumbers keep(pk_dis) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes nomtitles nolines noobs nogaps compress
			noisily: esttab LP`f'_pre_1_a LP`f'_pre_2_a LP`f'_pre_3_a LP`f'_pre_4_a LP`f'_pre_5_a, cells(none) stat(NormFin_pdiff, labels("Fin=norm") fmt(2)) nonumbers nomtitles nonotes noobs nolines nogaps 
			noisily: esttab LP`f'_pre_1_b LP`f'_pre_2_b LP`f'_pre_3_b LP`f'_pre_4_b LP`f'_pre_5_b, cells(none) stat(DisFin_pdiff, labels("Fin=dis") fmt(2)) nonumbers nomtitles nonotes  noobs nolines nogaps 
			noisily: esttab LP`f'_pre_1_a LP`f'_pre_2_a LP`f'_pre_3_a LP`f'_pre_4_a LP`f'_pre_5_a, cells(none) stat(r2 N, labels("R-sq." "Obs.") fmt(3 0)) nonumbers nomtitles nonotes noobs nolines nogaps 		
		*Post-WW2 sample 
			noisily: esttab LP`f'_post_1_a LP`f'_post_2_a LP`f'_post_3_a LP`f'_post_4_a LP`f'_post_5_a, se nonumbers keep(pk_fin pk_norm) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes title("`mylabel_`f'': POST-WW2 SAMPLE") noobs nolines compress
			noisily: esttab LP`f'_post_1_b LP`f'_post_2_b LP`f'_post_3_b LP`f'_post_4_b LP`f'_post_5_b, se nonumbers keep(pk_dis) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes nomtitles nolines noobs nogaps compress
			noisily: esttab LP`f'_post_1_a LP`f'_post_2_a LP`f'_post_3_a LP`f'_post_4_a LP`f'_post_5_a, cells(none) stat(NormFin_pdiff, labels("Fin=norm") fmt(2)) nonumbers nomtitles nonotes noobs nolines nogaps 
			noisily: esttab LP`f'_post_1_b LP`f'_post_2_b LP`f'_post_3_b LP`f'_post_4_b LP`f'_post_5_b, cells(none) stat(DisFin_pdiff, labels("Fin=dis") fmt(2)) nonumbers nomtitles nonotes noobs nolines nogaps 
			noisily: esttab LP`f'_post_1_a LP`f'_post_2_a LP`f'_post_3_a LP`f'_post_4_a LP`f'_post_5_a, cells(none) stat(r2 N, labels("R-sq." "Obs.") fmt(3 0)) nonumbers nomtitles nonotes  noobs nolines  nogaps 
		}
}

* Table 6: Local projections of political variables, post-World War II sample

qui{
	* Setup table
		label variable loggovvote "GOVERNMENT VOTE SHARE"
		label variable logfrac "FRACTIONALIZATION OF PARLIAMENT"
		label variable lpartycount "NUMBER OF PARTIES IN PARLIAMENT"
		label variable protestsdev "STREET PROTESTS (% DEVIATION FROM TREND)"
		local tab6 loggovvote logfrac lpartycount protestsdev
		foreach f of local tab6{
		label var `f'1 "Year 1"
		label var `f'2 "Year 2"
		label var `f'3 "Year 3"
		label var `f'4 "Year 4"
		label var `f'5 "Year 5"
		local mylabel_`f': variable label `f'
		
	*Make table
		noisily: esttab LP`f'_post_1_a LP`f'_post_2_a LP`f'_post_3_a LP`f'_post_4_a LP`f'_post_5_a, se nonumbers keep(pk_fin pk_norm) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes title("`mylabel_`f''") noobs nolines compress
		noisily: esttab LP`f'_post_1_b LP`f'_post_2_b LP`f'_post_3_b LP`f'_post_4_b LP`f'_post_5_b, se nonumbers keep(pk_dis) b(2) se(2) sfmt(2) label star(* 0.10 ** 0.05 *** 0.01) nonotes nomtitles nolines noobs nogaps compress
		noisily: esttab LP`f'_post_1_a LP`f'_post_2_a LP`f'_post_3_a LP`f'_post_4_a LP`f'_post_5_a, cells(none) stat(NormFin_pdiff, labels("Fin=norm") fmt(2)) nonumbers nomtitles nonotes noobs nolines nogaps 
		noisily: esttab LP`f'_post_1_b LP`f'_post_2_b LP`f'_post_3_b LP`f'_post_4_b LP`f'_post_5_b, cells(none) stat(DisFin_pdiff, labels("Fin=dis") fmt(2)) nonumbers nomtitles nonotes noobs nolines nogaps 
		}
}

clear
/**********************************************************************************/
