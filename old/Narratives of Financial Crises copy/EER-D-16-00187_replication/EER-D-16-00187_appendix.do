


/**********************************************************************************/
* Going to Extremes: Politics after Financial Crises, 1870-2014 
* Manuel Funke, Christoph Trebesch, Moritz Schularick 

* Replicates version that has been accepted at EER, March 2016 

* This program contains: 

* A) Replication codes for Appendix Figures C1 and F1 
* B) Replication codes for Appendix Tables A2 and E1-E5 



use C:\...\EER-D-16-00187_data.dta, clear

sort ccode year
tsset ccode year, yearly
estimates clear



* A) Appendix Figures



* Figure C1: Street protests: variation over time 


	* Setup figure
		gen decade = "1910s" if year >=1910 & year <=1919
		replace decade = "1920s" if year >=1920 & year <=1929
		replace decade = "1930s" if year >=1930 & year <=1939
		replace decade = "1940s" if year >=1940 & year <=1949
		replace decade = "1950s" if year >=1950 & year <=1959
		replace decade = "1960s" if year >=1960 & year <=1969
		replace decade = "1970s" if year >=1970 & year <=1979
		replace decade = "1980s" if year >=1980 & year <=1989
		replace decade = "1990s" if year >=1990 & year <=1999
		replace decade = "2000s" if year >=2000 & year <=2009
		replace decade = "2010s" if year >=2010 & year <=2014
		
	* Make figure
		graph bar (mean) protests if year>= 1920, over(decade,  label(labsize(small))) xsize(8) ysize(4)  ///
		ytitle("Average no. of incidents per year", margin(medsmall) size(medium)) graphregion(color(white)) name(FigureC1, replace)


* Figure F1: Veto Player Index (local projections): financial crisis recessions

	* Transform variables for LPs
		qui	gen logvetopl = log(1+vetopl)
		qui	gen lrgdp = log(rgdp)
		qui	gen dlrgdp =100*d.lrgdp
		qui	gen lcpi = log(cpi)
		qui	gen dlcpi = 100*d.lcpi
		qui	local vlags logvetopl dlrgdp dlcpi  
		foreach v of local vlags {
		gen l`v' = l.`v'
		}	
	
	* Generate sample for LPs	
		qui	gen post = 0
		qui	replace post = 1 if (year>=1950 & year<=2014) 
		local sample post
		
	* Generate centered country dummies
		forvalues n=1/20 {
		qui	gen cendum`n' = .
		qui	replace cendum`n' = 1 - 1/20 if ccode == `n'
		qui	replace cendum`n' = 0 - 1/20 if ccode != `n'
		}
	
	* Generate year columns
		qui	gen Year = 0 in 1/6 
		qui replace Year = 1 in 2/6
		qui	replace Year = 2 in 3/6
		qui	replace Year = 3 in 4/6
		qui	replace Year = 4 in 5/6
		qui	replace Year = 5 in 6/6
		qui	gen zero = 0 in 1/6
	
	* Generate dependent variables	
		local fragment logvetopl
		forvalues i=1/10 {
		foreach f of local fragment {
		qui	gen `f'`i' = 100*(f`i'.`f' - `f')
		}
		}
	
	* Generate columns for coefficients	
		foreach s of local sample{
		foreach f of local fragment{
		qui	gen b_f_`f'_`s'=0 in 1/6
		qui	gen se_f_`f'_`s'=0 in 1/6
		}
		}
	
	* LP regressions 
		foreach s of local sample{
		foreach f of local fragment{
		qui reg `f'1 pk_fin dlrgdp dlcpi l.dlrgdp l.dlcpi `f' l.`f'  cendum1-cendum19 if `s' == 1, noconstant cluster(iso)
		qui gen `f'sample_`s' = e(sample)
		local rhslevels dlrgdp dlcpi `f' 
		foreach v of local rhslevels{
		qui	sum `v' if `f'sample_`s'==1  
		qui	gen `f'_`s'_`v' = `v' - r(mean)
		qui	sum l`v' if `f'sample_`s'==1
		qui	gen l`f'_`s'_`v' = l`v' - r(mean)
		}
		forvalues i =1/5 {
		qui	reg `f'`i' pk_fin `f'_`s'_dlrgdp `f'_`s'_dlcpi `f'_`s'_`f' l`f'_`s'_dlrgdp l`f'_`s'_dlcpi l`f'_`s'_`f' cendum1-cendum19 if `f'sample_`s'==1, noconstant cluster(iso)  
		qui	replace b_f_`f'_`s'= _b[pk_fin] if _n == `i' + 1
		qui	replace se_f_`f'_`s'= _se[pk_fin] if _n == `i' + 1 
		eststo LP`f'_`s'_`i'
		}
	
	* Confidence bands	
		qui	gen `f'_up_f_`s' = b_f_`f'_`s' + 1.645 * se_f_`f'_`s'
		qui	gen `f'_low_f_`s' = b_f_`f'_`s' - 1.645 * se_f_`f'_`s'
		}
		}

	* Make figure 
		local fig3 logvetopl
		foreach f of local fig3{
		twoway(rarea `f'_up_f_post `f'_low_f_post Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid))(line b_f_`f'_post Year if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year  if Year <=5, lcolor(black)), graphregion(color(white)) ///
		bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2.5) legend(off) xtitle("") title("Post-WW2 sample", color(white)) name(FigureF1, replace)
		}


		
		
* B) Appendix Tables


* NOTE: Table A1 (Variable description and sources) not replicated here 


* Table A2: Summary statistics

	*Setup table
qui{
		label variable right "Far-right vote share"
		label variable left "Far-left vote share"
		label variable govvote "Government vote share"
		label variable oppvote "Opposition vote share"
		label variable frac "Fractionalization of parliament"
		label variable partycount "No. of parties in parliament"
		label variable strikes "No. of general strikes"
		label variable riots "No. of violent riots"
		label variable demos "No. of anti-government demonstrations"
		label variable protests "No. of street protest incidents" 
		label variable govcris "No. of major government crises"
		label variable turnover "Executive turnover dummy"
		label variable crisisJST "Financial crises dummy"  
		label variable pk_fin "Financial recession dummy"
		label variable pk_norm "Normal recession dummy"
		label variable pk_dis "Non-financial disaster dummy"
}	
	*Make table
		labsumm right left govvote oppvote frac partycount strikes riots demos protests govcris turnover crisisJST pk_fin pk_norm pk_dis

	
/*NOTE: Table B1-B2 and D1-D3 not replicated here. Table B1: Variable "election" indicates election years. Table B2: List of parties in text only. Table D1: Variable "crisisJST" indicates financial crisis years. 
	Table D2: Variable "pk_fin" indicates financial recessions, variable "pk_norm" indicates normal recessions. Table D3: Variable "pk_dis" indicates non-financial macro-economic disasters.*/

		
* Tables E1 to E5: Local projections 

qui{
	* Transform variables for LPs
		gen loggovvote = log(1+govvote)
		gen logoppvote = log(1+oppvote)
		gen logfrac = log(1+frac)
		gen lpartycount = log(1+partycount)
		local vlags loggovvote logoppvote logfrac lpartycount protestsdev  
		foreach v of local vlags {
		gen l`v' = l.`v'
		}	
	* Generate full and pre sample
		gen full = 0
		replace full = 1 if (year >=1870 & year <= 1913) | (year >= 1919 & year <= 1939) | (year>=1950 & year<=2014) 
		gen pre = 0
		replace pre = 1 if (year >=1870 & year <= 1913) | (year >= 1919 & year <= 1939)
		local samples full pre post 

	* Generate dependent variables	
		local fragment loggovvote logoppvote logfrac lpartycount protestsdev
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
		label variable loggovvote "TABLE E1: GOVERNMENT VOTE SHARES"
		label variable logoppvote "TABLE E2: OPPOSITION VOTE SHARES"
		label variable logfrac "TABLE E3: FRACTIONALIZATION OF PARLIAMENT"
		label variable lpartycount "TABLE E4: NO. OF PARTIES IN PARLIAMENT"
		label variable protestsdev "TABLE E5: NO. OF STREET PROTEST INCIDENTS (% DEVIATION FROM TREND)"
		foreach f of local fragment{
		label var `f'1 "Year 1"
		label var `f'2 "Year 2"
		label var `f'3 "Year 3"
		label var `f'4 "Year 4"
		label var `f'5 "Year 5"
		}
	
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

	* Setup tables
		foreach f of local fragment{
		local mylabel_`f': variable label `f'
	
	* Make tables	
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


clear
/**********************************************************************************/
