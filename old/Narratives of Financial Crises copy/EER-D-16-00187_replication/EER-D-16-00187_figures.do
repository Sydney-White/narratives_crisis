


/**********************************************************************************/
* Going to Extremes: Politics after Financial Crises, 1870-2014 
* Manuel Funke, Christoph Trebesch, Moritz Schularick 

* Replicates version that has been accepted at EER, March 2016 


* Replication code for Figure 1 to 8 in the main text 

	* IMPORTANT NOTE: This file requires the "grc1leg" ado (for replication of Figure 4). To install, open Stata and type: ssc install grc1leg. If this doesn't work, type findit grc1leg. 		

		
use C:\...\EER-D-16-00187_data.dta, clear

sort ccode year
tsset ccode year, yearly
estimates clear

* Figure 1: Vote shares of the far right and left 

	* Generate global war dummy 
		qui gen warx=0
		qui	replace warx=1 if year>=1914 & year<=1918
		qui	replace warx=1 if year>=1939 & year<=1949

	* Remove crises whith pre/post windows < 5 years (WWs/follow-up crises/sample limits)    
		qui	gen crisis_f1 = crisisJST 
		qui	replace crisis_f1=0 if (warx[_n-5]==1 | warx[_n-4]==1 | warx[_n-3]==1 | warx[_n-2]==1 | warx[_n-1]==1 | warx==1 | warx[_n+5]==1 | warx[_n+4]==1 | warx[_n+3]==1 | warx[_n+2]==1 | warx[_n+1]==1 ) 
		qui	replace crisis_f1=0 if (ccode[_n-5]!=ccode | ccode[_n-4]!=ccode | ccode[_n-3]!=ccode | ccode[_n-2]!=ccode | ccode[_n-1]!=ccode | ccode[_n+5]!=ccode | ccode[_n+4]!=ccode | ccode[_n+3]!=ccode | ccode[_n+2]!=ccode | ccode[_n+1]!=ccode ) 
		qui	replace crisis_f1=0 if (crisis_f1[_n-11]==1 | crisis_f1[_n-10]==1 | crisis_f1[_n-9]==1 | crisis_f1[_n-8]==1 | crisis_f1[_n-7]==1 | crisis_f1[_n-6]==1 | crisis_f1[_n-5]==1 | crisis_f1[_n-4]==1 | crisis_f1[_n-3]==1 | crisis_f1[_n-2]==1 | crisis_f1[_n-1]==1 )

	* Generate full 5-year pre/post-crisis spells for descriptives 	
		qui	gen prepost = "Post-crisis" if (crisis_f1[_n-5]==1 | crisis_f1[_n-4]==1 | crisis_f1[_n-3]==1 | crisis_f1[_n-2]==1 | crisis_f1[_n-1]==1)
		qui replace prepost = "Pre-crisis" if (crisis_f1[_n+5]==1 | crisis_f1[_n+4]==1 | crisis_f1[_n+3]==1 | crisis_f1[_n+2]==1 | crisis_f1[_n+1]==1) 

	* Make figure 
		graph bar (mean) left right extr, over(prepost, sort(1)) ytitle("Percentage of total votes") legend(rows(1) label(1 "Far-left (Avg.)") label(2 "Far-right (Avg.)") label(3 "Total (Avg.)")) bar(1, color(white) lcolor(black)) bar(2, color(black) lcolor(black)) ///
		bar(3, color(gs10) lcolor(black)) graphregion(color(white)) ysize(3.5) xsize(5) ylabel(0 5 10 15, glcolor(gs10)) name(Figure1, replace)		


* Figure 2: Far-right and right-wing populist votes in European elections 

	* NOTE: This figure is for illustrative purposes only and not replicated here.    


* Figure 3: Far-right vote shares (local projections): financial crisis recessions 

	* Transform variables for LPs
		qui	gen logright = log(1+right)
		qui	gen loggovvote = log(1+govvote)
		qui	gen logoppvote = log(1+oppvote)
		qui	gen logfrac = log(1+frac)
		qui	gen lpartycount = log(1+partycount)
		qui	gen lrgdp = log(rgdp)
		qui	gen dlrgdp =100*d.lrgdp
		qui	gen lcpi = log(cpi)
		qui	gen dlcpi = 100*d.lcpi
		qui	local vlags logright loggovvote logoppvote logfrac lpartycount dlrgdp dlcpi protestsdev  
		foreach v of local vlags {
		gen l`v' = l.`v'
		}	
	
	* Generate samples for LPs	
		qui	gen full = 0
		qui	replace full = 1 if (year >=1870 & year <= 1913) | (year >= 1919 & year <= 1939) | (year>=1950 & year<=2014) 
		qui	gen pre = 0
		qui	replace pre = 1 if (year >=1870 & year <= 1913) | (year >= 1919 & year <= 1939)
		qui	gen post = 0
		qui	replace post = 1 if (year>=1950 & year<=2014) 
		local samples full pre post
		
	* Generate centered country dummies
		forvalues n=1/20 {
		qui	gen cendum`n' = .
		qui	replace cendum`n' = 1 - 1/20 if ccode == `n'
		qui	replace cendum`n' = 0 - 1/20 if ccode != `n'
		}
	
	* Generate year columns
		qui	gen Year = 0 in 1/11 
		qui 	replace Year = 1 in 2/11
		qui	replace Year = 2 in 3/11
		qui	replace Year = 3 in 4/11
		qui	replace Year = 4 in 5/11
		qui	replace Year = 5 in 6/11
		qui	replace Year = 6 in 7/11
		qui	replace Year = 7 in 8/11
		qui	replace Year = 8 in 9/11
		qui	replace Year = 9 in 10/11
		qui	replace Year = 10 in 11/11
		qui	gen zero = 0 in 1/11
	
	* Generate dependent variables	
		local fragment logright loggovvote logoppvote logfrac lpartycount protestsdev
		forvalues i=1/10 {
		foreach f of local fragment {
		qui	gen `f'`i' = 100*(f`i'.`f' - `f')
		}
		}
	
	* Generate columns for coefficients	
		foreach s of local samples{
		foreach f of local fragment{
		qui	gen b_f_`f'_`s'=0 in 1/11
		qui	gen se_f_`f'_`s'=0 in 1/11
		}
		}
	
	* LP regressions 
		foreach s of local samples{
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
		forvalues i =1/10 {
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
		local fig3 logright
		foreach f of local fig3{
		twoway(rarea `f'_up_f_full `f'_low_f_full Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_full Year  if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") title("Full sample") name(All_5_`f', replace)
		twoway(rarea `f'_up_f_pre `f'_low_f_pre Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_pre Year  if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year  if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") title("Pre-WW2 sample") name(Pre_5_`f', replace)
		twoway(rarea `f'_up_f_post `f'_low_f_post Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid))(line b_f_`f'_post Year  if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year  if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") title("Post WW2 sample") name(Post_5_`f', replace)
		graph combine All_5_`f' Pre_5_`f' Post_5_`f', rows(1) ysize(2) xsize(5.5) ycommon graphregion(lcolor(white) color(white) margin(tiny)) name(Figure3, replace) title("`mylabel_`f''", size(large) margin(large)) 
		graph drop All_5_`f' Pre_5_`f' Post_5_`f'
		}

           
* Figure 4: Kernel densities of parliamentary variables 

	* Setup figure
		local parl govvote oppvote frac partycount
		label variable govvote `"Government vote share"' 
		label variable oppvote `"Opposition vote share"'
		label variable frac `"Fractionalization of parliament"'
		label variable partycount `"No. of parties in parliament"'
		foreach p of local parl{
		local mylabel_`p': variable label `p'
		
	* Make figure
		twoway (kdensity `p' if prepost == "Pre-crisis" & dict==0, lcolor(black) lpattern(longdash) lwidth(medium)) (kdensity `p' if prepost == "Post-crisis" & dict==0, lcolor(maroon) lpattern(solid) lwidth(medium)), ytitle("", size(small))  xtitle("") graphregion(color(white)) ylabel(,labsize(small) glcolor(gs14)) xlabel(,labsize(small)) title("`mylabel_`p''", size(medium) color(white) margin(medium)) legend(order(1 "Pre-Crisis" 2 "Post-Crisis") size(small)) name(`p'_full,replace)
		twoway (kdensity `p' if prepost == "Pre-crisis" & year>=1870 & year<=1939 & dict==0, lcolor(black) lpattern(longdash) lwidth(medium)) (kdensity `p' if prepost == "Post-crisis" & year>=1870 & year<=1939 & dict==0, lcolor(maroon) lpattern(solid) lwidth(medium)), ytitle("", size(small))  xtitle("") graphregion(color(white)) ylabel(,labsize(small) glcolor(gs14)) xlabel(,labsize(small)) title("`mylabel_`p''", size(medium) margin(medium)) legend(order(1 "Pre-Crisis" 2 "Post-Crisis") size(small)) name(`p'_pre,replace)
		twoway (kdensity `p' if prepost == "Pre-crisis" & year>=1950 &year<=2014 & dict==0, lcolor(black) lpattern(longdash) lwidth(medium))(kdensity `p' if prepost =="Post-crisis" & year>=1950 & year <=2014 & dict==0, lcolor(maroon) lpattern(solid) lwidth(medium)), ytitle("", size(small))  xtitle("") graphregion(color(white)) ylabel(,labsize(small) glcolor(gs14)) xlabel(,labsize(small)) title("`mylabel_`p''", size(medium) color(white) margin(medium)) legend(order(1 "Pre-Crisis" 2 "Post-Crisis") size(small)) name(`p'_post,replace)
		}
		grc1leg govvote_full oppvote_full frac_full partycount_full, legendfrom(govvote_full) position(6) cols(1) name(full, replace) graphregion(color(white)) title("Full sample", position(12) size(medium)) ysize(6) xsize(3) 
		grc1leg govvote_pre oppvote_pre frac_pre partycount_pre, legendfrom(govvote_pre) position(6) cols(1) name(pre, replace) graphregion(color(white)) title("Pre-WW2 sample", position(12) size(medium))  ysize(6) xsize(3)
		grc1leg govvote_post oppvote_post frac_post partycount_post, legendfrom(govvote_post) position(6) cols(1) name(post, replace) graphregion(color(white)) title("Post-WW2 sample", position(12)  size(medium)) ysize(6) xsize(3)
		grc1leg full pre post, ysize(8) xsize(5.5) legendfrom(full) position(6) cols(3) graphregion(color(white)) name(Figure4, replace)	 
		graph drop full pre post govvote_full oppvote_full frac_full partycount_full govvote_pre oppvote_pre frac_pre partycount_pre govvote_post oppvote_post frac_post partycount_post 
		

* Figure 5: Parliamentary variables (local projections): financial crisis recessions

	* Setup figure
		local fig5 loggovvote logoppvote logfrac lpartycount
		label variable loggovvote "Government vote share" 
		label variable logoppvote "Opposition vote share"
		label variable logfrac "Fractionalization of parliament"
		label variable lpartycount "No. of parties in parliament" 
		foreach f of local fig5{
		local mylabel_`f': variable label `f'
	
	* Make figure
		twoway(rarea `f'_up_f_full `f'_low_f_full Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_full Year if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") name(All_5_`f', replace) 
		twoway(rarea `f'_up_f_pre `f'_low_f_pre Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_pre Year if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off)  xtitle("") name(Pre_5_`f', replace)
		twoway(rarea `f'_up_f_post `f'_low_f_post Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_post Year if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") name(Post_5_`f', replace)
		graph combine All_5_`f' Pre_5_`f' Post_5_`f', rows(1) ysize(2) xsize(5.5) ycommon graphregion(lcolor(white) color(white) margin(tiny)) name(`f'_5, replace) title("`mylabel_`f''", size(medsmall)) 
		graph drop All_5_`f' Pre_5_`f' Post_5_`f'
		}
		graph combine loggovvote_5 logoppvote_5 logfrac_5 lpartycount_5, ysize(8) cols(1) graphregion(color(white)) name(Figure5, replace) title("       Full sample            Pre-WW2 sample    Post-WW2 sample", position(11) size(medsmall) margin(medium))     
		graph drop loggovvote_5 logoppvote_5 logfrac_5 lpartycount_5


* Figure 6: Street protests

	* Make figure
		graph bar (mean) strikes riots demos protests, over(prepost, sort(1)) ytitle("Average no. of incidents per year") legend(rows(2) label(1 "General Strikes") label(2 "Violent Riots") label(3 "Demonstrations") label(4 "Sum of All")) ///
		bar(1, color(bluishgray) lcolor(black)) bar(2, color(black) lcolor(black)) bar(3, color(white) lcolor(black)) bar(4, color(gs10) lcolor(black)) graphregion(color(white)) ysize(4) xsize(5.5)  ylabel(1 2 3, glcolor(gs10) glwidth(thin)) name(Figure6, replace)		  


* Figure 7: Streets protests (local projections): financial crisis recessions (% dev. from trend)

	* Setup figure
		local fig7 protestsdev
		
	* Make figure 
		foreach f of local fig7{
		twoway(rarea `f'_up_f_full `f'_low_f_full Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_full Year  if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") title("Full sample") name(All_5_`f', replace)
		twoway(rarea `f'_up_f_pre `f'_low_f_pre Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_pre Year  if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year  if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") title("Pre-WW2 sample") name(Pre_5_`f', replace)
		twoway(rarea `f'_up_f_post `f'_low_f_post Year if Year <=5, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_post Year  if Year <=5, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year  if Year <=5, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") title("Post WW2 sample") name(Post_5_`f', replace)
		graph combine All_5_`f' Pre_5_`f' Post_5_`f', rows(1) ysize(2) xsize(5.5) ycommon graphregion(lcolor(white) color(white) margin(tiny)) name(Figure7, replace)  
		graph drop All_5_`f' Pre_5_`f' Post_5_`f'
		}

		
* Figure 8: 10-year local projections: financial crisis recessions

	* Setup figure 
		local fig8 logright loggovvote logfrac lpartycount
	
	* Make figure
		foreach f of local fig8{
		twoway(rarea `f'_up_f_full `f'_low_f_full Year if Year <=10, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_full Year if Year <=10, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=10, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") name(All_10_`f', replace) 
		twoway(rarea `f'_up_f_pre `f'_low_f_pre Year if Year <=10, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_pre Year if Year <=10, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=10, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off)  xtitle("") name(Pre_10_`f', replace)
		twoway(rarea `f'_up_f_post `f'_low_f_post Year if Year <=10, fcolor(gs14) lcolor(white) lpattern(solid)) (line b_f_`f'_post Year if Year <=10, lcolor(red) lpattern(solid) lwidth(thick)) (line zero Year if Year <=10, lcolor(black)), graphregion(color(white)) bgcolor(white) ylabel(,nogrid) graphregion(lcolor(white) margin(tiny)) ysize(2.5) xsize(2) legend(off) xtitle("") name(Post_10_`f', replace)
		graph combine All_10_`f' Pre_10_`f' Post_10_`f', rows(1) ysize(2) xsize(5.5) ycommon graphregion(lcolor(white) color(white) margin(tiny)) name(`f'_10, replace) title("`mylabel_`f''", size(medsmall)) 
		graph drop All_10_`f' Pre_10_`f' Post_10_`f'
		}
		graph combine logright_10 loggovvote_10 logfrac_10 lpartycount_10, ysize(8) cols(1)  graphregion(color(white)) name(Figure8, replace) title("       Full sample            Pre-WW2 sample    Post-WW2 sample", position(11) size(medsmall) margin(medium))     
		graph drop logright_10 loggovvote_10 logfrac_10 lpartycount_10


		
clear
/**********************************************************************************/

