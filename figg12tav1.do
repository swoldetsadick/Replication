#delimit;
clear;
drop _all;
log using figg12tav1.log,replace;
use  open9201;
format identif %20.0f;

pause on;
set more off;

******************************************************************************;
* Figure 1: Firm size distribution and firm's age in the Mediocredito dataset ;
******************************************************************************;

*drops firms without information on rationing;

drop if razd==.;

*---------------------------;
*    kernel plot            ;
*---------------------------;

kdensity ldip, nogr w(.7) gen (x fx);
kdensity ldip, nogr w(.7) gen (fx0) at(x);
kdensity ldip if duage==0, nogr w(.7) gen (fx1) at(x);
kdensity ldip if duage==1, nogr w(.7) gen (fx2) at(x);
kdensity ldip if duage==2, nogr w(.7) gen (fx3) at(x);
kdensity ldip if duage==3, nogr w(.7) gen (fx4) at(x);
kdensity ldip if duage==4, nogr w(.7) gen (fx5) at(x);
label var fx0 "pooled sample (15,135 obs.)";
label var fx1 "(a) age <7   years  (1,009 obs.)";
label var fx2 "(b) age 7-15  years (4,613 obs.)";
label var fx3 "(c) age 16-25 years (4,276 obs.)";
label var fx4 "(d) age 26-50 years (4,110 obs.)";
label var fx5 "(e) age >50 years   (1,127 obs.)";
line fx0 fx1 fx2 fx3 fx4 fx5 x, sort ytitle(Density)
xtitle("ln n° employees")
lwidth(medthick medium medthick medium medium medthin)
color(black black black black black black)
  clp("l" "--" "--" "__" "-..-" "l")  mfc(white) mlc(white)  graphregion(fcolor(white))
legend(col(1) pos(1) ring(0) region(lstyle(none) fcolor(none)) size(small))
text(.13 8  "Kolmogorov-Smirnov tests"
            "for equality of distributions:"
            "(a)=(b): p-value 0.00"
            "(b)=(c): p-value 0.00"
            "(c)=(d): p-value 0.00"
            "(d)=(e): p-value 0.00",
place(se) just(left) size(small));
graph export age9201.eps,replace;
drop fx fx0 fx1 fx2 fx3 fx4 fx5 x;

sort razd;
*---------------------------------------------------------------------------------------------;
* Tabulates number of observations and descriptive statistics for the pooled 1992-2001 surveys;
* and for firms desiring more credit in the 1998-2001 surveys                                 ;
*  PRODUCES             TABLE1                                                                ;
*---------------------------------------------------------------------------------------------;

by razd:sum dip;
tabstat dip ,stats(count mean median) by(duage);
tabstat dip if razd==1 ,stats(count mean median) by(duage);
tabstat razd ,stats(count mean median) by(duage);
tabstat dip if (desi==1 | desi==0), stats(count mean median) by(duage);
tabstat dip if  desi==1           , stats(count mean median) by(duage);
tabstat desi if  (desi==1  | desi==0) , stats(count mean median) by(duage);

*-------------------------------------------------------;
* Kolmogorov-Smirnov tests for equality of distributions;
*-------------------------------------------------------;

ksmirnov ldip if duage==0 | duage==1,by(duage);
ksmirnov ldip if duage==1 | duage==2,by(duage);
ksmirnov ldip if duage==2 | duage==3,by(duage);
ksmirnov ldip if duage==3 | duage==4,by(duage);



***************************************************************************************;
* Figure 2: FSD and financial constraints: Baseline definition of financial constraints;
***************************************************************************************;


*---------------------;
*kernel plot          ;
*---------------------;


kdensity ldip, nogr w(.7) gen (x fx);
kdensity ldip, nogr w(.7) gen (fx0) at(x);
kdensity ldip if razd==1, nogr w(.7) gen (fx1) at(x);
kdensity ldip if persist==1, nogr w(.7) gen (fx2) at(x);
kdensity ldip if razd~=1, nogr w(.7) gen (fx3) at(x);
label var fx0 "(a) total (15,135 obs.)";
label var fx1 "(b) fc  (736 obs.)";
label var fx2 "(c) persistently fc (17 firms)";
label var fx3 "(d) total net of fc (14,399 obs.)";
line fx0 fx1 fx2 fx3 x, sort ytitle(Density) xtitle(ln n° employees)
  clp("l" "." "_#_" "###-") mfc(white) mlc(white)  graphregion(fcolor(white))
lwidth(medium medthick medium thick)
legend(col(1) pos(1) ring(0) region(lstyle(none) fcolor(none)) size(small))
color(black black black black)
text(.13 8  "Kolmogorov-Smirnov tests"
            "for equality of distributions:"
            "H1: (b)=(d): p-value 0.47"
            "H2: (a)=(d): p-value 1.00"
            "H1: (c)=balanced sample:"
            "             p-value 0.01",
place(se) just(left) size(small));
graph export raz9201.eps,replace;
drop fx fx0 fx1 fx2 fx3 x;

*-------------------------------------------------------;
* Kolmogorov-Smirnov tests for equality of distributions;
* (b) versus (d) in Figure 2                            ;
*-------------------------------------------------------;
ksmirnov ldip,by(razd);


*---------------------------------------------------------------------------------------------;
* Tabulates number of observations for persistently rationed firms                            ;
*                                                                                             ;
*---------------------------------------------------------------------------------------------;
tabstat dip if persist==1, stats(count mean median);


pause;


***************************************************************************************;
* Figure 3: FSD and financial constraints: Baseline definition of financial constraints;
*           Young firms  (less than 7 years)                                           ;
***************************************************************************************;

*----------------------------------------;
*kernel plot   young                     ;
*----------------------------------------;

keep if duage==0;

kdensity ldip, nogr w(.7) gen (x fx);
kdensity ldip, nogr w(.7) gen (fx0) at(x);
kdensity ldip if (razd==1), nogr w(.7) gen (fx1) at(x);
kdensity ldip if razd~=1, nogr w(.7) gen (fx2) at(x);
label var fx0 "(a) total (1,009 obs.)";
label var fx1 "(b) fc  (59 obs.)";
label var fx2 "(c) total net of fc (950 obs.)";
line fx0 fx1 fx2 x, sort ytitle(Density) xtitle(ln  n° employees)
  clp("l" "." "###-") mfc(white) mlc(white)  graphregion(fcolor(white))
  lwidth(medium medthick thick)
  color(black black black)
legend(col(1) pos(2) ring(0) region(lstyle(none) fcolor(none)) size(small))
text(.13 8  "Kolmogorov-Smirnov tests"
            "for equality of distributions:"
            "H1: (b)=(c): p-value 0.54"
            "H2: (a)=(c): p-value 1.00",
place(se) just(left) size(small));
graph export razy9201.eps,replace;
drop fx fx0 fx1 fx2 x;

*-------------------------------------------------------;
* Kolmogorov-Smirnov tests for equality of distributions;
* (b) versus (c) in Figure 3                            ;
*-------------------------------------------------------;
ksmirnov ldip,by(razd);

*---------------------------------------------------------------------------------------------;
* Tabulates number of observations and descriptive statistics                                 ;
* Looks at rationed, persistently rationed firms and firms desiring more credit               ;
*---------------------------------------------------------------------------------------------;
tabstat dip, stats(count mean);
tabstat dip if (razd==1), stats(count mean median);
tabstat dip if persist==1, stats(count mean median);
tabstat dip if (razd~=1), stats(count mean median);
sum razd desi ;
 pause;


*---------------------------------------------------------------------------------------------;
* . kolmogorov-smirnov tests for total sample vs non financially constrained, total and young.;
* Fig 2  (a) versus (d) ;
* Fig 3  (a) versus (c) ;
*---------------------------------------------------------------------------------------------;

clear;
drop _all;
use open9201;
drop if razd==.;
gen nonraz=anno*0;
drop if razd==1;
append using open9201;
drop if razd==.;
replace nonraz=1 if nonraz==.;
ksmirnov ldip, by(nonraz);

keep if duage==0;
ksmirnov ldip, by(nonraz);


* ........... kolmogorov-smirnov tests for balanced sample versus persistently constrained .;
* ........... Fig. 2 (c) versus balanced sample  ;

clear;
drop _all;
use closed9201;
drop if razd==.;
keep if anno==1994|anno==2000;
ksmirnov ldip, by(persist);


log close;
