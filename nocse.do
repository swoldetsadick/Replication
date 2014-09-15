#delimit;
clear;
drop _all;
log using nocse.log,replace;

use wbes;

gen pip = string(vsal, "%20.0f");
gen pep = substr(pip,1,6);
set more off;

*........... selects non OECD countries ............................;

keep if region1~=7;


*###################### filters ######################################;
* eliminating missing values.....;

drop if pep == "999999";

* vsal is sales;
drop if vsal== .;
drop if age==.;

* drops a large number of firms with very suspicious total annual value of sales equal to $1;

drop if vsal<1000;




gen duage= vsal*0;

replace duage  =0 if  7>age;
replace duage  =1 if  6<age&16>age;
replace duage  =2 if 15<age&26>age;
replace duage  =3 if 25<age&51>age;
replace duage  =4 if 50<age;

gen lfatt= log(vsal);
gen vfatt= vsal/1000;

drop if lfatt<0;


* ....... creates dummy for financially constrained firms ......................................;

gen ind3 = age*0;
replace ind3=1 if gcf==4;
* gcf is general constraint financing a score of 4 is major obstacle;
* firms with missing values for gcf are assigned to the non fc group, unless no answer to the key survey question was given;
* however, results not significantly affected in case all observations with gcf==. are dropped;
replace ind3=. if gcf==.&infr==.&gcpi==.&infl==.&exr==.&scri==.&ocri==.&txreg==.&gcorr==.;

drop if ind3==.;







*---------------------------------;
*kernel for fin constr. non OECD  ;
*---------------------------------;

kdensity lfatt,  nogr w(1.5) gen (x fx);
kdensity lfatt, nogr w(1.5) gen (fx0) at(x);
kdensity lfatt if ind3==1, nogr w(1.5) gen (fx1) at(x);
kdensity lfatt if ind3~=1, nogr w(1.5) gen (fx2) at(x);
label var fx0 "(a) total (2,597 firms)";
label var fx1 "(b) fc (864 firms)";
label var fx2 "(c) total net of fc (1,733 firms)";
line fx0 fx1 fx2  x, sort ytitle(Density) xtitle(ln sales)
  clp("l" "." "###-") mfc(white) mlc(white) graphregion(fcolor(white))
  lwidth(medium medthick thick)
  color(black black black)
legend(col(1) pos(6) ring(0) region(lstyle(none) fcolor(none)) size(small))
text(.13 20 "Kolmogorov-Smirnov tests"
            "for equality of distributions:"
            "(b)=(c): p-value 0.00"
            "(a)=(c): p-value 0.02",
place(ne) just(left) size(small));
graph export ind3nocse.eps,replace;
drop fx fx0 fx1 fx2  x;
tabstat vfatt, stats(count mean);
tabstat vfatt if ind3==1, stats(count mean) ;
tabstat vfatt if ind3~=1, stats(count mean) ;
ksmirnov lfatt, by(ind3);

*..... test for tot sample = sample of non fin constr....;


sum lfatt;
sum lfatt if ind3==1;
keep lfatt ind3 size_f;
save appocse,replace;
drop if ind3==1;
gen tot=lfatt*0;
append using appocse;
replace tot=1 if tot==.;
ksmirnov lfatt, by(tot);
ksmirnov size_f, by(tot);
log close;
