#delimit;
clear;
drop _all;
log using dataset.log,replace;
set mem 1g;


**************************************************************;
**************************************************************;
*  LOADS MEDIOCREDITO SURVEY & BALANCE-SHEET DATA      *******;
*  FOR THE DESCRIPTION OF THE DATASET datasur          *******;
*  see the readme file                                        ;
*  this program generates and saves the relevant variables    ;
*  used in the paper                                          ;
**************************************************************;
**************************************************************;
use datasur;

* --------------------------------;
* identif  IS THE FIRM IDENTIFIER ;
* anno     IS THE YEAR            ;
* --------------------------------;

* --------------------------------------;
* LOADS 1992 SURVEY & BALANCE SHEET DATA;
* --------------------------------------;


keep if ind92==1;

format identif %20.0f;

drop if dip91==.;
drop if dip91<=0;


* SIZE AS PROXIED BY TOTAL SALES;
gen lsize =log(sales);

* DUMMY RATIONING BASELINE DEF.;

gen razd=0;
replace razd=1 if d17_1==3 | d17_2==3 | d17_3==3 | d17_4==3;
replace razd=. if d17_1==9 & d17_2==9 & d17_3==9 & d17_4==9;


gen raz92d=razd;

* DUMMIES M&As;

gen duacq=identif*0;
replace duacq=1 if acqui==1;
drop acqui;

gen dusco=identif*0;
replace dusco=1 if scorpo==1;
drop scorpo;


* 4 PAVITT SECTORAL DUMMIES;

gen dupav1=identif*0;
replace dupav1=1 if pav==1;

gen dupav2=identif*0;
replace dupav2=1 if pav==2;

gen dupav3=identif*0;
replace dupav3=1 if pav==3;

gen dupav4=identif*0;
replace dupav4=1 if pav==4;
drop pav ;

sort identif;

* DEFINING BALANCE-SHEET VARIABLES;

* RETURN ON ASSETS;
gen roa=util/(aco+aim);

* FINANCIAL EXPENSE/OPERATING PROFITS;
gen ofut=of/(of+utiln);

* SHARE OF TANGIBLE ASSETS OVER TOTAL ASSETS;
gen quoimm=imte/(aco+aim);

* LOG OF SALES;
gen lsales=log(sales);


*GENERATING PERCENTILES FOR THE DUMMIES LOW COVERAGE - LOW COLLATERAL;

egen pofut=pctile(ofut),p(75) by(anno);
gen duofut=identif*0;
replace duofut=1 if ofut>pofut & ofut ~=.;
replace duofut=. if ofut==.;

egen pquoimm=pctile(quoimm),p(25) by(anno);
gen dimm=identif*0;
replace dimm=1 if quoimm<pquoimm &quoimm~=.;
replace dimm=. if quoimm==.;

save csur92.dta,replace;

drop _all;
use datasur;

* --------------------------------------;
* LOADS 1995 SURVEY & BALANCE SHEET DATA;
* --------------------------------------;
keep if ind95==1;
format identif %20.0f;

set more off;

drop if dip94==.;
drop if dip94<=0;

* SIZE AS PROXIED BY TOTAL SALES;
gen lsize =log(sales);

* DUMMY RATIONING TAKING ACCOUNT MISSING VALUES;

gen razd=identif*0;
replace razd=1 if nonotten==1;

gen raz95d=razd;
replace razd=. if nonotten==. & avrebacc==.;


* DUMMIES M&As;

gen duacq=identif*0;
replace duacq=1 if acqui==1;
drop acqui;

gen dusco=identif*0;
replace dusco=1 if scorpo==1;
drop scorpo;


* DUMMY INDUSTRIAL GROUP;

gen dugr=identif*0;
replace dugr=1 if isgru ==1;
drop isgru ;
rename dugr isgru ;

* 4 PAVITT SECTORAL DUMMIES;

gen dupav1=identif*0;
replace dupav1=1 if pav==1;

gen dupav2=identif*0;
replace dupav2=1 if pav==2;

gen dupav3=identif*0;
replace dupav3=1 if pav==3;

gen dupav4=identif*0;
replace dupav4=1 if pav==4;
drop pav ;

sort identif;

* RETURN ON ASSETS;
gen roa=util/(aco+aim);

* FINANCIAL EXPENSE/OPERATING PROFITS;
gen ofut=of/(of+utiln);

* SHARE OF TANGIBLE ASSETS OVER TOTAL ASSETS;
gen quoimm=imte/(aco+aim);

* LOG OF SALES;
gen lsales=log(sales);


*GENERATING PERCENTILES FOR THE DUMMIES LOW COVERAGE - LOW COLLATERAL;

egen pofut=pctile(ofut),p(75) by(anno);
gen duofut=identif*0;
replace duofut=1 if ofut>pofut & ofut ~=.;
replace duofut=. if ofut==.;

egen pquoimm=pctile(quoimm),p(25) by(anno);
gen dimm=identif*0;
replace dimm=1 if quoimm<pquoimm &quoimm~=.;
replace dimm=. if quoimm==.;

save csur95.dta,replace;

drop _all;

use datasur;
* --------------------------------------;
* LOADS 1998 SURVEY & BALANCE SHEET DATA;
* --------------------------------------;
keep if ind98==1;
format identif %20.0f;


drop if eta98<0;

drop if dip97==.;

drop if dip97<=0;


* DUMMIES M&As;

gen duacq=identif*0;
replace duacq=1 if acqui==1;
drop acqui;

gen dusco=identif*0;
replace dusco=1 if scorpo==1;
drop scorpo;


* 4 PAVITT SECTORAL DUMMIES;

gen dupav1=identif*0;
replace dupav1=1 if pav==1;

gen dupav2=identif*0;
replace dupav2=1 if pav==2;

gen dupav3=identif*0;
replace dupav3=1 if pav==3;

gen dupav4=identif*0;
replace dupav4=1 if pav==4;
drop pav ;

* DUMMY INDUSTRIAL GROUP;

gen dugr=identif*0;
replace dugr=1 if isgru==1;
drop isgru;
rename dugr isgru;

* DUMMY RATIONING BASELINE DEF;


gen razd=identif*0;

replace razd=1 if desi==1&chie==1;
replace razd=. if desi==.&paga==.&chie==.;

gen raz98d=razd;

* DUMMY DESIRING MORE CREDIT;

gen desi1=desi;

drop desi;

gen desi=identif*0;
replace desi=1 if desi1==1;
replace desi=0 if desi1==2;
replace desi=. if desi1==.&paga==.&chie==.;
drop desi1;
gen desi98=desi;


sort identif;

* RETURN ON ASSETS;
gen roa=util/(aco+aim);

* FINANCIAL EXPENSE/OPERATING PROFITS;
gen ofut=of/(of+utiln);

* SHARE OF TANGIBLE ASSETS OVER TOTAL ASSETS;
gen quoimm=imte/(aco+aim);

* LOG OF SALES;
gen lsales=log(sales);


*GENERATING PERCENTILES FOR THE DUMMIES LOW COVERAGE - LOW COLLATERAL;
egen pofut=pctile(ofut),p(75) by(anno);
gen duofut=identif*0;
replace duofut=1 if ofut>pofut & ofut ~=.;
replace duofut=. if ofut==.;


egen pquoimm=pctile(quoimm),p(25) by(anno);
gen dimm=identif*0;
replace dimm=1 if quoimm<pquoimm & quoimm~=.;
replace dimm=. if quoimm==.;
sort identif anno;
save csur98.dta,replace;

drop _all;


use datasur;
* --------------------------------------;
* LOADS 2001 SURVEY & BALANCE SHEET DATA;
* --------------------------------------;

format identif %20.0f;

keep if ind01==1;
set more off;

drop if eta01<0;

drop if dip00==.;

drop if dip00<=0;


* DUMMIES M&As;

gen duacq=identif*0;
replace duacq=1 if acqui==1;
drop acqui;

gen dusco=identif*0;
replace dusco=1 if scorpo==1;
drop scorpo;

* 4 PAVITT SECTORAL DUMMIES;

gen dupav1=identif*0;
replace dupav1=1 if pav==1;

gen dupav2=identif*0;
replace dupav2=1 if pav==2;

gen dupav3=identif*0;
replace dupav3=1 if pav==3;

gen dupav4=identif*0;
replace dupav4=1 if pav==4;
drop pav ;

* DUMMY INDUSTRIAL GROUP;

gen dugr=identif*0;
replace dugr=1 if isgru==1;
drop isgru;
rename dugr isgru;

* DUMMY RATIONING BASELINE DEF;

gen razd=identif*0;

replace razd=1 if desi==1&chie==1;
replace razd=. if desi==.&paga==.&chie==.;

gen raz01d=razd;

* DUMMY DESIRING MORE CREDIT;

gen desi1=desi;

drop desi;

gen desi=identif*0;
replace desi=1 if desi1==1;
replace desi=0 if desi1==2;
replace desi=. if desi1==.&paga==.&chie==.;
drop desi1;
gen desi01=desi;

sort identif;

* RETURN ON ASSETS;
gen roa=util/(aco+aim);

* FINANCIAL EXPENSE/OPERATING PROFITS;
gen ofut=of/(of+utiln);

* SHARE OF TANGIBLE ASSETS OVER TOTAL ASSETS;
gen quoimm=imte/(aco+aim);

* LOG OF SALES;
gen lsales=log(sales);


*GENERATING PERCENTILES FOR THE DUMMIES LOW COVERAGE - LOW COLLATERAL;
egen pofut=pctile(ofut),p(75) by(anno);
gen duofut=identif*0;
replace duofut=1 if ofut>pofut & ofut ~=.;
replace duofut=. if ofut==.;

egen pquoimm=pctile(quoimm),p(25) by(anno);
gen dimm=identif*0;
replace dimm=1 if quoimm<pquoimm & quoimm~=.;
replace dimm=. if quoimm==.;
sort identif anno;
save csur01.dta,replace;

drop _all;


***********************************************************************************************;
************************************ 1992 - 1995 surveys **************************************;
***********************************************************************************************;
*****************************ELIMINATES FIRMS WITH ERRORS IN THE AGE FIELD ********************;
*****************************FIXES AGE FIELDS IN CASE AGE IS MISSING **************************;
**************************** CAN BE DONE ONLY FOR FIRMS PRESENT IN BOTH SURVEYS ***************;
**************************** ALSO, CREATES DUMMIES FOR PERSISTENTLY CONSTRAINED FIRMS *********;
***********************************************************************************************;

use csur92;
keep if anno==1991;
keep identif eta92 raz92d;
sort identif;
save csuro92.dta,replace;
drop _all;
use  csur95;
keep if anno==1994;
keep identif eta95 raz95d;
sort identif;
save csuro95.dta,replace;
drop _all;

use csuro92;
merge identif using csuro95;
sum eta92 eta95 if _merge==3;


*.. uses age from one survey to compute age in other survey for firms present in both surveys that have...;
*.. mising data for age in one survey;

replace eta92=eta95-3 if eta92==. & eta95~=. & _merge==3;
replace eta92=eta95-3 if eta92<0  & eta95~=. & _merge==3;

replace eta95=eta92+3 if eta95==. & eta92~=. & _merge==3;
replace eta95=eta92+3 if eta95<0  & eta92~=. & _merge==3;
rename eta92 eta92c;
rename eta95 eta95c;

*.. drops firms in the balanced sample with large errors in the age field ................................;

gen deta=eta95c-eta92c;
sum eta92c eta95c if _merge==3;

keep if deta>1|deta==.;
keep if deta<5|deta==.;


*.. forces small differences (+ or -1) in age field to square to 3 ................................................;

replace eta95c =eta92c+3 if deta==2|deta==4;
sum deta;

*.. dummy for persistently constrained firms ....................................................................;

gen persist=identif*0;
replace persist=1 if raz92d==1&raz95d==1;
keep identif eta92c eta95c persist;

sort identif;
save temp9295, replace;

*.................................... corrects and saves 1992 survey ........................................................;

clear;
drop _all;
use csur92;
merge identif using temp9295;
keep if _merge==3;
replace eta92=eta92c;

*.. resets persist to zero in 1992 .............................................................................;
replace persist=0;

drop _merge eta92c eta95c;
drop if eta92<0;
sort identif;
save csur92, replace;

*.................................... corrects and saves 1995 survey ........................................................;

clear;
drop _all;
use csur95;
merge identif using temp9295;
keep if _merge==3;
replace eta95=eta95c;
drop _merge eta92c eta95c;
drop if eta95<0;
sort identif;
save csur95, replace;

set type float;



***********************************************************************************************;
************************************* 1998 - 2001 surveys *************************************;
***********************************************************************************************;
*****************************ELIMINATES FIRMS WITH ERRORS IN THE AGE FIELD ********************;
*****************************FIXES AGE FIELDS IN CASE AGE IS MISSING **************************;
**************************** CAN BE DONE ONLY FOR FIRMS PRESENT IN BOTH SURVEYS ***************;
**************************** ALSO, CREATES DUMMIES FOR PERSISTENTLY CONSTRAINED FIRMS *********;
**************************** fixes wrong data in the age field for firms in the closed 98-01***;
**************************** panel. Uses age data from  cerved (eta01n) ***********************;
***********************************************************************************************;



clear;
drop _all;
use csur98;
keep if anno==1997;
keep identif eta98 eta98n raz98d  desi98;
sort identif;
save temp98.dta,replace;

drop _all;
use csur01;
keep if anno==2000;
keep identif eta01 eta01n raz01d  desi01;
sort identif;
save temp01.dta,replace;

merge identif using temp98.dta;

drop eta98n;
gen eta98n=eta01n-3;

gen diff=eta01-eta98;

replace eta98=eta98n if diff~=3 & eta01==eta01n & eta98n~=. & eta98n>=0;
replace eta01=eta01n if diff~=3 & eta98==eta98n & eta01n~=. & eta01n>=0;
*drop eta98n eta01n;
*.. uses age from one survey to compute age in other survey for firms present in both surveys that have...;
*.. mising data for age in one survey;

replace eta98=eta01-3 if eta98==. & eta01~=.;
replace eta98=eta01-3 if eta98<0  & eta01~=.;

replace eta01=eta98+3 if eta01==. & eta98~=.;
replace eta01=eta98+3 if eta01<0  & eta98~=.;



*.. drops firms in the balanced sample with large errors in the age field ................................;

gen deta=eta01-eta98;
sum deta if _merge==3;
keep if deta>1|deta==.;
keep if deta<5|deta==.;

*.. forces small differences (+ or -1) in age field to square to 3 ................................................;
replace eta01 =eta98+3 if deta==2|deta==4;

rename eta98 eta98c;
rename eta01 eta01c;


*.. dummies for persistently constrained firms ....................................................................;

gen persist=identif*0;
gen perdesi=identif*0;
replace persist=1 if raz98d==1&raz01d==1;
replace perdesi=1 if desi98==1&desi01==1;
keep identif eta98c eta01c persist perdesi;

sort identif;
save temp9801, replace;


*.................................... corrects and saves 1998 survey ........................................................;

clear;
drop _all;
use csur98;
sort identif anno;
merge identif using temp9801;
replace eta98=eta98c if eta98c~=.;
drop eta98c eta01c;

*.. resets persist to zero in 1998 .............................................................................;
replace persist=0;
replace perdesi=0;
keep if _merge==3;
drop _merge;
sort identif anno;
save csur98, replace;

*.................................... corrects and saves 2001 survey ........................................................;



clear;
drop _all;
use csur01;
sort identif;
merge identif using temp9801;
replace eta01=eta01c if eta01c~=.;
drop eta98c eta01c;
keep if _merge==3;
drop _merge;
sort identif anno;
save csur01, replace;


*... loads main dataset........................................................;
clear;
drop _all;


use csur92;
sort identif anno;
gen eta=eta92 if eta92~=.;
replace eta=eta92-1 if anno==1990 & eta92~=.;
replace eta=eta92-2 if anno==1989 & eta92~=.;
save csur92,replace;

use csur95;
sort identif anno;
gen eta=eta95 if eta95~=.;
replace eta=eta95-1 if anno==1993 & eta95~=.;
replace eta=eta95-2 if anno==1992 & eta95~=.;
save csur95,replace;

use csur98;
sort identif anno;
gen eta=eta98 if eta98~=.;
replace eta=eta98-1 if anno==1996 & eta98~=.;
replace eta=eta98-2 if anno==1995 & eta98~=.;
save csur98,replace;

use csur01;
sort identif anno;
gen eta=eta01 if eta01~=.;
replace eta=eta01-1 if anno==1999 & eta01~=.;
replace eta=eta01-2 if anno==1998 & eta01~=.;
save csur01,replace;

drop _all;
use csur92;
append using csur95;
append using csur98;
append using csur01;


sort identif anno;

sum anno;
sum anno if eta==.;

by identif:gen roa1=roa[_n-1];
sort identif anno;
by identif: gen detab=eta-eta[_n-1];
by identif: gen danno=anno-anno[_n-1];
sum detab danno;

list identif anno if detab~=1&detab~=.;
list identif anno if danno~=1&danno~=.;

list identif anno if detab~=1&detab~=. &danno==1;
list identif anno if danno~=1&danno~=.;


gen dip=.;
replace dip=dip89 if anno==1989;
replace dip=dip90 if anno==1990;
replace dip=dip91 if anno==1991;
replace dip=dip92 if anno==1992;
replace dip=dip93 if anno==1993;
replace dip=dip94 if anno==1994;
replace dip=dip95 if anno==1995;
replace dip=dip96 if anno==1996;
replace dip=dip97 if anno==1997;
replace dip=dip98 if anno==1998;
replace dip=dip99 if anno==1999;
replace dip=dip00 if anno==2000;


*... interpolates possible missing values of n° dip. ......................................................;
sort identif anno;
by identif: replace dip=(dip[_n+1]+dip[_n-1])/2 if dip==.&
                             (dip[_n+1]~=.&dip[_n-1]~=.&identif[_n-1]==identif[_n+1]);

by identif:gen ddip=((dip-dip[_n-1])/dip[_n-1]);
gen ldip=log(dip);
by identif:gen ldip1=log(dip[_n-1]);
by identif:gen ldip2=log(dip[_n-2]);


gen duage=.;
replace duage=0 if  7>eta;
replace duage=1 if  6<eta&16>eta;
replace duage=2 if 15<eta&26>eta;
replace duage=3 if 25<eta&51>eta;
replace duage=4 if 50<eta&eta~=.;
gen ugo=7;

gen dupico=eta*0;

replace  dupico=1 if dip<50;

gen razdy  =eta*0;
replace razdy=1 if razd==1&eta<ugo;
replace razdy=. if razd==.;

gen dimmy  =eta*0;
replace dimmy=1 if dimm==1&eta<ugo;

gen dofuy  =eta*0;
replace dofuy=1 if duofut==1&eta<ugo;

gen razdp  =eta*0;
replace razdp=1 if razd==1&dupico==1;
replace razdp=. if razd==.;


gen dimmp  =eta*0;
replace dimmp=1 if dimm==1&dupico==1;

gen dofup  =eta*0;
replace dofup=1 if duofut==1&dupico==1;


*set more off;

* TOTAL ASSETS;
gen ato=aco+aim;

gen ldim=log(ato) if ato~=.;
gen dudex= desi*0;
replace dudex=1 if desi==1;

gen dudexy  =eta*0;
replace dudexy=1 if dudex==1&eta<ugo;
replace dudexy=. if desi==.;

gen dudexp  =eta*0;
replace dudexp=1 if dudex==1&dupico==1;
replace dudexp=. if desi==.;



sort identif anno;
save tempstime.dta,replace;




*... flags young firms in years in which they  have been involved in M&As.................................................;
*... also flags a few large young firms which create a hump in the far r.h.s. tail of the distr. of young fc firms .......;

gen razo=razd;
replace razo=0 if razd==.;

egen pdip=pctile(dip),p(90) by(duage);
gen dudim=razo*0;
replace dudim=1 if dip>pdip &dip~=.;

egen pfat=pctile(lsales),p(90) by(duage);
gen dudim1=razo*0;
replace dudim1=1 if lsales>pfat & lsales~=.;

gen ippo=razd*0;
replace ippo=1 if dudim==1 & dudim1==1 & duage==0;

keep if (dusco==1&eta<7|duacq==1&eta<7) | ippo==1;
sort identif;
collapse dip, by(identif);
sort identif;
keep identif;
save temp_m&a.dta,replace;

clear;
drop _all;
use tempstime.dta,replace;
merge identif using temp_m&a;
*format identif %20.0f;
gen madum=1 if _merge==3;
drop _merge;
sort identif anno;
drop if eta==.;
drop if eta<0;
drop if dip==.;
drop if dip<1;
save tempstime_1.dta,replace;


use tempstime_1.dta,replace;

*################# CREATING THE DATASET FOR THE CHARTS AND TABLE 1 ####################################################;




*^^^^ closed dataset ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;


*..... keeps the four survey years ....................................................................................;

keep if anno==1991|anno==1994|anno==1997|anno==2000;
drop if dip==.;
drop if eta==.;

*.. eliminates firms present in both surveys but with missing key data in one of the two surveys ...........;
sort identif anno;
collapse (count) nobs=dip, by(identif);
keep if nobs==2;
keep identif;
sort identif;
save tempclo, replace;

clear;
drop _all;
use tempstime_1.dta;

*..... keeps the four survey years ....................................................................................;

keep if anno==1991|anno==1994|anno==1997|anno==2000;

sort identif;
set type double;
merge identif using tempclo;
keep if _merge==3;
drop _merge;
sort identif anno;
save closed9201, replace;





*^^^^ open dataset ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^;

clear;
drop _all;
use tempstime_1.dta,replace;
*..... keeps the four survey years ....................................................................................;

format identif %20.0f;
des;

keep if anno==1991|anno==1994|anno==1997|anno==2000;
des;


* drops young firms which have been involved in M&As ..................................................................;

drop if  dusco==1 & duage==0;
drop if  duacq==1 & duage==0;


* drops a few large young firms which create a hump in the far right hand tail of the distribution of young fc firms ......;
* these drops are equivalent to those flagged with madum ==1 in the regressions dataset. Dropping or not dropping .........;
* madum==1 does nto qualitatively affect the estimates, but does affect somewhat the figures ..............................;
* firms with madum==1 are large young firms ...............................................................................;

egen pdip=pctile(dip),p(90) by(duage);
gen dudim=razd*0;
replace dudim=1 if dip>pdip &dip~=.;

des;
egen pfat=pctile(lsales),p(90) by(duage);
gen dudim1=razd*0;
replace dudim1=1 if lsales>pfat & lsales~=.;
des;

gen ippo=razd*0;
replace ippo=1 if dudim==1 & dudim1==1 & duage==0;
sum ippo if ippo==1;

drop if ippo==1;

* .. these drops are merely for adjusting the scale of the horizontal axis ................................................;

drop if dip>60000;
drop if dip<=5;

save open9201, replace;
log close;
