#delimit;
clear;
drop _all;
log using ols.log,replace;
* this version - .6/-.6  ddip  ;
* this version -        filters;


pause off;
set more off;
set mem 1g;

use tempstime_1;
drop if identif==.;


drop if ddip>=.6 &ddip~=.;
drop if ddip<=-.6 &ddip~=.;

drop if roa>=.6 &roa~=.;
drop if roa<=-.6 &roa~=.;


drop if madum==1;
replace eta=.5 if eta==0;
gen leta=log(eta);

keep ddip dip roa ldip roa1 duofut dofuy dofup dimm dimmy dimmp eta leta dupav* identif anno
razd razdy razdp dudex dudexy dudexp  ofut anno;
sort identif anno;



for num 89/99: gen duX=(anno==19X);



sort identif anno;
tsset identif anno,yearly;




* fc baseline def.          - ols regression;
regress ddip l.roa razd razdy razdp leta  du9* dupav1 dupav3 dupav4 if anno==1991 | anno==1994
| anno==1997 | anno==2000
,robust;

regress ddip l.roa razd razdy  leta  du9* dupav1 dupav3 dupav4 if anno==1991 | anno==1994
| anno==1997 | anno==2000
,robust;

drop if anno <1995;
*desiring;


* desiring          - ols regression;
regress ddip l.roa dudex dudexy dudexp leta  du9* dupav1 dupav3 dupav4 if
 anno==1997 | anno==2000
,robust;

log close;
