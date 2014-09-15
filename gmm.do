#delimit;
clear;
drop _all;
log using gmm.log,replace;
* this version - normal filters;
* this version - normal filters;


pause off;
set more off;
set mem 1g;

use tempstime_1;
gen dudip=0;
replace dudip=1 if (ddip>.6|ddip<-.6)&ddip~=.;

by identif:gen ddanno=anno-anno[_n-1];
gen dudanno=0;
replace dudanno=1 if (ddanno~=1&ddanno~=.);

gen duroa=0;
replace duroa=1 if (roa<-.6|roa>.6)&roa~=.;
sort identif anno;

collapse dudip dudanno duroa, by(identif);
save filterdip, replace;

clear;
drop _all;
use tempstime_1;
merge identif using filterdip;

drop if dudip>0;
drop if dudanno>0;
drop if duroa>0;
drop if madum==1;
drop if ddip==.;
drop if roa==.;
replace eta=.5 if eta==0;
gen leta=log(eta);
gen lanno=log(anno);
keep ddip dip roa ldip roa1 duofut dofuy dofup dimm dimmy dimmp eta leta dupav* identif anno
razd razdy razdp dudex dudexy dudexp  ofut lanno anno;
sort identif anno;


for num 89/99: gen duX=(anno==19X);

pause;

sort identif anno;
tsset identif anno,yearly;
mata: mata set matafavor speed;


* financial expense;

xtabond2  ddip roa l.ldip l.ddip duofut dofuy dofup  leta du9* dupav1 dupav3
dupav4,
iv(du9*  leta) iv(l.ldip l.duofut l.dofuy l.dofup, eq(level))
gmm(l.ddip duofut dofuy dofup roa,collapse) gmm( dupav1 dupav3 dupav4, collapse
eq(diff)) robust twostep;


* tangible assets;

xtabond2 ddip roa l.ldip l.ddip dimm dimmy dimmp  leta du9* dupav1 dupav3 dupav4,
iv(du9* dupav1 dupav3 dupav4 leta) iv(l.ldip l.dimm l.dimmy l.dimmp, eq(level))
gmm(l.ddip dimm dimmy dimmp roa dupav1 dupav3 dupav4, collapse) robust twostep;

log close;
