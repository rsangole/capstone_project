
/*
data SeqTabLookups;
	set
		SAS09.SeqTabLookup (in=in09)
		SAS10.SeqTabLookup (in=in10)
		SAS11.SeqTabLookup (in=in11)
		SAS12.SeqTabLookup (in=in12)
		SAS13.SeqTabLookup (in=in13)
		SAS14.SeqTabLookup (in=in14)
		SAS15.SeqTabLookup (in=in15)
		SAS16.SeqTabLookup (in=in16)
		SAS17.SeqTabLookup (in=in17)
	;
	if in09 then yr=2009;
	else if in10 then yr=2010;
	else if in11 then yr=2011;
	else if in12 then yr=2012;
	else if in13 then yr=2013;
	else if in14 then yr=2014;
	else if in15 then yr=2015;
	else if in16 then yr=2016;
	else if in17 then yr=2017;

	my_seqn = _N_;

	if substr(tblid,1,6) in ('B00001','B01001') then output;
run;

proc sort data = SeqTabLookups;
	by TblId order yr my_seqn;
run;

*/

/*

data chk;
	set Sas17.SeqTabLookup;
	where lowcase(title) like '%race%' or lowcase(subject_area) like '%race%';
run;
data chk2;
	set Sas17.SeqTabLookup;
	where TblId in ('B02001','B02008','B02009');
run;
data chk3;
	set Sas17.SeqTabLookup;
	where lowcase(title) like '%ethnic%' or lowcase(subject_area) like '%ethnic%';
run;
data chk3;
	set Sas17.SeqTabLookup;
	where lowcase(title) like '%hispan%' or lowcase(subject_area) like '%hispan%';
run;

*/



%Macro ReadInMoreData(yr,acstype=5,TblIds=%str('B15003','B19013','C17002'));

%let datayear = &yr.;
%let yr2 = %sysfunc(substr(&yr.,3,2));

/*libname bg&yr. "D:\temp\Census ACS\Illinois &acstype.-year ACS &datayear.\SAS block group\";*/

LIBNAME tract&yr2. "D:\temp\Census ACS\Illinois 5-year ACS 20&yr2.\SAS census tract";
LIBNAME bg&yr2. "D:\temp\Census ACS\Illinois 5-year ACS 20&yr2.\SAS block group";
LIBNAME sas&yr2. "D:\temp\Census ACS\Illinois 5-year ACS 20&yr2.\SAS data";


data Tab&yr.;
	set sas&yr2..SeqTabLookup;
/*	where TblId in ('B15003','B19013','C17002');*/
	where TblId in (&TblIds.);
run;

proc sql;
	create table Tab&yr.b as (
		select distinct TblId,Seq 
			,"bg&yr2..sfe" || trim(left(Seq)) || "il" as bg
			,"tract&yr2..sfe" || trim(left(Seq)) || "il" as tract
		from Tab&yr.
	);
quit;
	
data _NULL_;
	set Tab&yr.b end=end nobs=nobs;
	call symput("bg&yr2._" || trim(left(put(_N_,8.0))),trim(left(bg)));
	call symput("tract&yr2._" || trim(left(put(_N_,8.0))),trim(left(tract)));
	if end then call symput("numSeq",trim(left(put(_N_,8.0))));
run;

/*%put &numSeq.;*/

%do i = 1 %to &numSeq.;
/*%put &&&bg&yr2._&i.;*/
proc sort data = &&&bg&yr2._&i.; by GEOID; run;
%end;

data work.bg_&yr.;
	merge 
%do i = 1 %to &numSeq.;
&&&bg&yr2._&i.
%end;
	;
	by GEOID; 
/*	%CodeAnalyticVars*/
	year = &yr.;
	if state="17" and county = "031" then output;
run;


%do i = 1 %to &numSeq.;
/*%put &&&bg&yr2._&i.;*/
proc sort data = &&&tract&yr2._&i.; by GEOID; run;
%end;

data work.tract_&yr.;
	merge 
%do i = 1 %to &numSeq.;
&&&tract&yr2._&i. 
%end;
	;
	by GEOID; 
/*	%CodeAnalyticVars*/
	year = &yr.;
	if state="17" and county = "031" then output;
run;

%MEnd ReadInMoreData;


/*%let mytbls = %str('B00001','B01001','B01001A','B01001B','B01001C','B01001D','B01001E','B01001F','B01001G','B01001H','B01001I','B01002');*/
%let mytbls = %str('B00001','B01001','B01002','B02001','B03001','B03002');

options nosource nonotes errors=0;

%ReadInMoreData(2017,TblIds = &mytbls.)

%ReadInMoreData(2016,TblIds = &mytbls.)
%ReadInMoreData(2015,TblIds = &mytbls.)

%ReadInMoreData(2014,TblIds = &mytbls.)
%ReadInMoreData(2013,TblIds = &mytbls.)

%ReadInMoreData(2012,TblIds = &mytbls.)
%ReadInMoreData(2011,TblIds = &mytbls.)
%ReadInMoreData(2010,TblIds = &mytbls.)
%ReadInMoreData(2009,TblIds = &mytbls.)

options source notes errors=1;





%Macro ReLabelDemographics();
	pop_units = B00001e1;
	housing_units = B00002e1;
	tot_pop = B01001e1;

	male_pop = B01001e2;
	female_pop = B01001e26;
	if not(missing(B01001e1)) and B01001e1 > 0 then male_pct = B01001e2 / B01001e1; else male_pct = 0;
	if not(missing(B01001e1)) and B01001e1 > 0 then female_pct = B01001e26 / B01001e1; else female_pct = 0;
	sex_pct_check = male_pct + female_pct;

	age_median = B01002e1;

	male_under18 = B01001e3 + B01001e4 + B01001e5 + B01001e6;
	male_18to64 = B01001e7 + B01001e8 + B01001e9 + B01001e10 + B01001e11 + B01001e12 + B01001e13
		+ B01001e14 + B01001e15 + B01001e16 + B01001e17 + B01001e18 + B01001e19 ;
	male_65plus = B01001e20 + B01001e21 + B01001e22 + B01001e23 + B01001e24 + B01001e25 ;

	female_under18 = B01001e27 +  B01001e28 + B01001e29 + B01001e30;
	female_18to64 = B01001e31 + B01001e32 + B01001e33 + B01001e34 + B01001e35 + B01001e36 + B01001e37
		+ B01001e38 + B01001e39 + B01001e40 + B01001e41 + B01001e42 + B01001e43 ;
	female_65plus = B01001e44 + B01001e45 + B01001e46 + B01001e47 + B01001e48 + B01001e49;

	if not(missing(B01001e1)) and B01001e1 > 0 then tot_under18 = male_under18 + female_under18; else tot_under18 = 0;
	if not(missing(B01001e1)) and B01001e1 > 0 then tot_18to64 = male_18to64 + female_18to64; else tot_18to64 = 0;
	if not(missing(B01001e1)) and B01001e1 > 0 then tot_65plus = male_65plus + female_65plus; else tot_65plus = 0;

	if not(missing(B01001e1)) and B01001e1 > 0 then tot_under18_pct = tot_under18 / B01001e1; else tot_under18_pct = 0;
	if not(missing(B01001e1)) and B01001e1 > 0 then tot_18to64_pct = tot_18to64 / B01001e1; else tot_18to64_pct = 0;
	if not(missing(B01001e1)) and B01001e1 > 0 then tot_65plus_pct = tot_65plus / B01001e1; else tot_65plus_pct = 0;
	age_pct_chk = tot_under18_pct + tot_18to64_pct + tot_65plus_pct;

	race_white = B02001e2;
	race_aa = B02001e3;
	race_aian = B02001e4;
	race_asian = B02001e5;
	race_hpi = B02001e6;
	race_other = B02001e7;
	race_mult = B02001e8;

	if not(missing(B02001e1)) and B02001e1 > 0 then race_aa_pct = race_aa / B02001e1; else race_aa_pct = 0;
	if not(missing(B02001e1)) and B02001e1 > 0 then race_aian_pct = race_aian / B02001e1; else race_aian_pct = 0;
	if not(missing(B02001e1)) and B02001e1 > 0 then race_asian_pct = race_asian / B02001e1; else race_asian_pct = 0;
	if not(missing(B02001e1)) and B02001e1 > 0 then race_hpi_pct = race_hpi / B02001e1; else race_hpi_pct = 0;
	if not(missing(B02001e1)) and B02001e1 > 0 then race_other_pct = race_other / B02001e1; else race_other_pct = 0;
	if not(missing(B02001e1)) and B02001e1 > 0 then race_mult_pct = race_mult / B02001e1; else race_mult_pct = 0;
	if not(missing(B02001e1)) and B02001e1 > 0 then race_white_pct = race_white / B02001e1; else race_white_pct = 0;

	race_chk = race_aa + race_aian + race_asian + race_hpi + race_other + race_mult + race_white;
	race_pct_chk = race_aa_pct + race_aian_pct + race_asian_pct + race_hpi_pct + race_other_pct + race_mult_pct + race_white_pct;

	eth_latino = B03003e3;
	eth_notlatino = B03003e2;
	if not(missing(B03003e1)) and B03003e1 > 0 then eth_latino_pct = B03003e3 / B03003e1; else eth_latino_pct = 0;
	if not(missing(B03003e1)) and B03003e1 > 0 then eth_notlatino_pct = B03003e2 / B03003e1; else eth_notlatino_pct = 0;
	eth_pct_chk = eth_latino_pct + eth_notlatino_pct;

	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_white_pct = B03002e3 / B03002e1; else raceeth_white_pct = 0;
	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_aa_pct = B03002e4 / B03002e1; else raceeth_aa_pct = 0;
	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_aian_pct = B03002e5 / B03002e1; else raceeth_aian_pct = 0;
	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_asian_pct = B03002e6 / B03002e1; else raceeth_asian_pct = 0;
	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_hpi_pct = B03002e7 / B03002e1; else raceeth_hpi_pct = 0;
	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_other_pct = B03002e8 / B03002e1; else raceeth_other_pct = 0;
	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_mult_pct = B03002e9 / B03002e1; else raceeth_mult_pct = 0;
	if not(missing(B03002e1)) and B03002e1 > 0 then raceeth_hispanic_pct = B03002e12 / B03002e1; else raceeth_hispanic_pct = 0;

	raceeth_pct_chk = 
		raceeth_white_pct 
		+ raceeth_aa_pct 
		+ raceeth_aian_pct 
		+ raceeth_asian_pct 
		+ raceeth_hpi_pct 
		+ raceeth_other_pct 
		+ raceeth_mult_pct 
		+ raceeth_hispanic_pct 
	;


%MEnd ReLabelDemographics;

data blkgrp;
	set 
		bg_2017
		bg_2016
		bg_2015
		bg_2014
		bg_2013
		bg_2012
		bg_2011
		bg_2010
		bg_2009
	;

	%ReLabelDemographics

/*	year = fileid;*/
	year=put(substr(FILETYPE,1,4),8.0);

	keep
		sumlevel 
/*		logrecno state county tract blkgrp geoid name fileid filetype stusab */
		filetype
		year
		geoid
/*		pop_units housing_units */
		tot_pop 
		male_pct female_pct
		age_median 
		tot_under18_pct tot_18to64_pct tot_65plus_pct
/*		race_white_pct race_aian_pct race_asian_pct race_hpi_pct race_other_pct */
/*		race_mult_pct race_aa_pct*/
/*		eth_latino_pct eth_notlatino_pct*/

/*		age_pct_chk */
/*		race_pct_chk*/
/*		eth_pct_chk*/
/*		raceeth_pct_chk*/

		raceeth_white_pct 
		raceeth_aa_pct 
		raceeth_aian_pct 
		raceeth_asian_pct 
		raceeth_hpi_pct 
		raceeth_other_pct 
		raceeth_mult_pct 
		raceeth_hispanic_pct 
	;

run;

data tract;
	set 
		tract_2017
		tract_2016
		tract_2015
		tract_2014
		tract_2013
		tract_2012
		tract_2011
		tract_2010
		tract_2009
	;

	%ReLabelDemographics

/*	year = fileid;*/
	year=put(substr(FILETYPE,1,4),8.0);

	keep
		sumlevel 
/*		logrecno state county tract blkgrp geoid name fileid filetype stusab */
		filetype
		year
		geoid
/*		pop_units housing_units */
		tot_pop 
		male_pct female_pct
		age_median 
		tot_under18_pct tot_18to64_pct tot_65plus_pct
/*		race_white_pct race_aian_pct race_asian_pct race_hpi_pct race_other_pct */
/*		race_mult_pct race_aa_pct*/
/*		eth_latino_pct eth_notlatino_pct*/

/*		age_pct_chk */
/*		race_pct_chk*/
/*		eth_pct_chk*/
/*		raceeth_pct_chk*/

		raceeth_white_pct 
		raceeth_aa_pct 
		raceeth_aian_pct 
		raceeth_asian_pct 
		raceeth_hpi_pct 
		raceeth_other_pct 
		raceeth_mult_pct 
		raceeth_hispanic_pct 
	;


run;




libname acs "D:\ajc188\github\capstone_project\data\raw\census_acs";


data acs.Demographics_BlkGrp;
	set BlkGrp;
run;


data acs.Demographics_tract;
	set tract;
run;


proc means data = acs.Demographics_BlkGrp n nmiss mean stddev min q1 median q3 qrange max maxdec=3;
	var _NUMERIC_;
run;

proc means data = acs.Demographics_tract n nmiss mean stddev min q1 median q3 qrange max maxdec=3;
	var _NUMERIC_;
run;


proc export data=acs.Demographics_blkgrp  outfile="D:\ajc188\github\capstone_project\data\raw\census_acs\Demographics_BlkGrp.csv" dbms=csv ;
run;
proc export data=acs.Demographics_tract  outfile="D:\ajc188\github\capstone_project\data\raw\census_acs\Demographics_tract.csv" dbms=csv ;
run;





