/*******************************************************************************
Paths
*******************************************************************************/

%let datayear = 2017;
%let acstype = 5;

%let BasePath = D:\temp\Census ACS\Illinois &acstype.-year ACS &datayear.;
%let SasPath = &BasePath.\SAS code (generated);
/*%let InputPath = &BasePath.\Illinois_Tracts_Block_Groups_Only;*/
%let InputPath = &BasePath.\Tracts_Block_Groups_Only;
/*%let GeoPath = &BasePath.\Illinois_Tracts_Block_Groups_Only;*/
%let GeoPath = &BasePath.\2017_ACS_Geography_Files;


/*******************************************************************************
Paths
*******************************************************************************/




/*******************************************************************************
Use ACS items to calculate summary variables
*******************************************************************************/

%Macro CodeAnalyticVars(FileType=E);

/*
EDUCATIONAL ATTAINMENT
B15003e1="Total:"
B15003e2="No schooling completed"
B15003e3="Nursery school"
B15003e4="Kindergarten"
B15003e5="1st grade"
B15003e6="2nd grade"
B15003e7="3rd grade"
B15003e8="4th grade"
B15003e9="5th grade"
B15003e10="6th grade"
B15003e11="7th grade"
B15003e12="8th grade"
B15003e13="9th grade"
B15003e14="10th grade"
B15003e15="11th grade"
B15003e16="12th grade, no diploma"
B15003e17="Regular high school diploma"
B15003e18="GED or alternative credential"
B15003e19="Some college, less than 1 year"
B15003e20="Some college, 1 or more years, no degree"
B15003e21="Associate""s degree"
B15003e22="Bachelor""s degree"
B15003e23="Master""s degree"
B15003e24="Professional school degree"
B15003e25="Doctorate degree"

These are overall #s 25+.  There are also race-specific tallies with less granularity.  
*/

	LT_HS_cnt = Sum(B15003&FileType.2,B15003&FileType.3,B15003&FileType.4,B15003&FileType.5,B15003&FileType.6,B15003&FileType.7,B15003&FileType.8,B15003&FileType.9,B15003&FileType.10
		,B15003&FileType.11,B15003&FileType.12,B15003&FileType.13,B15003&FileType.14,B15003&FileType.15,B15003&FileType.16);
	GE_HS_cnt = Sum(B15003&FileType.17,B15003&FileType.18,B15003&FileType.19,B15003&FileType.20,B15003&FileType.21,B15003&FileType.22,B15003&FileType.23,B15003&FileType.24,B15003&FileType.25);
	Educ_cnt = Sum(LT_HS_cnt,GE_HS_cnt);
	if Educ_cnt > 0 then LT_HS_pct = 100 * LT_HS_cnt / Educ_cnt;
	if Educ_cnt > 0 then GE_HS_pct = 100 * GE_HS_cnt / Educ_cnt;

	if not(missing(B19013&FileType.1)) then median_HHInc = B19013&FileType.1;
 
/*C17002e1="Total:"*/
/*C17002e2="Under .50"*/
/*C17002e3=".50 to .99"*/
/*C17002e4="1.00 to 1.24"*/
/*C17002e5="1.25 to 1.49"*/
/*C17002e6="1.50 to 1.84"*/
/*C17002e7="1.85 to 1.99"*/
/*C17002e8="2.00 and over"*/
/*;*/

	LT_Pov_cnt = Sum(C17002&FileType.2,C17002&FileType.3);
	GE_Pov_cnt = Sum(C17002&FileType.4,C17002&FileType.5,C17002&FileType.6,C17002&FileType.7,C17002&FileType.8);
	Pov_cnt = Sum(LT_Pov_cnt,GE_Pov_cnt);
	if Pov_cnt > 0 then LT_Pov_pct = 100 * LT_Pov_cnt / Pov_cnt;
	if Pov_cnt > 0 then GE_Pov_pct = 100 * GE_Pov_cnt / Pov_cnt;
%Mend CodeAnalyticVars;




/*******************************************************************************
Further combine sequences (block group)
Crap.  
I need to break sequence files down to table files first.... 
*******************************************************************************/



%Macro ReadInData(yr,acstype=5);

%let datayear = &yr.;
%let yr2 = %sysfunc(substr(&yr.,3,2));

/*libname bg&yr. "D:\temp\Census ACS\Illinois &acstype.-year ACS &datayear.\SAS block group\";*/

LIBNAME tract&yr2. "D:\temp\Census ACS\Illinois 5-year ACS 20&yr2.\SAS census tract";
LIBNAME bg&yr2. "D:\temp\Census ACS\Illinois 5-year ACS 20&yr2.\SAS block group";


proc sort data = bg&yr2..sfe0059il; by GEOID; run;
proc sort data = bg&yr2..sfe0063il; by GEOID; run;
proc sort data = bg&yr2..sfe0050il; by GEOID; run;
proc sort data = bg&yr2..sfe0043il; by GEOID; run;
proc sort data = bg&yr2..sfe0121il; by GEOID; run;

data bg&yr2..sfeMULTil;
	merge bg&yr2..sfe0059il bg&yr2..sfe0063il bg&yr2..sfe0050il
				bg&yr2..sfe0043il bg&yr2..sfe0121il 
	;
	by GEOID; 
	%CodeAnalyticVars
run;


proc sort data = tract&yr2..sfe0059il; by GEOID; run;
proc sort data = tract&yr2..sfe0063il; by GEOID; run;
proc sort data = tract&yr2..sfe0050il; by GEOID; run;
proc sort data = tract&yr2..sfe0043il; by GEOID; run;
proc sort data = tract&yr2..sfe0121il; by GEOID; run;

data tract&yr2..sfeMULTil;
	merge tract&yr2..sfe0059il tract&yr2..sfe0063il tract&yr2..sfe0050il
				tract&yr2..sfe0043il tract&yr2..sfe0121il 
	;
	by GEOID; 
	%CodeAnalyticVars
run;


data work.bg_&yr.;
	set bg&yr2..sfeMULTil;
	year = &yr.;
	if state="17" and county = "031" then output;
run;
data work.tract_&yr.;
	set tract&yr2..sfeMULTil;
	year = &yr.;
	if state="17" and county = "031" then output;
run;


%MEnd ReadInData;


%ReadInData(2017)
%ReadInData(2016)
%ReadInData(2015)

%ReadInData(2014)
%ReadInData(2013)

%ReadInData(2012)
%ReadInData(2011)
%ReadInData(2010)
%ReadInData(2009)


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
run;




/*D:\temp\Census ACS\Illinois 5-year ACS 2016\SAS block group*/

