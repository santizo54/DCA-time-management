ODS PDF File="C:\Users\Christian Santizo\Google Drive\CSULB\Courses\STAT 576\Discrete Choice Analysis\DCA_LL.pdf"; *this will make a pdf output file;
ODS Listing Close;


/* Create the a temporary libname that reads in the our excel file */
libname mylib excel "C:\Users\Christian Santizo\Google Drive\CSULB\Courses\STAT 576\Discrete Choice Analysis\Raw_Data_tabs.xls";

/* Macro that retrieves the sheets from the excel file */
%macro xlsimport(sheet=);
  proc import
    out = out&sheet.
    datafile = "C:\Users\Christian Santizo\Google Drive\CSULB\Courses\STAT 576\Discrete Choice Analysis\Raw_Data_tabs.xls"
    dbms = xls replace;
    sheet = "&sheet.";
	getnames=no;
  run;
  data out&sheet.;
	set out&sheet.;
	if cmiss(of _all_) then delete; * deletes all the empty rows ;
  run;
%mend xlsimport;

/* Concatenate sheets into one dataset */
proc sql;
  select cats('%xlsimport(sheet=',substr(memname,1,length(memname)-1),')')
    into :importlist separated by ' '
    from dictionary.tables
    where libname='MYLIB' and substr(memname,length(memname))='$'; * sheet contains a ($) at the end ;
quit;

libname mylib clear;

/* Call the dataset */
&importlist.;

/* Input survey data */
data LL_res;
	set Outchristian (firstobs=2) Outfrances (firstobs=2) Outtyler (firstobs=2);
	label	B = Age
			C = Gender
			D = Residence
			E = Ethnicity
			F = WHPW
			G = q1
			H = q2
			I = q3
			J = q4
			K = q5
			L = q6
			M = q7
			N = q8
			O = q9;
	rename 	B = Age
			C = Gender
			D = Residence
			E = Ethnicity
			F = WHPW
			G = q1
			H = q2
			I = q3
			J = q4
			K = q5
			L = q6
			M = q7
			N = q8
			O = q9;
	drop A;
	Subj=_n_;
run;

title "Frequency table of Demographics";
proc freq data = LL_res;
	tables Age Gender Residence Ethnicity WHPW;
run;

/* Inputting our LL design */
data LL_des;
input Set Ratio Leisure Labor;
cards;
1 1 2 1
1 3 4 2
2 3 2 3
2 1 1 2
3 2 1 3
3 1 3 2
4 3 4 1
4 2 1 2
5 1 2 2
5 2 3 1
6 3 1 1
6 2 3 1
7 3 3 2
7 1 4 3
8 2 4 2
8 3 1 3
9 2 2 1
9 1 3 3
;
run;

/* Assign the numeric values to the appropriate strings */
proc format;
	value Ratio	1 = '75% / 25%' 
				2 = '50% / 50%' 
				3 = '25% / 75%';
	value Leisure	1 = 'Social Media and TV' 
			 		2 = 'Gaming' 
					3 = 'Art'
					4 = 'Outdoor Activities';
	value Labor	1 = 'School'
				2 = 'School AND work'
				3 = 'Work';
run;
 

/* Overall */
%mktmerge(design=LL_des, data=LL_res,
out=LL_res2, nsets=9, nalts=2, setvars=q1-q9);

proc transreg design=5000 data=LL_res2 nozeroconstant norestoremissing;
	model class(Ratio Leisure Labor/ zero=none order=formatted) /lprefix=0;output
	out=LL_coded(drop=_type_ _name_ intercept);
	id subj gender set c;
run;

ods graphics off;
ods exclude all; 
proc phreg data=LL_coded brief;
	model c*c(2) = &_trgind / ties=breslow;
	strata subj set;
	ods output ParameterEstimates=outMLE_overall;
run;
ods exclude none;

title "MLE of Parameter Estimates: Overall";
proc print data=outMLE_overall;
	* where ProbChiSq ^=. & ProbChiSq < 0.05; * Modify to change significance level;
run;

/* Gender: Female */
proc sql;
	create table LL_female as
	select *
	from LL_res
	where Gender='Female';
quit;

%mktmerge(design=LL_des, data=LL_female,
out=LL_female2, nsets=9, nalts=2, setvars=q1-q9);

proc transreg design=5000 data=LL_female2 nozeroconstant norestoremissing;
	model class(Ratio Leisure Labor/ zero=none order=formatted) /lprefix=0;output
	out=LL_female_coded(drop=_type_ _name_ intercept);
	id subj gender set c;
run;

ods graphics off; 
ods exclude all; 
proc phreg data=LL_female_coded brief;
	model c*c(2) = &_trgind / ties=breslow;
	strata subj set;
	ods output ParameterEstimates=outMLE_female; 
run;
ods exclude none; 

title "MLE of Parameter Estimates: Female";
proc print data=outMLE_female;
	* where ProbChiSq ^=. & ProbChiSq < 0.05; * Modify to change significance level;
run;

/* Gender: Male */
proc sql;
	create table LL_male as
	select *
	from LL_res
	where Gender='Male';
quit;

%mktmerge(design=LL_des, data=LL_male,
out=LL_male2, nsets=9, nalts=2, setvars=q1-q9);

proc transreg design=5000 data=LL_male2 nozeroconstant norestoremissing;
	model class(Ratio Leisure Labor/ zero=none order=formatted) /lprefix=0;output
	out=LL_male_coded(drop=_type_ _name_ intercept);
	id subj gender set c;
run;

ods graphics off; 
ods exclude all;
proc phreg data=LL_male_coded brief;
	model c*c(2) = &_trgind / ties=breslow;
	strata subj set;
	ods output ParameterEstimates=outMLE_male; 
run;
ods exclude none; 

title "MLE of Parameter Estimates: Male";
proc print data=outMLE_male;
	* where ProbChiSq ^=. & ProbChiSq < 0.05; * Modify to change significance level;
run;

/* Residence: On-Campus */
proc sql;
	create table LL_onC as
	select *
	from LL_res
	where Residence='On-Campus';
quit;

%mktmerge(design=LL_des, data=LL_onC,
out=LL_onC2, nsets=9, nalts=2, setvars=q1-q9);

proc transreg design=5000 data=LL_onC2 nozeroconstant norestoremissing;
	model class(Ratio Leisure Labor/ zero=none order=formatted) /lprefix=0;output
	out=LL_onC_coded(drop=_type_ _name_ intercept);
	id subj gender set c;
run;

ods graphics off; 
ods exclude all;
proc phreg data=LL_onC_coded brief;
	model c*c(2) = &_trgind / ties=breslow;
	strata subj set;
	ods output ParameterEstimates=outMLE_onC;
run;
ods exclude none; 

title "MLE of Parameter Estimates: On-Campus";
proc print data=outMLE_onC;
	* where ProbChiSq ^=. & ProbChiSq < 0.05; * Modify to change significance level;
run;

/* Residence: Off-Campus, With Family */
proc sql;
	create table LL_offF as
	select *
	from LL_res
	where Residence='Off-Campus, With Family';
quit;

%mktmerge(design=LL_des, data=LL_offF,
out=LL_offF2, nsets=9, nalts=2, setvars=q1-q9);

proc transreg design=5000 data=LL_offF2 nozeroconstant norestoremissing;
	model class(Ratio Leisure Labor/ zero=none order=formatted) /lprefix=0;output
	out=LL_offF_coded(drop=_type_ _name_ intercept);
	id subj gender set c;
run;

ods graphics off; 
ods exclude all;
proc phreg data=LL_offF_coded brief;
	model c*c(2) = &_trgind / ties=breslow;
	strata subj set;
	ods output ParameterEstimates=outMLE_offF;
run;
ods exclude none;

title "MLE of Parameter Estimates: Off-Campus, With Family";
proc print data=outMLE_offF;
	* where ProbChiSq ^=. & ProbChiSq < 0.05; * Modify to change significance level;
run;

/* Residence: Off-Campus, Independently */
proc sql;
	create table LL_offI as
	select *
	from LL_res
	where Residence='Off-Campus, Independently';
quit;;

%mktmerge(design=LL_des, data=LL_offI,
out=LL_offI2, nsets=9, nalts=2, setvars=q1-q9);

proc transreg design=5000 data=LL_offI2 nozeroconstant norestoremissing;
	model class(Ratio Leisure Labor/ zero=none order=formatted) /lprefix=0;output
	out=LL_offI_coded(drop=_type_ _name_ intercept);
	id subj gender set c;
run;

ods graphics off; 
ods exclude all;
proc phreg data=LL_offI_coded brief;
	model c*c(2) = &_trgind / ties=breslow;
	strata subj set;
	ods output ParameterEstimates=outMLE_offF;
run;
ods exclude none;

title "MLE of Parameter Estimates: Off-Campus, Independently";
proc print data=outMLE_offF;
	* where ProbChiSq ^=. & ProbChiSq < 0.05; * Modify to change significance level;
run;



ODS Listing;
ODS PDF Close;
