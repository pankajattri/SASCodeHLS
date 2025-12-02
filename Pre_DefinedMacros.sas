*************************************************************************************
***
*** Program:       MFREQ.sas
*** Programmer:    Matt Becker
*** Date Created:  02Feb2010
***
*** Purpose:       Macro to create frequency counts and percentages
***
*** Comments:
***
*** Macro Parameters    : INSET    - Input dataset
***                      OUTSET   - Ouptut dataset
***                      CNTVAR   - Variable to be counted
***                      MFFMT    - Format to be applied to CNTVAR in output dataset
***                      WHFREQ   - Subset to be applied
***                      BYFREQ   - BY variable
***                      DEN      - Macro variable of denominator (default=pop)
***                      MFORD    - Order in output dataset
***                      MFPERC   - Y/N on whether percentages are required
***                      MFMISS   - The value of missings, to be omitted from the denominator
***                                 when MFPERC=Y and DEN=missing
***                      MFSBS    - Y/N on whether the count and denominator should be
***                                 side by side (e.g. xxx/yyy (zz%) );
***                      MFCI95   - Y/N on whether confidence intervals are required
***                      MFCI95L  - The value that the CI95 is based on
***                      MFDRNG   - To force in missing values of CNTVAR, specify range of values
***                                 in format %str(X to Y) or %str(X, Y, Z)
***                      MTOTTRT  - Defaults to global macro variable MTOTTRT
***                                 Can be set to OVTRT to allow overall totals
***
*** Modification History:
***
*** Date        Programmer              Description
*** ----------  ----------------------  ---------------------
***
*************************************************************************************;
%macro mfreq (mfdata=,mfout=,mftrt=atrt,mfcntvar=,mffmt=,mfwhere=,mforder=1,mfden=POP,mfby=,mfperc=Y,
			mfmiss=,mfdrng=,mtottrt=&tottrt,mfspace=&trtspace);

  data _null_;
    length table $100;
    if "&mftrt" ne " " then table="&mftrt";
	else table=" ";
	if "&mfby" ne " " then table=trim(table) || ' ' || trim("&mfby");
	call symput('mfoby',trim(table));
	call symput('mfobyl',reverse(scan(reverse(table),1,' ')));
	table=trim(table) || ' ' || trim("&mfcntvar");
	call symput('mftable',translate(trim(left(table)),'*',' '));
  run;

proc freq data=&mfdata noprint;
  %IF &MFWHERE NE %THEN %DO;
    where &MFWHERE;
  %END;
  tables &mftable / out=mf2;
run;

%IF &MFDRNG NE %THEN %DO;
  data mfall0;
  set mf2 (drop=&mfcntvar);
  by &mfoby;
    if last.&mfobyl then do &mfcntvar=&mfdrng;
	  count=0;
	  output;
	end;
  run;

  data mf3;
  merge mfall0
        mf2;
  by &mfoby &mfcntvar;
  run;
%END;

%ELSE %DO;
  data mf3;
  set mf2;
  run;
%END;

%IF &MFMISS NE %THEN %DO;
  data mf4;
  set mf3;
  by &mfoby &mfcntvar;
    retain denom;
    if first.&mfobyl then denom=0;
    if &mfcntvar not in(&mfmiss) then denom=denom+count;
    if last.&mfobyl;
  run;
%END;

data mf5;
merge mf3
  %IF &MFMISS NE %THEN %DO;
    mf4 (keep=&mfoby denom)
  %END;;
by &mfoby;
  %IF &MFDEN ne %THEN %DO I=1 %TO &MTOTTRT;
    if &MFTRT=&I then denom=&&&mfden&i;
  %END;
  %IF &MFPERC=Y %THEN %DO;
    perc=compress('(' || put((count/denom*100),perc.) ||'%)');
  %END;
  length disp $50;
  if count >= 0 
  %IF &MFMISS NE %THEN %DO;
    and &MFCNTVAR not in(&mfmiss)
  %END;  
  then disp=put(count,&&mfspace...)
  %IF &MFPERC=Y %THEN %DO;
    || ' ' || right(perc)
  %END;;
  else disp=put(count,&&mfspace...);
run;

proc sort data=mf5;
  by &mfby &mfcntvar &mftrt;
run;

proc transpose data=mf5 out=mf6 
  %IF &MFTRT NE %THEN %DO;
    prefix=trt;
    id &mftrt
  %END;;
  by &mfby &mfcntvar;
  var disp;
run;

data &mfout;
%IF &MFTRT NE %THEN %DO;
length %DO I=1 %TO &MTOTTRT;
         trt&i 
	   %END; $50;
%END;
set mf6;
  order=&mforder;
  %IF &MFCNTVAR NE %THEN %DO;
  sorder=&mfcntvar;
  %END;
  %ELSE %DO;
   sorder=1;
  %END;
  length text $200;
  text='';
  %IF &MFFMT NE %THEN %DO;
    text=put(sorder,&MFFMT..);
  %END;
  %IF &MFTRT NE %THEN %DO I=1 %TO &MTOTTRT;
    %IF &MFPERC=Y %THEN %DO;
      perc=compress('(' || put((0/1*100),perc.) ||'%)');
    %END;
    if trt&i='' then trt&i=put(0,&&mfspace...) 
    %if &MFPERC=Y %then %do;
       || ' ' || right(perc)
    %end;
    ;
	/*
	%IF %EVAL(&MFSPACE) GT 4 %THEN %DO;
	  trt&i=repeat(' ',&mfspace-5) || trt&i;
	%END;
	*/

  %END;
  %IF &MFMISS NE %THEN %DO;
    if &mfcntvar=&mfmiss and compress(trt1)='0'
    %DO I=2 %TO &MTOTTRT;
      and compress(trt&i)='0'
    %END;
	then delete;
  %END;  
run;

%mend mfreq;



**************************************************************************************;
** macro mstudydy - calculates study day
** should be called from within a data step
** parameters:
**    todate (required)   to date variable name
**    basedate (required) baseline date variable name
**    studyday (optional) output studyday var name
**                        (defaults to 'studyday')
** mjb 30jan2010
** mods:
**
**************************************************************************************;
%macro mstudydy (todate=, basedate=, studyday=studyday);

 %if &todate= |&basedate= %then %do;
  put 'missing parameters - aborting...'
 %end;

 %else %do;
  &studyday=&todate-&basedate+(&todate ge &basedate);
 %end;

%mend mstudydy;

