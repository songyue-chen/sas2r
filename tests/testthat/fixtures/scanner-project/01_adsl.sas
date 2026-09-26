libname adam 'data/adam';

data work.adsl_prep;
  set adam.adsl;
  %derive_flag(ds=work.adsl_prep, var=saffl)
run;

proc sort data=work.adsl_prep out=work.adsl_srt;
  by usubjid;
run;
