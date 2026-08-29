%macro derive_flag(ds=, var=);
  data &ds; set &ds; &var = 'Y'; run;
%mend derive_flag;
