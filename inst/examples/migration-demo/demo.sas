%let plotds = input_ds;

data work.stg1;
  set &plotds;
  if missing(AVAL) then AVAL_FLAG = 'MISSING';
  else AVAL_FLAG = 'RECORDED';
run;

proc sort data=work.stg1 out=work.stg_sort;
  by TRTP AVISITN;
run;

data adam.final_ds;
  set work.stg_sort;
  if AVAL_FLAG = 'RECORDED' and AVAL > 10 then HIGH_FLAG = 1;
  else HIGH_FLAG = 0;
run;

ods pdf file="outputs/figure1.pdf";
proc print data=adam.final_ds;
run;
ods pdf close;
