proc sort data=work.adsl_srt out=work.adsl_bysex; by sex usubjid; run;

data work.elderly;
  set work.adsl_bysex;
  where saffl = 'Y';
  if age ge 65 then eldfl = 'Y';
  keep usubjid sex eldfl;
run;
