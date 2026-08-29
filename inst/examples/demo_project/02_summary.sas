proc sql;
  create table work.summary as
  select trt01p, count(*) as n
  from work.adsl_srt
  group by trt01p;
quit;

%ghost_macro(x=1)
