proc sql; create table out as select GROUP, sum(VALUE) as TOTAL from src group by GROUP order by GROUP; quit;
