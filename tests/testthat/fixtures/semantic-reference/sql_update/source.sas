proc sql; create table out as select * from src; update out set x = 9; quit;
