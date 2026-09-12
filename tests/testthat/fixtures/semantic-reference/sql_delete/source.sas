proc sql; create table out as select * from src; delete from out where x < 0; quit;
