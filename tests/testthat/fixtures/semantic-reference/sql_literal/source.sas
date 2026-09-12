proc sql; create table out as select ID from src where LABEL = 'A  B'; quit;
