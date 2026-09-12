data out; merge a(in=ina) b(in=inb); by id; if ina; z = x + y; if x < 0 then delete; keep id z; run;
