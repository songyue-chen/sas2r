/* Run in a fresh SAS batch session from the repository root.
   tools/run-semantic-references.R sets SAS2R_REFERENCE_OUT and saves sas.log.
   For a manual SAS session, create the output directory first and preserve
   the complete SAS log there as sas.log. No expected CSV is read by SAS. */
%let corpus=tests/testthat/fixtures/semantic-reference;
%let reference_out=%sysget(SAS2R_REFERENCE_OUT);
%macro default_reference_out;
  %if %length(%superq(reference_out))=0 %then
    %let reference_out=&corpus/sas-generated;
%mend;
%default_reference_out;

%macro semantic_case(id);
  /* A failed case must never export an earlier case's WORK.OUT. */
  %if %sysfunc(exist(work.out)) %then %do;
    proc datasets library=work nolist; delete out; quit;
  %end;
  %include "&corpus/&id/setup.sas";
  %include "&corpus/&id/source.sas";
  %if &syscc > 4 or not %sysfunc(exist(work.out)) %then %do;
    %put ERROR: semantic fixture &id failed before export.;
    %abort abend;
  %end;
  proc export data=work.out outfile="&reference_out/&id..csv"
    dbms=csv replace;
  run;
  %if &syscc > 4 %then %abort abend;
%mend;

%semantic_case(missing_assignment);
%semantic_case(computed_missing);
%semantic_case(where_timing);
%semantic_case(literal_text);
%semantic_case(sql_case);
%semantic_case(sql_group_case);
%semantic_case(sql_literal);
%semantic_case(merge_disjoint);
%semantic_case(merge_shared);
%semantic_case(merge_body);
%semantic_case(sql_delete);
%semantic_case(sql_update);
%semantic_case(multiple_where);
%semantic_case(comparison_values);

data _null_;
  file "&reference_out/provenance.txt";
  put "SAS &sysvlong on &sysscp; &sysdate9 &systime";
run;
