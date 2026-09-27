diagnosis_fixture <- function(envir = parent.frame(), code = "data out; set work.absent; run;") {
  root <- withr::local_tempdir(.local_envir = envir)
  writeLines(code, file.path(root, "main.sas"))
  root
}

diagnosis_answer <- function(classification = "missing_resource") list(
  summary = "Check the input producer.", findings = list(list(
    classification = classification, explanation = "The read has no known producer.",
    evidence = "main.sas:1 reads work.absent", suggestion = "Supply the intended input step.",
    uncertainty = "Macro effects may be unknown.")),
  issue_title = if (classification == "suspected_sas2r_bug") "Producer lookup needs investigation" else "",
  issue_body = if (classification == "suspected_sas2r_bug") "Reproduce with the reported source and order." else "")

diagnosis_mock <- function(answer = diagnosis_answer(), callback = function(request) NULL) {
  new_llm(function(request, audit_context = list()) {
    callback(request)
    new_llm_response(status = "completed", action = "final", data = answer,
      request = request, provider = "mock", resolved_model = "diagnosis-test")
  }, provider = "mock", model = "diagnosis-test")
}
