test_that("aggregate fileref includes resolve through filename statements", {
  dir <- withr::local_tempdir()
  mac <- file.path(dir, "macs"); dir.create(mac)
  writeLines("%macro alloc; %mend;", file.path(mac, "alloc.sas"))
  writeLines(sprintf("filename mymacs '%s';\n%%include mymacs(alloc);\n%%alloc()", mac),
             file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "alloc"], "resolved_project")
  expect_false("unresolved_include" %in% p$flags$kind)
})

test_that("unregistered filerefs still flag unresolved_include", {
  dir <- withr::local_tempdir()
  writeLines("%include ghostref(x);", file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_true("unresolved_include" %in% p$flags$kind)
})

test_that("single fileref includes resolve through filename statements", {
  dir <- withr::local_tempdir()
  writeLines("%macro singlemac; %mend;", file.path(dir, "helper.sas"))
  writeLines(sprintf("filename myref '%s';\n%%include myref;\n%%singlemac()", file.path(dir, "helper.sas")),
             file.path(dir, "main.sas"))
  p <- sas_project(file.path(dir, "main.sas"))
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "singlemac"], "resolved_project")
  expect_false("unresolved_include" %in% p$flags$kind)
})

test_that("filerefs from autoexec environment resolve in program includes", {
  dir <- withr::local_tempdir()
  mac <- file.path(dir, "macs"); dir.create(mac)
  writeLines("%macro envmac; %mend;", file.path(mac, "envmac.sas"))
  writeLines(sprintf("filename envref '%s';", mac), file.path(dir, "autoexec.sas"))
  writeLines("%include envref(envmac);\n%envmac()", file.path(dir, "driver.sas"))
  p <- sas_project(dir)
  res <- p$macros$resolution
  expect_identical(res$status[res$name == "envmac"], "resolved_project")
  expect_false("unresolved_include" %in% p$flags$kind)
})

test_that("filerefs pointing to nonexistent files flag unresolved_include", {
  dir <- withr::local_tempdir()
  writeLines("filename badref '/nonexistent/path';\n%include badref(missing);",
             file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  expect_true("unresolved_include" %in% p$flags$kind)
})

test_that("fileref includes are recorded as fileref-origin occurrences", {
  dir <- withr::local_tempdir()
  mac <- file.path(dir, "macs"); dir.create(mac)
  writeLines("%macro alloc; %mend;", file.path(mac, "alloc.sas"))
  writeLines(sprintf("filename mymacs '%s';\n%%include mymacs(alloc);", mac),
             file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  occ <- p$include_graph$occurrences

  expect_identical(nrow(occ), 1L)
  expect_identical(occ$status, "resolved")
  expect_identical(occ$resolution_origin, "fileref")
  expect_identical(occ$target_expression, "mymacs(alloc)")
  expect_identical(normalizePath(occ$target_file),
                   normalizePath(file.path(mac, "alloc.sas")))
})

test_that("filerefs bound to relative directories resolve source-first", {
  dir <- withr::local_tempdir()
  mac <- file.path(dir, "macs"); dir.create(mac)
  writeLines("%macro relmac; %mend;", file.path(mac, "relmac.sas"))
  writeLines("filename relref 'macs';\n%include relref(relmac);\n%relmac()",
             file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  occ <- p$include_graph$occurrences

  expect_identical(occ$resolution_origin, "fileref")
  expect_identical(normalizePath(occ$target_file),
                   normalizePath(file.path(mac, "relmac.sas")))
  expect_false("unresolved_include" %in% p$flags$kind)
})

test_that("unregistered filerefs produce unresolved occurrences with no origin", {
  dir <- withr::local_tempdir()
  writeLines("%include ghostref(x);", file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  occ <- p$include_graph$occurrences

  expect_identical(occ$status, "unresolved")
  expect_identical(occ$resolution_origin, "none")
  expect_identical(occ$reason, "target_not_found")
  expect_true(is.na(occ$target_file))
})

test_that("quoted include targets never consult filerefs", {
  dir <- withr::local_tempdir()
  decoy_dir <- withr::local_tempdir()
  writeLines("%macro decoy; %mend;", file.path(decoy_dir, "decoy.sas"))
  writeLines("%macro real; %mend;", file.path(dir, "setup.sas"))
  writeLines(sprintf("filename setup '%s';\n%%include 'setup.sas';",
                     file.path(decoy_dir, "decoy.sas")),
             file.path(dir, "a.sas"))
  p <- sas_project(file.path(dir, "a.sas"))
  occ <- p$include_graph$occurrences

  expect_identical(occ$resolution_origin, "including_dir")
  expect_identical(normalizePath(occ$target_file),
                   normalizePath(file.path(dir, "setup.sas")))
})
