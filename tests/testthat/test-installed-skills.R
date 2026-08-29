test_that("installed package skills exist in package system files", {
  skill1 <- system.file("skills", "sas-missing-sort-semantics", "SKILL.md", package = "sas2r")
  skill2 <- system.file("skills", "sas-dataset-row-alignment", "SKILL.md", package = "sas2r")

  # When run via pkgload during dev, inst/ is mapped directly
  if (!nzchar(skill1)) {
    skill1 <- system.file("inst", "skills", "sas-missing-sort-semantics", "SKILL.md", package = "sas2r")
  }
  if (!nzchar(skill2)) {
    skill2 <- system.file("inst", "skills", "sas-dataset-row-alignment", "SKILL.md", package = "sas2r")
  }

  expect_true(nzchar(skill1) || file.exists("inst/skills/sas-missing-sort-semantics/SKILL.md"))
  expect_true(nzchar(skill2) || file.exists("inst/skills/sas-dataset-row-alignment/SKILL.md"))

  catalog <- sas2r:::agent_skill_catalog()
  expect_true("sas-missing-sort-semantics" %in% names(catalog))
  expect_true("sas-dataset-row-alignment" %in% names(catalog))
})

test_that("installed package skills have valid schemas and expected bindings", {
  catalog <- sas2r:::agent_skill_catalog()

  # sas-missing-sort-semantics
  sort_skill <- catalog[["sas-missing-sort-semantics"]]
  expect_s3_class(sort_skill, "sas2r_agent_skill")
  expect_identical(sort_skill$skill_id, "sas-missing-sort-semantics")
  expect_identical(sort_skill$folder_name, "sas-missing-sort-semantics")
  expect_identical(sort_skill$version, 1L)
  expect_true(is.character(sort_skill$description) && nzchar(sort_skill$description))
  expect_true(is.character(sort_skill$body) && nzchar(sort_skill$body))
  expect_setequal(sort_skill$agents, c("translator", "reviewer", "fixer"))
  expect_identical(as.integer(sort_skill$priority), 100L)
  expect_identical(sort_skill$tools, c("lookup_rulebook", "query_project_graph"))
  expect_true("sort" %in% sort_skill$triggers$procs)
  expect_true("by_group" %in% sort_skill$triggers$flags)

  # sas-dataset-row-alignment
  align_skill <- catalog[["sas-dataset-row-alignment"]]
  expect_s3_class(align_skill, "sas2r_agent_skill")
  expect_identical(align_skill$skill_id, "sas-dataset-row-alignment")
  expect_identical(align_skill$folder_name, "sas-dataset-row-alignment")
  expect_identical(align_skill$version, 1L)
  expect_true(is.character(align_skill$description) && nzchar(align_skill$description))
  expect_true(is.character(align_skill$body) && nzchar(align_skill$body))
  expect_setequal(align_skill$agents, c("translator", "reviewer", "fixer"))
  expect_identical(as.integer(align_skill$priority), 90L)
  expect_identical(align_skill$tools, c("lookup_rulebook", "query_project_graph"))
  expect_true("duplicate_keys" %in% align_skill$triggers$flags)
})

test_that("skill loader validates all metadata fields and content hash stability", {
  catalog <- sas2r:::agent_skill_catalog()
  for (nm in names(catalog)) {
    skill <- catalog[[nm]]
    expect_match(skill$content_hash, "^[a-f0-9]{64}$")
    expect_false(grepl("\r", skill$body)) # normalized line endings
    expect_true(is.list(skill$triggers))
    expect_true(length(skill$tools) > 0L)
    expect_true(length(skill$agents) > 0L)
    expect_true(is.numeric(skill$priority))
  }
})
