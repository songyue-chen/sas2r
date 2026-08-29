test_that("add_flag combines and deduplicates flags correctly", {
  expect_identical(add_flag(character(0), "foo"), character(0))
  expect_identical(add_flag("", "foo"), "foo")
  expect_identical(add_flag(NA_character_, "foo"), "foo")
  expect_identical(add_flag("foo", "bar"), "foo,bar")
  expect_identical(add_flag("foo,bar", "foo"), "foo,bar")
  expect_identical(add_flag("foo, bar", c("baz", "bar")), "foo,bar,baz")
  expect_identical(add_flag(c("a", "b,c"), "d"), c("a,d", "b,c,d"))
})
