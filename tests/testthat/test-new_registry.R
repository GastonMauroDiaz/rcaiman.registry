test_that("`new_registry_entry()` returns a `hs_registry_entry`", {
  expect_true(inherits(new_registry_entry("test", device = "test"), "hs_registry_entry"))
})
