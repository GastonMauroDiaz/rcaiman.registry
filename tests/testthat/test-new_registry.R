test_that("`new_registry()` returns a `hemispherical_camera_registry`", {
  expect_true(inherits(new_registry("test", device = "test"), "hemispherical_camera_registry"))
})
