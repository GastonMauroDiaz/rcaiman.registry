test_that("signature names are consistent from construction to snapshot", {
  entry <- new_registry_entry("test", device = "Test device")
  entry <- add_signature(entry, "raw", rules = list(model = list(
    group = "IFD0", tag = "Model", label = "Model", expected = "Test device")),
    dim = c(20, 12), validated_with_libraw = NA_character_,
    validated_with_exiftool = NA_character_)
  signature <- get_signature(entry, signature = "raw")
  expect_identical(class(signature), "signature")
  entry <- add_band_spec(entry, signature = "raw", id = "bands")
  entry <- add_geometry_spec(entry, signature = "raw", band_spec = "bands",
    id = "radial", model = "radial_projection", parameters = 2 / pi,
    center_of_vision = c(10, 6), normalization_radius = 10,
    normalization_angle = 90)
  signature <- get_signature(entry, "raw")
  expect_identical(get_band_spec(signature, "bands"),
    get_band_spec(entry, signature = "raw", band_spec = "bands"))
  expect_identical(get_geometry_spec(signature, "bands", "radial"),
    get_geometry_spec(entry, signature = "raw", band_spec = "bands",
                      geometry_spec = "radial"))
  expect_named(inspect_registry_index(entry), "signature")
  expect_output(print(entry), "Signatures: 1")
  expect_output(summary(entry), "Signatures \\(1\\)")
  file <- tempfile(fileext = ".yaml")
  on.exit(unlink(file))
  export_registry_snapshot(entry, signature = "raw", band_spec = "bands",
    geometry_spec = "radial", file = file)
  snapshot <- yaml::read_yaml(file)
  expect_identical(snapshot$configuration$signature, "raw")
  expect_named(snapshot$registry_entry, c("instrument_metadata", "signature",
    "band_spec", "geometry_spec", "radiometry_spec"))
  expect_equal(read_registry_snapshot(file), entry)
})
