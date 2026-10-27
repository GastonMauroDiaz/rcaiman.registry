band_test_entry <- function() {
  entry <- new_registry_entry("test", device = "Test device")
  for (id in c("raw", "jpeg")) {
    entry <- add_signature(
      entry, id, dim = c(20, 16),
      validated_with_libraw = NA_character_,
      validated_with_exiftool = NA_character_,
      rules = list(model = list(group = "IFD0", tag = "Model",
                               label = "Model", expected = "Test device"))
    )
  }
  entry
}

band_test_geometry <- function(entry, metadata, band, id = "simple", radius = 3) {
  add_geometry_spec(entry, metadata, band, id, model = "radial_projection",
                    parameters = equidistant_coef,
                    center_of_vision = c(10, 8), normalization_radius = radius,
                    normalization_angle = 90)
}

band_test_cfa <- function() {
  matrix(c("R", "G1", "G2", "B"), ncol = 2, byrow = TRUE)
}

test_that("band interpretation is stored once above geometry and radiometry", {
  entry <- band_test_entry()
  original <- entry
  date <- as.Date("2026-09-25")
  mapping <- list(Red = "R", Green = "(G1 + G2)/2", Blue = "B")
  entry <- add_band_spec(entry, "raw", "bayer", band_test_cfa(), mapping,
                         date, "Band interpretation", "Maintainer")
  band <- get_band_spec(entry, "raw", "bayer")
  expect_s3_class(band, "band_spec")
  expect_identical(band$cfa_pattern, band_test_cfa())
  expect_identical(band$spectral_mapping, mapping)
  expect_identical(band$date, date)
  expect_identical(band$notes, "Band interpretation")
  expect_identical(band$contact_information, "Maintainer")
  expect_identical(band$geometry, list())
  expect_identical(entry$raw$dim, c(20, 16))
  expect_false("geometry" %in% names(entry$raw))
  expect_identical(original$raw$bands, list())

  entry <- band_test_geometry(entry, "raw", "bayer")
  entry <- add_radiometry_spec(
    entry, "raw", "bayer", "simple", "flat",
    model = "simple_polynomial", parameters = list("5" = list(Blue = c(0.1, 0.2))),
    date = date
  )
  geometry <- get_geometry_spec(entry, "raw", "bayer", "simple")
  radiometry <- get_radiometry_spec(entry, "raw", "bayer", "simple", "flat")
  expect_s3_class(geometry, "geometry_spec")
  expect_s3_class(radiometry, "radiometry_spec")
  expect_false("type" %in% names(radiometry))
  expect_false(any(c("cfa_pattern", "spectral_mapping") %in% names(radiometry)))
  expect_identical(get_band_spec(entry, "raw", "bayer")$spectral_mapping, mapping)
  expect_error(add_radiometry_spec(
    entry, "raw", "bayer", "simple", "old", type = "interpretive_constraint"
  ), "unused argument")
  expect_error(add_radiometry_spec(
    entry, "raw", "bayer", "simple", "old", cfa_pattern = band_test_cfa()
  ), "unused argument")
})

test_that("accessors follow complete branches with repeated sibling identifiers", {
  entry <- band_test_entry()
  for (metadata in c("raw", "jpeg")) {
    for (band in c("first", "second")) {
      entry <- add_band_spec(entry, metadata, band)
      radius <- if (band == "first") 3 else 4
      entry <- band_test_geometry(entry, metadata, band, radius = radius)
      entry <- add_radiometry_spec(
        entry, metadata, band, "simple", "flat",
        model = "simple_polynomial",
        parameters = list("5" = list(Blue = c(radius, if (metadata == "raw") 1 else 2)))
      )
      em <- get_signature(entry, metadata)
      bs <- get_band_spec(em, band)
      expect_identical(bs, get_band_spec(entry, metadata, band))
      gs <- get_geometry_spec(bs, "simple")
      expect_identical(gs, get_geometry_spec(em, band, "simple"))
      expect_identical(gs, get_geometry_spec(entry, metadata, band, "simple"))
      expect_equal(gs$normalization_radius, radius)
      rs <- get_radiometry_spec(gs, "flat")
      expect_identical(rs, get_radiometry_spec(entry, metadata, band,
                                               "simple", "flat"))
    }
  }
  expect_error(add_band_spec(entry, "raw", "first"), "already exists")
  expect_error(band_test_geometry(entry, "raw", "first"), "already exists")
  expect_error(add_radiometry_spec(entry, "raw", "first", "simple", "flat"),
                 "already exists")
  expect_error(get_band_spec(entry, "absent", "first"), "absent")
  expect_error(get_band_spec(entry, "raw", "absent"), "under embedded metadata 'raw'")
  expect_error(get_geometry_spec(entry, "raw", "first", "absent"),
                 "under band specification 'first'")
  expect_error(get_radiometry_spec(entry, "raw", "first", "simple", "absent"),
                 "under geometry 'simple'")
  expect_error(band_test_geometry(entry, "raw", "absent"), "absent")
  expect_error(add_band_spec(entry, "absent", "new"), "absent")
  expect_error(get_band_spec(entry, "raw"), "missing")
})

test_that("optional band fields are explicit and validated when supplied", {
  entry <- add_band_spec(band_test_entry(), "jpeg", "native")
  band <- get_band_spec(entry, "jpeg", "native")
  expect_null(band$cfa_pattern)
  expect_null(band$spectral_mapping)
  expect_s3_class(band$date, "Date")
  entry <- add_band_spec(entry, "raw", "cfa_only", cfa_pattern = band_test_cfa())
  expect_null(get_band_spec(entry, "raw", "cfa_only")$spectral_mapping)
  expect_error(add_band_spec(entry, "raw", "bad", spectral_mapping = list(R = "R")),
                 "requires `cfa_pattern`")
  expect_error(add_band_spec(entry, "raw", "bad", cfa_pattern = matrix("R", 2, 2)),
                 "four unique")
  expect_error(add_band_spec(entry, "raw", "bad", cfa_pattern = matrix("R", 1, 1)),
                 "2x2")
  expect_error(add_band_spec(entry, "raw", "bad", band_test_cfa(),
                              list(Red = "unknown")), "Invalid token")
  expect_error(add_band_spec(entry, "raw", "bad", band_test_cfa(),
                              list(Red = "sqrt(R)")), "Invalid token")
  expect_error(add_band_spec(entry, "raw", "Bad Name"), "snake_case")
  expect_error(add_band_spec(entry, "raw", "bad", date = "2026-09-25"))
  expect_error(add_band_spec(entry, "raw", "bad", notes = 1))
  malformed <- get_signature(entry, "raw")
  malformed$bands <- NULL
  expect_error(get_band_spec(malformed, "cfa_only"), "Band container")
  malformed <- get_band_spec(entry, "raw", "cfa_only")
  malformed$geometry <- NULL
  expect_error(get_geometry_spec(malformed, "simple"), "Geometry container")
})

test_that("index and displays expose bands, dimensions and empty branches", {
  entry <- new_registry_entry("test", device = "Test")
  invisible(capture.output(printed <- withVisible(print(entry))))
  expect_identical(printed$visible, FALSE)
  expect_output(summary(entry), "Signatures \\(0\\)")
  entry <- band_test_entry()
  expect_output(print(entry), "Total band specifications: 0")
  expect_output(summary(entry), "Band specifications \\(0\\)")
  entry <- add_band_spec(entry, "raw", "first")
  expect_output(summary(entry), "Geometry specifications \\(0\\)")
  entry <- add_band_spec(entry, "raw", "second")
  entry <- band_test_geometry(entry, "raw", "first")
  entry <- add_radiometry_spec(
    entry, "raw", "first", "simple", "flat",
    model = "simple_polynomial", parameters = list("5" = list(Blue = c(0.1, 0.2)))
  )
  entry <- band_test_geometry(entry, "raw", "first", "extra")
  entry <- band_test_geometry(entry, "raw", "second")
  idx <- inspect_registry_index(entry)$signature
  expect_identical(idx$raw$dim, c(20, 16))
  expect_identical(idx$raw$band_spec_ids, c("first", "second"))
  expect_identical(idx$raw$band_spec$first$geometry_spec_ids, c("simple", "extra"))
  expect_identical(idx$raw$band_spec$first$radiometry_spec_ids,
                    list(simple = "flat", extra = character(0)))
  expect_identical(idx$jpeg$band_spec_ids, character(0))
  expect_identical(idx$jpeg$band_spec, list())
  printed <- capture.output(expect_identical(print(entry), entry))
  expect_true("Total band specifications: 2" %in% printed)
  expect_true("Total geometry specifications: 3" %in% printed)
  expect_true("Total radiometry specifications: 1" %in% printed)
  summarized <- capture.output(expect_identical(summary(entry), entry))
  expect_true("      - 20 x 16" %in% summarized)
  expect_true("    Band specifications (2):" %in% summarized)
  expect_true("      - first" %in% summarized)
  expect_true("            - simple" %in% summarized)
  expect_true("                  * flat" %in% summarized)
  expect_true("                Radiometry specifications: none" %in% summarized)
})
