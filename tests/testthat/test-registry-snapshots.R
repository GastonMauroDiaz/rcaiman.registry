snapshot_entry <- function() {
  entry <- new_registry_entry("snapshot_test", body = "Body", lens = "Lens",
                               lens_serial = "001", institution = "Test")
  entry <- add_signature(
    entry, "raw", dim = c(20, 12), validated_with_libraw = NA_character_,
    validated_with_exiftool = "13.59", date = as.Date("2026-09-28"),
    rules = list(model = list(group = "IFD0", tag = "Model",
                              label = "Model", expected = "Body")))
  entry <- add_band_spec(entry, "raw", "bands",
    cfa_pattern = matrix(c("R", "G1", "G2", "B"), 2, byrow = TRUE,
                         dimnames = list(c("top", "bottom"), c("left", "right"))),
    spectral_mapping = list(Red = "R", Green = "(G1 + G2)/2", Blue = "B"))
  entry <- add_geometry_spec(entry, "raw", "bands", "radial",
    model = "radial_projection", parameters = c(linear = 3 / pi),
    center_of_vision = c(x = 10, y_down = 6), normalization_radius = 10.25,
    normalization_angle = 60, max_off_axis_angle = 100,
    max_observed_off_axis_angle = 75)
  entry <- add_geometry_spec(entry, "raw", "bands", "anisotropic",
    model = "anisotropic_projection", center_of_vision = c(10, 6),
    parameters = list(off_axis_coef = c(0.4, 0.05),
      sin_internal_azimuth_coef = c(0, 1), cos_internal_azimuth_coef = c(-1, 0),
      atan_scale = .6))
  samples <- expand.grid(x = seq(.5, 19.5, length.out = 8),
                         y_down = seq(.5, 11.5, length.out = 8))
  samples$relative_response <- 1 - .001 * (samples$x - 10)^2 -
    .002 * (samples$y_down - 6)^2
  p <- fit_trend_surface(samples, "relative_response", np = 2)
  models <- list(surface = list(model = "trend_surface_image",
                                parameters = list("5.0" = list(Blue = p), "8" = list(Blue = p))),
                 simple = list(model = "simple_polynomial",
                               parameters = list("0" = list(Blue = c(a = .01, b = -.02)))),
                 free = list(model = "free_form_polynomial",
                             parameters = list("5" = list(Blue = c(1, .01, -.02)))),
                 lang = list(model = "lang2010", parameters = NULL))
  for (geometry in c("radial", "anisotropic")) {
    for (id in names(models)) {
      entry <- do.call(add_radiometry_spec, c(list(entry, "raw", "bands", geometry, id),
                                             models[[id]]))
    }
  }
  add_band_spec(entry, "raw", "other")
}

snapshot_roundtrip <- function(entry, geometry = "radial", radiometry = NULL) {
  file <- tempfile(fileext = ".yaml")
  on.exit(unlink(file))
  export_registry_snapshot(entry, "raw", "bands", geometry, radiometry, file)
  read_registry_snapshot(file)
}

test_that("snapshots select one branch and restore dates, bands, and geometry", {
  entry <- snapshot_entry()
  original <- entry
  for (geometry in c("radial", "anisotropic")) {
    restored <- snapshot_roundtrip(entry, geometry)
    expect_s3_class(restored, "hs_registry_entry")
    expect_identical(names(restored), c("instrument_metadata", "raw"))
    expect_identical(restored$instrument_metadata, entry$instrument_metadata)
    ems <- get_signature(restored, "raw")
    expect_identical(ems$date, entry$raw$date)
    expect_identical(ems$rules, entry$raw$rules)
    expect_identical(ems$validated_with_libraw, NA_character_)
    expect_identical(names(ems$bands), "bands")
    band <- get_band_spec(restored, "raw", "bands")
    expect_identical(band$cfa_pattern, entry$raw$bands$bands$cfa_pattern)
    expect_identical(band$spectral_mapping, entry$raw$bands$bands$spectral_mapping)
    expect_identical(names(band$geometry), geometry)
    g <- get_geometry_spec(restored, "raw", "bands", geometry)
    expected <- get_geometry_spec(entry, "raw", "bands", geometry)
    expected$radiometry <- list()
    expect_equal(g, expected, tolerance = 1e-15)
    expect_equal(values(set_internal_geometry(ems$dim, g)),
                   values(set_internal_geometry(ems$dim, expected)), tolerance = 1e-12)
  }
  expect_identical(entry, original)
})

test_that("all radiometry evaluators retain predictions through YAML", {
  entry <- snapshot_entry()
  for (geometry in c("radial", "anisotropic")) {
    for (id in c("surface", "simple", "free", "lang")) {
      restored <- snapshot_roundtrip(entry, geometry, id)
      g <- get_geometry_spec(restored, "raw", "bands", geometry)
      expect_identical(names(g$radiometry), id)
      spec <- get_radiometry_spec(g, id)
      before <- get_radiometry_spec(entry, "raw", "bands", geometry, id)
      expect_equal(spec, before, tolerance = 1e-15)
      ig <- set_internal_geometry(restored$raw$dim, g)
      evaluate <- function(spec) {
        if (spec$model == "lang2010")
          flat_field_image(ig$off_axis, spec$model, aperture = 5)
        else if (spec$model == "trend_surface_image")
          trend_surface_image(ig[[1]], spec$parameters[[1]]$Blue)
        else flat_field_image(ig$off_axis, spec$model, spec$parameters[[1]]$Blue)
      }
      expect_equal(values(evaluate(spec)), values(evaluate(before)), tolerance = 1e-12)
    }
  }
})

test_that("snapshot reader checks its schema and reconstructed contracts", {
  file <- tempfile(fileext = ".yaml")
  on.exit(unlink(file))
  export_registry_snapshot(snapshot_entry(), "raw", "bands", "radial", file = file)
  original <- yaml::read_yaml(file)
  reordered <- original
  reordered$configuration <- rev(reordered$configuration)
  yaml::write_yaml(reordered, file, precision = 17)
  expect_s3_class(read_registry_snapshot(file), "hs_registry_entry")
  bad <- original
  bad$schema_version <- 3L
  yaml::write_yaml(bad, file)
  expect_error(read_registry_snapshot(file), "schema")
  bad <- original
  bad$registry_entry$band_spec <- NULL
  yaml::write_yaml(bad, file)
  expect_error(read_registry_snapshot(file), "Incomplete")
  bad <- original
  bad$configuration$geometry_spec <- "another"
  yaml::write_yaml(bad, file)
  expect_error(read_registry_snapshot(file), "configuration")
  bad <- original
  bad$registry_entry$geometry_spec$normalization_radius <- -1
  yaml::write_yaml(bad, file)
  expect_error(read_registry_snapshot(file), "normalization_radius")
  bad <- original
  bad$registry_entry$band_spec$cfa_pattern$values <- c("R", "G1", "G2")
  yaml::write_yaml(bad, file)
  expect_error(read_registry_snapshot(file), "four values")
})

test_that("snapshots preserve empty band interpretation and large numeric coefficients", {
  entry <- snapshot_entry()
  entry$raw$bands$bands$cfa_pattern <- NULL
  entry$raw$bands$bands$spectral_mapping <- NULL
  entry <- add_radiometry_spec(entry, "raw", "bands", "radial", "large",
    model = "free_form_polynomial", parameters = list("5" = list(Blue = c(3e10, 1e-15))))
  restored <- snapshot_roundtrip(entry, radiometry = "large")
  expect_null(get_band_spec(restored, "raw", "bands")$cfa_pattern)
  expect_equal(get_radiometry_spec(restored, "raw", "bands", "radial", "large")$parameters,
               list("5" = list(Blue = c(3e10, 1e-15))), tolerance = 1e-15)
})

test_that("distributed snapshot is usable by the core", {
  for (file in c("my_entry_v1.yaml", "my_entry_v2.yaml")) {
    entry <- read_registry_snapshot(system.file("external", file,
                                                package = "rcaiman.registry"))
    expect_s3_class(get_signature(entry, "raw"), "signature")
    g <- get_geometry_spec(entry, "raw", "cygm", "simple_method")
    expect_s3_class(g, "geometry_spec")
    expect_s4_class(set_internal_geometry(entry$raw$dim, g), "SpatRaster")
  }
})

test_that("snapshots can end at bands with or without available geometries", {
  entry <- snapshot_entry()
  original <- entry
  file <- tempfile(fileext = ".yaml")
  on.exit(unlink(file))
  export_registry_snapshot(entry, "raw", "bands", file = file)
  snapshot <- yaml::read_yaml(file)
  expect_null(snapshot$configuration$geometry_spec)
  expect_null(snapshot$registry_entry$geometry_spec)
  expect_null(snapshot$registry_entry$radiometry_spec)
  restored <- read_registry_snapshot(file)
  expected <- entry
  expected$raw$bands <- expected$raw$bands["bands"]
  expected$raw$bands$bands$geometry <- list()
  expect_equal(restored, expected)
  expect_output(summary(restored), "bands")
  expect_identical(entry, original)

  export_registry_snapshot(entry, "raw", "other", geometry_spec = NULL, file = file)
  restored <- read_registry_snapshot(file)
  band <- get_band_spec(restored, "raw", "other")
  expect_identical(band, get_band_spec(entry, "raw", "other"))
  expect_identical(names(restored$raw$bands), "other")
})

test_that("snapshot radiometry requires a selected geometry", {
  entry <- snapshot_entry()
  file <- tempfile(fileext = ".yaml")
  on.exit(unlink(file))
  expect_error(export_registry_snapshot(entry, "raw", "bands",
    radiometry_spec = "lang", file = file), "requires `geometry_spec`")
  expect_false(file.exists(file))
  export_registry_snapshot(entry, "raw", "bands", "radial", "lang", file)
  snapshot <- yaml::read_yaml(file)
  snapshot$registry_entry["geometry_spec"] <- list(NULL)
  snapshot$configuration["geometry_spec"] <- list(NULL)
  yaml::write_yaml(snapshot, file)
  expect_error(read_registry_snapshot(file), "requires a geometry specification")
})


test_that("snapshots name radiometry consistently and omit its retired type", {
  file <- tempfile(fileext = ".yaml")
  on.exit(unlink(file))
  export_registry_snapshot(snapshot_entry(), "raw", "bands", "radial",
    radiometry_spec = "simple", file = file)
  snapshot <- yaml::read_yaml(file)
  expect_equal(snapshot$schema_version, 4)
  expect_identical(snapshot$configuration$radiometry_spec, "simple")
  expect_identical(snapshot$registry_entry$radiometry_spec$id, "simple")
  expect_false("flat_field_correction" %in% names(snapshot$configuration))
  expect_false("flat_field_correction" %in% names(snapshot$registry_entry))
  expect_false("type" %in% names(snapshot$registry_entry$radiometry_spec))
  restored <- read_registry_snapshot(file)
  expect_equal(get_radiometry_spec(restored, "raw", "bands", "radial", "simple"),
    get_radiometry_spec(snapshot_entry(), "raw", "bands", "radial", "simple"))
})
