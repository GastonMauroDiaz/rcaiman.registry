radiometry_surface_entry <- function() {
  entry <- new_registry_entry("spatial_test", device = "Test device")
  entry <- add_signature(
    entry, "raw", dim = c(20, 12), validated_with_libraw = NA_character_,
    validated_with_exiftool = NA_character_,
    rules = list(model = list(group = "IFD0", tag = "Model",
                              label = "Model", expected = "Test device"))
  )
  entry <- add_band_spec(entry, "raw", "bands")
  add_geometry_spec(entry, "raw", "bands", "radial",
                    model = "radial_projection", parameters = 2 / pi,
                    center_of_vision = c(10, 6), normalization_radius = 10,
                    normalization_angle = 90)
}

radiometry_surface_parameters <- function() {
  samples <- expand.grid(x = seq(0.5, 19.5, length.out = 8),
                         y_down = seq(0.5, 11.5, length.out = 8))
  samples$relative_response <- 1 - .001 * (samples$x - 10)^2 -
    .002 * (samples$y_down - 6)^2
  fit_trend_surface(samples, col_id = "relative_response", np = 2)
}

test_that("registry stores compact surfaces by aperture and core evaluates them", {
  entry <- radiometry_surface_entry()
  original <- entry
  p <- radiometry_surface_parameters()
  q <- p
  q$beta <- p$beta * 0.95
  entry <- add_radiometry_spec(entry, "raw", "bands", "radial", "surface",
                               model = "predict_trend_surface",
                               parameters = list("5" = list(Blue = p), "8" = list(Blue = q)))
  spec <- get_radiometry_spec(entry, "raw", "bands", "radial", "surface")
  expect_s3_class(spec, "radiometry_spec")
  expect_false("type" %in% names(spec))
  expect_identical(spec$model, "predict_trend_surface")
  expect_identical(spec$parameters, list("5" = list(Blue = p), "8" = list(Blue = q)))
  expect_false("data" %in% names(spec))
  expect_length(get_geometry_spec(original, "raw", "bands", "radial")$radiometry, 0)
  r <- set_rcaiman_geometry(terra::rast(nrows = 12, ncols = 20))
  evaluate <- getExportedValue("rcaiman", spec$model)
  ff <- evaluate(r, spec$parameters[["5"]]$Blue)
  expect_equal(terra::values(ff), terra::values(predict_trend_surface(r, p)))
  expect_equal(terra::values(evaluate(r, spec$parameters[["8"]]$Blue)),
               terra::values(ff * 0.95))
  expect_equal(terra::values(apply_flat_field(100 * ff, ff)),
               matrix(100, nrow = ncell(r), dimnames = list(NULL, "prediction")))
  expect_output(print(entry), "spatial_test")
  expect_output(summary(entry), "surface")
})

test_that("surface validation uses the shared core contract", {
  p <- radiometry_surface_parameters()
  make <- function(parameters, model = "predict_trend_surface") {
    .new_radiometry_spec("surface", model = model, parameters = parameters,
                         date = Sys.Date())
  }
  expect_identical(make(list("0" = list(Blue = p)))$parameters[["0"]]$Blue, p)
  expect_error(make(list("5" = list(Blue = p[-1]))), "np")
  bad <- p
  bad$beta <- 1
  expect_error(make(list("5" = list(Blue = bad))), "beta")
  bad <- p
  bad$rx <- rev(bad$rx)
  expect_error(make(list("5" = list(Blue = bad))), "increasing")
  expect_error(make(NULL), "non-empty")
  expect_error(make(list()), "non-empty")
  expect_error(make(list(p)), "names")
  expect_error(make(list("5" = list(Blue = p), "5.0" = list(Blue = p))), "Duplicated")
  expect_error(make(list("Inf" = p)), "finite")
  expect_error(make(list("-1" = list(Blue = p))), "non-negative")
  expect_error(make(list("5" = list(Blue = p)), "ff_trend_surface"), "supported")
  expect_error(.new_radiometry_spec("surface", model = "predict_trend_surface",
                                    data = list(), date = Sys.Date()), "unused argument")
  for (model in c("simple_polynomial", "free_form_polynomial")) {
    expect_identical(make(list("5" = list(Blue = c(1, .1))), model)$parameters,
                      list("5" = list(Blue = c(1, .1))))
    expect_error(make(list("5" = list(Blue = p)), model), "atomic vector")
  }
})
test_that("Lang registers an empirical evaluator without fitted parameters", {
  entry <- radiometry_surface_entry()
  entry <- add_radiometry_spec(entry, "raw", "bands", "radial", "lang",
                               model = "lang2010")
  spec <- get_radiometry_spec(entry, "raw", "bands", "radial", "lang")
  expect_null(spec$parameters)
  expect_identical(spec$rcaiman_version, as.character(utils::packageVersion("rcaiman")))
  ig <- set_internal_geometry(entry$raw$dim,
    get_geometry_spec(entry, "raw", "bands", "radial"))
  expect_equal(values(flat_field_image(ig$off_axis, spec$model, aperture = 5)),
               values(flat_field_image(ig$off_axis, "lang2010", aperture = 5)))
  expect_false(isTRUE(all.equal(
    values(flat_field_image(ig$off_axis, spec$model, aperture = 5)),
    values(flat_field_image(ig$off_axis, spec$model, aperture = 8)))))
  expect_error(add_radiometry_spec(entry, "raw", "bands", "radial", "bad",
    model = "lang2010", parameters = list("5" = list(Blue = 1))), "must be NULL")
  expect_output(summary(entry), "lang")
})

test_that("apertures explicitly identify bands and snapshots preserve every leaf", {
  p <- radiometry_surface_parameters()
  q <- p; q$beta <- q$beta * .9
  parameters <- list("5.6"=list(Blue=p,Cyan=q),"8"=list(Cyan=p))
  entry <- add_radiometry_spec(radiometry_surface_entry(),"raw","bands","radial",
                              "multiband",model="predict_trend_surface",parameters=parameters)
  for (extension in c("yaml","json")) {
    path <- tempfile(fileext=paste0(".",extension))
    export_registry_snapshot(entry,"raw","bands","radial","multiband",file=path)
    restored <- read_registry_snapshot(path)
    got <- get_radiometry_spec(restored,"raw","bands","radial","multiband")
    expect_equal(got$parameters,parameters,tolerance=1e-12)
  }
  make <- function(x) .new_radiometry_spec("test",model="simple_polynomial",
                                          parameters=x,date=Sys.Date())
  expect_error(make(list("5"=c(.1,.2))),"band names")
  expect_error(make(list("5"=list())),"band names")
  expect_error(make(list("5"=list(Blue=.1,Blue=.2))),"band names")
  expect_error(make(list("5"=setNames(list(.1),""))),"band names")
  expect_identical(make(list("0"=list(Cyan=c(.1,.2))))$parameters,
                   list("0"=list(Cyan=c(.1,.2))))
})
