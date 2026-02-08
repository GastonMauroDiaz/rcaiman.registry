## code to prepare `iPhone6plus.Olloclip` dataset goes here

iPhone6plus.Olloclip <- new_registry(
  id = "iPhone6plus.Olloclip",
  device = "iPhone 6 Plus (rear camera)",
  device_manufacturer = "Apple Inc",
  auxiliary_lens = "Olloclip",
  auxiliary_lens_manufacturer = "Olloclip LLC")

iPhone6plus.Olloclip <- add_file_identity(
  iPhone6plus.Olloclip,
  id = "raw",
  extension = "DNG",
  filename_pattern = "^APC_[0-9]{4}$"
)

iPhone6plus.Olloclip <- add_embedded_metadata_identity(
  iPhone6plus.Olloclip,
  id = "exif_01",
  namespace = "exif",
  rules = list(
    "Camera Model Name" = "iPhone 6s Plus",
    "Software" = "Adobe Lightroom 7.1.0 (iOS)",
    "Lens ID" = "iPhone 6s Plus back camera 4.15mm f/2.2",
    "CFA Pattern" = "[Red,Green][Green,Blue]"
  ),
  notes = "Produced with ExifTool Version Number 12.92",
  contact_information = "gastonmaurodiaz@gmail.com"
)

iPhone6plus.Olloclip <- add_geometry_spec(
  iPhone6plus.Olloclip,
  id = "simple_method",
  lens_coef = c(0.801, 0.178, -0.179),
  horizon_radius = 1002,
  is_horizon_circle_clipped = TRUE,
  zenith_colrow = c(960, 738),
  max_zenith_angle = 86.2,
  dim = c(2016, 1512),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020 and revised in 2025",
  contact_information = "gastonmaurodiaz@gmail.com"
)

iPhone6plus.Olloclip <- add_radiometry_spec(
  iPhone6plus.Olloclip,
  geometry_id = "simple_method",
  id = "spectral_bands",
  type = "interpretive_constraint",
  cfa_pattern = matrix(c("Red", "Green1",
                         "Green2", "Blue"), byrow = TRUE, ncol = 2),
  spectral_mapping = list(Red = function(Red) Red,
                          Green = function(Green1, Green2) (Green1+Green2)/2,
                          Blue = function(Blue) Blue),
  contact_information = "gastonmaurodiaz@gmail.com"
)


iPhone6plus.Olloclip <- add_radiometry_spec(
  iPhone6plus.Olloclip,
  geometry_id = "simple_method",
  id = "simple_method",
  type = "vignetting_correction",
  scheme = "simple",
  model_type = "polynomial",
  parameters = list("2.2" = c(-0.0111, -0.702, 0.327)),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020 and revised in 2025",
  contact_information = "gastonmaurodiaz@gmail.com"
)

usethis::use_data(iPhone6plus.Olloclip, overwrite = TRUE)
