## code to prepare `Nikon_Coolpix5700.FCE9.CIEFAP` dataset goes here

Nikon_Coolpix5700.FCE9.CIEFAP <- new_registry(
  "Nikon_Coolpix5700.FCE9.CIEFAP",
  body = "E5700",
  body_manufacturer = "NIKON CORP",
  body_serial = "7053067",
  lens = "Zoom Nikkor ED 8.9-71.2mm 1:2.8-4.2",
  lens_manufacturer = "NIKON CORP",
  auxiliary_lens = "Fisheye Converter FC-E9 0.2x",
  auxiliary_lens_manufacturer = "NIKON CORP",
  institution = "CIEFAP"
)

Nikon_Coolpix5700.FCE9.CIEFAP <- add_file_identity(
  Nikon_Coolpix5700.FCE9.CIEFAP,
  id = "raw",
  extension = "NEF",
  filename_pattern = "^DSCN[0-9]{4}$"
)

Nikon_Coolpix5700.FCE9.CIEFAP <- add_file_identity(
  Nikon_Coolpix5700.FCE9.CIEFAP,
  id = "jpeg",
  extension = "JPG",
  filename_pattern = "^DSCN[0-9]{4}$"
)

Nikon_Coolpix5700.FCE9.CIEFAP <- add_embedded_metadata_identity(
  Nikon_Coolpix5700.FCE9.CIEFAP,
  id = "exif_01",
  namespace = "exif",
  rules = list(
    "Camera Model Name" = "E5700",
    "Software" = "E5700v1.1",
    "CFA Pattern" = "[Yellow,Cyan][Green,Magenta]"
  ),
  notes = "Produced with ExifTool Version Number 12.92",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_Coolpix5700.FCE9.CIEFAP <- add_geometry_spec(
  Nikon_Coolpix5700.FCE9.CIEFAP,
  id = "delta_t",
  lens_coef = c(0.6427, 0.0346, -0.024491),
  zenith_colrow = c(1286, 986),
  horizon_radius = 742,
  is_horizon_circle_clipped = FALSE,
  max_zenith_angle = 95,
  dim = c(2560, 1920),
  notes = "Validation of lens coefficients by Delta-T in doi:10.1139/cjfr-2018-0006 (supplementary material)",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_Coolpix5700.FCE9.CIEFAP <- add_geometry_spec(
  Nikon_Coolpix5700.FCE9.CIEFAP,
  id = "simple_method",
  lens_coef = signif(c(482,23.2,-15.1)/756,3),
  zenith_colrow = c(645, 494),
  horizon_radius = 378,
  is_horizon_circle_clipped = FALSE,
  max_zenith_angle = 94.8,
  dim = c(1288, 962),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_Coolpix5700.FCE9.CIEFAP <- add_radiometry_spec(
  Nikon_Coolpix5700.FCE9.CIEFAP,
  geometry_id = "simple_method",
  id = "spectral_bands",
  type = "interpretive_constraint",
  cfa_pattern = matrix(c("Yellow", "Cyan",
                         "Green", "Magenta"), byrow = TRUE, ncol = 2),
  spectral_mapping = list(Red = function(Yellow, Magenta) (Yellow+Magenta)/2,
                          Green = function(Green) Green,
                          Blue = function(Cyan) Cyan),
  firmware_version = "1.01",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_Coolpix5700.FCE9.CIEFAP <- add_radiometry_spec(
  Nikon_Coolpix5700.FCE9.CIEFAP,
  geometry_id = "simple_method",
  id = "simple_method",
  type = "vignetting_correction",
  scheme = "simple",
  model_type = "polynomial",
  parameters = list("5.0" = c(0.0638, 0.101)),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

usethis::use_data(Nikon_Coolpix5700.FCE9.CIEFAP, overwrite = TRUE)
