## code to prepare `Canon_EOS_5D.Sigma_8mm.EMY` dataset goes here

Canon_EOS_5D.Sigma_8mm.EMY <- new_registry(
  id = "Canon_EOS_5D.Sigma_8mm.EMY",
  body = "EOS 5D",
  body_serial = "2331203538",
  body_manufacturer = "Canon Inc",
  lens = "Sigma 8mm F3.5 EX DG",
  lens_manufacturer = "Sigma Corp",
  institution = "Eesti Maaülikool (Estonian University of Life Sciences)"
)

Canon_EOS_5D.Sigma_8mm.EMY <- add_file_identity(
  Canon_EOS_5D.Sigma_8mm.EMY,
  id = "raw",
  extension = "CR2"
)

Canon_EOS_5D.Sigma_8mm.EMY <- add_embedded_metadata_identity(
  Canon_EOS_5D.Sigma_8mm.EMY,
  id = "exif_01",
  namespace = "exif",
  rules = list(
    "Exif Version" = "0221",
    "Camera Model Name" = "Canon EOS 5D",
    "Internal Serial Number" = "E725462",
    "Serial Number" = "2331203538",
    "Lens ID" = "Sigma 8mm f/3.5 EX DG Circular Fisheye",
    "Canon Firmware Version" = "Firmware Version 1.1.0",
    "CR2 CFA Pattern" = "[Red,Green][Green,Blue]",
    "Bits Per Sample" = "12"
  ),
  firmware_version = "1.1.0",
  notes = "Produced with ExifTool Version Number 12.92",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_5D.Sigma_8mm.EMY <- add_geometry_spec(
  Canon_EOS_5D.Sigma_8mm.EMY,
  id = "hsp_legacy",
  lens_coef = c(0.702301, 0.043405, -0.05425),
  zenith_colrow = round(c(2190+90, 1453+34)/2,0),
  horizon_radius = 694,
  is_horizon_circle_clipped = FALSE,
  dim = c(4476, 2954),
  firmware_version = "1.1.0",
  notes = "Based on script started on 06.11.2025 by Mait Lang, Tartu Observatory",
  contact_information = "lang@to.ee"
)

Canon_EOS_5D.Sigma_8mm.EMY <- add_radiometry_spec(
  Canon_EOS_5D.Sigma_8mm.EMY,
  geometry_id = "hsp_legacy",
  id = "spectral_bands",
  type = "interpretive_constraint",
  cfa_pattern = matrix(c("Red","Green1",
                         "Green2","Blue"), byrow = TRUE, ncol = 2),
  spectral_mapping = list(Red = function(Red) Red,
                          Green = function(Green1, Green2) mean(Green1, Green2),
                          Blue = function(Blue) Blue),
  offset_value = list("400" = 128),
  firmware_version = "1.1.0",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_5D.Sigma_8mm.EMY <- add_radiometry_spec(
  Canon_EOS_5D.Sigma_8mm.EMY,
  geometry_id = "hsp_legacy",
  id = "legacy",
  type = "vignetting_correction",
  scheme = "legacy",
  notes = "Use rcaiman::vignetting_correction_legacy()",
)

usethis::use_data(Canon_EOS_5D.Sigma_8mm.EMY, overwrite = TRUE)
