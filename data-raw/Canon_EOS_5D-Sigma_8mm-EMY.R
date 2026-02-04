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

Canon_EOS_5D.Sigma_8mm.EMY <- add_embedded_metadata_identity(
  Canon_EOS_5D.Sigma_8mm.EMY,
  id = "exif_01",
  namespace = "exif",
  rules = list(
    "Camera Model Name" = "Canon EOS 5D",
    "Internal Serial Number" = "E725462",
    "Serial Number" = "2331203538",
    "Lens ID" = "Sigma 8mm f/3.5 EX DG Circular Fisheye",
    "Canon Firmware Version" = "Firmware Version 1.1.0",
    "CR2 CFA Pattern" = "[Red,Green][Green,Blue]"
  ),
  firmware_version = "1.1.0",
  notes = "EXIF expectations derived from Mait Lang calibration plugin.",
  contact_information = "lang@to.ee"
)

Canon_EOS_5D.Sigma_8mm.EMY <- add_geometry_spec(
  Canon_EOS_5D.Sigma_8mm.EMY,
  id = "HSP",
  lens_coef = c(0.702301, 0.043405, -0.05425),
  zenith_colrow = round(c(2190+90, 1453+34)/2,0),
  horizon_radius = 693,
  dim = c(4476, 2954),
  firmware_version = "1.1.0",
  notes = "From Mait Lang calibration plugin.",
  contact_information = "lang@to.ee"
)

Canon_EOS_5D.Sigma_8mm.EMY <- add_radiometry_spec(
  Canon_EOS_5D.Sigma_8mm.EMY,
  geometry_id = "HSP",
  id = "HSP",
  type = "vignetting",
  scheme = "free_form",
  model_type = "legacy",
  parameters = list( "0" = 0),
  firmware_version = "1.1.0",
  notes = "Full model available in rcaiman::correct_vignetting_legacy()",
  contact_information = "lang@to.ee"
)

usethis::use_data(Canon_EOS_5D.Sigma_8mm.EMY, overwrite = TRUE)
