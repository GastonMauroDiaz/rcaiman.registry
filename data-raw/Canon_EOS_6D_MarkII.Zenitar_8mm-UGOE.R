## code to prepare `Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE` dataset goes here

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- new_registry(
  id = "Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE",
  body = "EOS 6D Mark II",
  body_manufacturer = "Canon Inc",
  lens = "MC-Zenitar 8 mm f/3.5 fish-eye lens, Canon EF-Mount",
  lens_manufacturer = "Zenit",
  institution = "University of Goettingen"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- add_file_identity(
  Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE,
  id = "raw",
  extension = "CR2",
  filename_pattern = "^IMG_[0-9]{4}$"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- add_file_identity(
  Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE,
  id = "jpeg",
  extension = "JPG",
  filename_pattern = "^IMG_[0-9]{4}$"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- add_embedded_metadata_identity(
  Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE,
  id = "exif_01",
  namespace = "exif",
  rules = list(
    "Camera Model Name" = "Canon EOS 6D Mark II",
    "Serial Number" = "473053001563",
    "Canon Firmware Version" = "Firmware Version 1.1.1",
    "CR2 CFA Pattern" = "[Red,Green][Green,Blue]"
  ),
  firmware_version = "1.1.1",
  notes = "Produced with ExifTool Version Number 12.92",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- add_geometry_spec(
  Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE,
  id = "simple_method",
  lens_coef = c(0.718, 0.0285, -0.0511),
  zenith_colrow = c(1628, 1067),
  horizon_radius = 1010,
  is_horizon_circle_clipped = FALSE,
  max_zenith_angle = 88.07,
  dim = c(3192, 2112),
  firmware_version = "1.1.1",
  notes = "Calibration done by Gastón Díaz and Ariel Neri at CIEFAP in 2024",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- add_geometry_spec(
  Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE,
  id = "simple_method_rescaled",
  lens_coef = c(0.718, 0.0285, -0.0511),
  zenith_colrow = c(3111, 2085),
  horizon_radius = 2020,
  is_horizon_circle_clipped = FALSE,
  max_zenith_angle = 88.07,
  dim = c(6246, 4160),
  firmware_version = "1.1.1",
  notes = "Adaptation of the simple method for legacy JPEG imagery (solves raster dimension mismatch).",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- add_radiometry_spec(
  Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE,
  geometry_id = "simple_method",
  id = "spectral_bands",
  type = "interpretive_constraint",
  cfa_pattern = matrix(c("Red","Green1",
                         "Green2", "Blue"), byrow = TRUE, ncol = 2),
  spectral_mapping = list(Red = function(Red) Red,
                          Green = function(Green1, Green2) (Green1+Green2)/2,
                          Blue = function(Blue) Blue),
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE <- add_radiometry_spec(
  Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE,
  geometry_id = "simple_method",
  id = "simple_method",
  type = "vignetting_correction",
  scheme = "simple",
  model_type = "polynomial",
  parameters = list( "22" = c(-0.0543, 0.0593, -0.0907)),
  firmware_version = "1.1.1",
  notes = "Calibration done by Gastón Díaz and Ariel Neri at CIEFAP in 2024",
  contact_information = "gastonmaurodiaz@gmail.com"
)

usethis::use_data(Canon_EOS_6D_MarkII.Zenitar_8mm.UGOE, overwrite = TRUE)
