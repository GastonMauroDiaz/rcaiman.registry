## code to prepare `Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen` dataset goes here

# This file was created with usethis::use_data_raw("Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen")

Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen <- new_registry(
  id = "Canon EOS 6D Mark II + Zenitar 8mm, University of Goettingen",
  body = "EOS 6D Mark II",
  body_manufacturer = "Canon Inc",
  lens = "MC-Zenitar 8 mm f/3.5 fish-eye lens, Canon EF-Mount",
  lens_manufacturer = "Zenit",
  institution = "University of Goettingen"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen <- add_geometry_spec(
  Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen,
  id = "simple_method",
  lens_coef = c(0.718, 0.0285, -0.0511),
  zenith_colrow = c(1628, 1067),
  horizon_radius = 1010,
  max_zenith_angle = 88.07,
  dim = c(2112, 3192),
  notes = "Calibration done by Gastón Díaz and Ariel Neri at CIEFAP in 2024",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen <- add_geometry_spec(
  Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen,
  id = "simple_method_rescaled",
  lens_coef = c(0.718, 0.0285, -0.0511),
  zenith_colrow = c(3111, 2085),
  horizon_radius = 2020,
  max_zenith_angle = 88.07,
  dim = c(6246, 4160),
  notes = "Adaptation of the simple method for legacy JPEG imagery (solves raster dimension mismatch).",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen <- add_radiometry_spec(
  Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen,
  geometry_id = "simple_method",
  id = "simple_method",
  type = "vignetting",
  scheme = "simple",
  model_type = "polynomial",
  parameters = list( "22" = c(-0.0543, 0.0593, -0.0907)),
  notes = "Calibration done by Gastón Díaz and Ariel Neri at CIEFAP in 2024",
  contact_information = "gastonmaurodiaz@gmail.com"
)

usethis::use_data(Canon_EOS_6D_MarkII.Zenitar_8mm.University_of_Goettingen, overwrite = TRUE)
