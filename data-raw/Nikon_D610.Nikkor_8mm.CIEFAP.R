## code to prepare `Nikon_Coolpix5700_FCE9_CIEFAP` dataset goes here

# This file was created with usethis::use_data_raw("Nikon_Coolpix5700_FCE9_CIEFAP")

Nikon_D610.Nikkor_8mm.CIEFAP <- new_registry(
  "Nikon D610 + Nikkor 8-15mm, CIEFAP",
  body = "D610",
  body_manufacturer = "NIKON CORP",
  body_serial = "9023728",
  lens = "AF-S FISHEYE NIKKOR 8-15mm 1:3.5-4.5E ED",
  lens_manufacturer = "NIKON CORP",
  lens_serial = "210020",
  institution = "CIEFAP"
)

Nikon_D610.Nikkor_8mm.CIEFAP <- add_geometry_spec(
  Nikon_D610.Nikkor_8mm.CIEFAP,
  id = "simple method",
  date = as.Date("2023-12-26"),
  lens_coef = signif(c(1306,24.8,-56.2)/1894,3),
  zenith_colrow = c(1500, 997),
  horizon_radius = 947,
  max_zenith_angle = 92.8,
  dim = c(3040, 2014),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_D610.Nikkor_8mm.CIEFAP <- add_radiometry_spec(
  Nikon_D610.Nikkor_8mm.CIEFAP,
  geometry_id = "simple method",
  id = "simple method",
  type = "vignetting",
  method = "simple",
  model = "polynomial",
  parameters = c(0.0638, 0.101),
  date = as.Date("2023-12-26"),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

usethis::use_data(Nikon_D610.Nikkor_8mm.CIEFAP, overwrite = TRUE)
