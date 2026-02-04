## code to prepare `Nikon_D610.Nikkor_8mm.CIEFAP` dataset goes here

# This file was created with usethis::use_data_raw("Nikon_D610.Nikkor_8mm.CIEFAP")

Nikon_D610.Nikkor_8mm.CIEFAP <- new_registry(
  "Nikon_D610.Nikkor_8mm.CIEFAP",
  body = "D610",
  body_manufacturer = "NIKON CORP",
  body_serial = "9023728",
  lens = "AF-S FISHEYE NIKKOR 8-15mm 1:3.5-4.5E ED",
  lens_manufacturer = "NIKON CORP",
  lens_serial = "210020",
  institution = "CIEFAP"
)

Nikon_D610.Nikkor_8mm.CIEFAP <- add_file_identity(
  Nikon_D610.Nikkor_8mm.CIEFAP,
  id = "raw",
  extension = "NEF",
  filename_pattern = "^DSC_[0-9]{4}$"
)

Nikon_D610.Nikkor_8mm.CIEFAP <- add_embedded_metadata_identity(
  Nikon_D610.Nikkor_8mm.CIEFAP,
  id = "exif_01",
  namespace = "exif",
  rules = list(
    "Camera Model Name" = "NIKON D610",
    "Serial Number" = "9023728",
    "Lens" = "8-15mm f/3.5-4.5",
    "Firmware Version" = "1.01",
    "CFA Pattern" = "[Red,Green][Green,Blue]"
  ),
  firmware_version = "1.01",
  notes = "Taken with ExifTool Version Number 12.92",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_D610.Nikkor_8mm.CIEFAP <- add_geometry_spec(
  Nikon_D610.Nikkor_8mm.CIEFAP,
  id = "simple_method",
  lens_coef = signif(c(1306,24.8,-56.2)/1894,3),
  zenith_colrow = c(1500, 997),
  horizon_radius = 947,
  max_zenith_angle = 92.8,
  dim = c(3040, 2014),
  firmware_version = "1.01",
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_D610.Nikkor_8mm.CIEFAP <- add_radiometry_spec(
  Nikon_D610.Nikkor_8mm.CIEFAP,
  geometry_id = "simple_method",
  id = "simple_method",
  type = "vignetting",
  scheme = "simple",
  model_type = "polynomial",
  parameters = list("3.5" = c(0.0302, 0.320, 0.0908)),
  firmware_version = "1.01",
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

Nikon_D610.Nikkor_8mm.CIEFAP <- add_radiometry_spec(
  Nikon_D610.Nikkor_8mm.CIEFAP,
  geometry_id = "simple_method",
  id = "cross_calibration",
  type = "vignetting",
  scheme = "simple",
  model_type = "polynomial",
  parameters = list(
    "4.0"	=	c(-0.0814	,	0.3440	,	-0.5010	,	0.1490),
    "4.5"	=	c(-0.0280	,	0.0252	,	0.0474	,	-0.0723),
    "5.0"	=	c(-0.0481	,	0.1360	,	-0.1530	,	0.0488),
    "5.6"	=	c(-0.0260	,	0.0481	,	-0.0579	,	0.0251),
    "6.3"	=	c(-0.0272	,	0.0809	,	-0.1090	,	0.0567),
    "7.1"	=	c(-0.0349	,	0.1150	,	-0.1540	,	0.0777),
    "9.0"	=	c(-0.0494	,	0.1880	,	-0.2560	,	0.1280),
    "10.0"	=	c(-0.0434	,	0.1590	,	-0.2150	,	0.1120),
    "11.0"	=	c(-0.0424	,	0.1560	,	-0.2160	,	0.1180),
    "13.0"	=	c(-0.0375	,	0.1270	,	-0.1710	,	0.0988),
    "14.0"	=	c(-0.0349	,	0.1210	,	-0.1740	,	0.1040),
    "16.0"	=	c(-0.0422	,	0.1620	,	-0.2300	,	0.1300),
    "18.0" =	c(-0.0507	,	0.1840	,	-0.2500	,	0.1380),
    "20.0"	=	c(-0.0424	,	0.1590	,	-0.2270	,	0.1320)
    ),
  firmware_version = "1.01",
  notes = "Cross-calibration using the simple method as reference with data taken under a clear sky at dusk.",
  contact_information = "gastonmaurodiaz@gmail.com"
)

usethis::use_data(Nikon_D610.Nikkor_8mm.CIEFAP, overwrite = TRUE)
