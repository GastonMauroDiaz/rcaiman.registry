my_entry <- new_registry_entry(
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

path <- system.file("external/nikon_coolpix5700_fce9_ciefap_snippet.R",
                    package = "rcaiman.registry")
source(path)
metadata_rules

my_entry <- add_signature(
  my_entry,
  id = "raw",
  dim = c(1288, 962),
  validated_with_libraw = "0.21.1",
  validated_with_exiftool = "13.59",
  rules = metadata_rules
)

my_entry <- add_band_spec(
  my_entry,
  signature = "raw",
  id = "cygm",
  cfa_pattern =  matrix(c("Green", "Yellow",
                          "Magenta", "Cyan"), byrow = TRUE, ncol = 2),
  spectral_mapping = list(Red = "Magenta", Green = "Green", Blue = "Cyan")
)

my_entry <- add_geometry_spec(
  my_entry,
  band_spec = "cygm",
  signature = "raw",
  id = "simple_method",
  model = "radial_projection",
  parameters = c(0.63723886, 0.03455758, -0.02225094),
  center_of_vision = c(639, 469),
  normalization_radius = 377.6047,
  normalization_angle = 90,
  notes = "Documented in rcaiman.registry.data source code, folder data-raw",
  contact_information = "gastonmaurodiaz@gmail.com"
)

export_registry_snapshot(my_entry, "raw", "cygm", "simple_method",
                         file = "inst/external/my_entry_v1.yaml")

my_entry <- add_radiometry_spec(
  my_entry,
  signature = "raw",
  band_spec = "cygm",
  geometry_spec = "simple_method",
  id = "simple_method",
  model = "simple_polynomial",
  parameters = list("0" = list("Cyan" = c(0.0638 - 0.101))),
  notes = "Documented in 10.1016/j.agrformet.2024.110020" ,
  contact_information = "gastonmaurodiaz@gmail.com"
)

export_registry_snapshot(my_entry, "raw", "cygm", "simple_method", "simple_method",
                         file = "inst/external/my_entry_v2.yaml")
