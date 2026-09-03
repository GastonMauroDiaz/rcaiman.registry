foo <- new_registry_entry(
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

foo <- add_embedded_metadata_sig(
  foo,
  id = "exif_raw",
  dim = c(1288, 962),
  validated_with_libraw = "0.21.1",
  validated_with_exiftool = "13.59",
  rules = list(
    auto_focus = list(
      group = "Composite",
      tag = "AutoFocus",
      label = "AutoFocus",
      expected = "On"
    ),
    software = list(
      group = "IFD0",
      tag = "Software",
      label = "Software",
      expected = "E5700v1.1"
    ),
    digital_zoom = list(
      group = "Nikon",
      tag = "DigitalZoom",
      label = "DigitalZoom",
      expected = "1"
    ),
    quality = list(
      group = "Nikon",
      tag = "Quality",
      label = "Quality",
      expected = "RAW"
    )
  )
)

foo <- add_geometry_spec(
  foo,
  embedded_metadata_sig = "exif_raw",
  id = "simple_method",
  model = "radial_projection",
  parameters = c(0.6380,  0.0307, -0.0200),
  zenith_col_row = c(645, 494),
  horizon_radius = 378,
  is_horizon_circle_clipped = FALSE,
  max_zenith_angle = 94.8,
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

foo <- add_radiometry_spec(
  foo,
  embedded_metadata_sig = "exif_raw",
  geometry_spec = "simple_method",
  id = "spectral_bands",
  type = "interpretive_constraint",
  cfa_pattern = matrix(c("Green", "Yellow",
                         "Magenta", "Cyan"), byrow = TRUE, ncol = 2),
  spectral_mapping = list(Red = "(Yellow + Magenta)/2",
                          Green = "Green",
                          Blue = "Cyan"),
  contact_information = "gastonmaurodiaz@gmail.com"
)

foo <- add_radiometry_spec(
  foo,
  embedded_metadata_sig = "exif_raw",
  geometry_spec = "simple_method",
  id = "simple_method",
  type = "flat_field_correction",
  model = "ff_simple_polynomial",
  parameters = list("5.0" = c(0.0638, -0.101)),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

export_registry_snapshot(foo,
                         "exif_raw",
                         "simple_method",
                         "spectral_bands",
                         "simple_method",
                         "inst/external/registry_snapshot.yaml")
