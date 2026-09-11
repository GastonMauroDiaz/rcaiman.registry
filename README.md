
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcaiman.registry

    #> [1] "\\ifelse{html}{\\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\\strong{[Stable]}}"

`rcaiman.registry` provides the infrastructure used by `rcaiman` to
describe imaging systems used in canopy photography.

The package implements a structured registry where instrument
characteristics and interpretation rules are declared through
specifications combined into registry entries.

The package also provides tools to export and read **registry
snapshots** (YAML files) that capture a portable subset of a registry
entry.

## Installation

You can install the development version of rcaiman.registry like so:

``` r
# install.packages("devtools")
devtools::install_github("GastonMauroDiaz/rcaiman.registry")
```

## Registry entries

A registry entry represents a complete technical description of an
imaging system. It typically combines:

- an `embedded_metadata_sig` describing how metadata is detected in
  files
- a `geometry_spec` defining the directional reference frame of the
  image
- a `radiometry_spec` defining how pixel values should be interpreted

## How to create and navigate a registry entry

The package provides helper functions that allow you to build a valid
registry entry step by step:

``` r
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
  id = "exif_01",
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
  embedded_metadata_sig = "exif_01",
  id = "simple_method",
  model = "radial_projection",
  parameters = c(0.6380,  0.0307, -0.0200),
  snapped_zenith_coord = c(645, 494),
  horizon_radius = 378,
  is_horizon_circle_clipped = FALSE,
  max_zenith_angle = 94.8,
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)

foo <- add_radiometry_spec(
  foo,
  embedded_metadata_sig = "exif_01",
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
  embedded_metadata_sig = "exif_01",
  geometry_spec = "simple_method",
  id = "simple_method",
  type = "flat_field_correction",
  model = "ff_simple_polynomial",
  parameters = list("5.0" = c(0.0638, -0.101)),
  notes = "Calibration documented in doi:10.1016/j.agrformet.2024.110020",
  contact_information = "gastonmaurodiaz@gmail.com"
)
```

A registry entry available in memory can be quickly inspected to
understand which instrument it describes:

``` r
foo
#> <Registry entry>
#> 
#> Instrument system
#> -----------------
#> ID:            Nikon_Coolpix5700.FCE9.CIEFAP
#> Configuration: Camera body + auxiliary lens
#> Institution:   CIEFAP
#> Camera body:
#>   Model:        E5700
#>   Manufacturer: NIKON CORP
#>   Serial:       7053067
#> Lens:
#>   Model:        Zoom Nikkor ED 8.9-71.2mm 1:2.8-4.2
#>   Manufacturer: NIKON CORP
#> Auxiliary lens:
#>   Model:        Fisheye Converter FC-E9 0.2x
#>   Manufacturer: NIKON CORP
#> 
#> Registry contents
#> -----------------
#> Embedded metadata signatures: 1
#> 
#> * exif_01
#>     Geometry specifications: 1
#>       - simple_method (radiometry: 2)
#> 
#> Total geometry specifications: 1
#> Total radiometry specifications: 2
```

`summary()` can be used to list the specification identifiers.

``` r
summary(foo)
#> 
#> Registry entry - Summary
#> ========================
#> 
#> Embedded metadata signatures (1)
#> ---------------------------------
#> * exif_01
#>     Dimensions:
#>       - 1288 x 962
#>     Geometry specifications (1):
#>       - simple_method
#>           Radiometry specifications (2):
#>             * simple_method
#>             * spectral_bands
```

The `get_*()` helpers allow specifications to be retrieved easily for
use in scripts that interact with `rcaiman`.

``` r
ems <- get_embedded_metadata_sig(foo, "exif_01")
spec <- get_geometry_spec(foo, "exif_01", "simple_method")
spec <- get_geometry_spec(ems, "simple_method")
rspec <- get_radiometry_spec(spec, "spectral_bands")
rspec <- get_radiometry_spec(foo, "exif_01", "simple_method", "spectral_bands")
```

A registry snapshot can be exported as a YAML file to be distributed as
metadata alongside preprocessed images.

``` r
export_registry_snapshot(
  registry_entry = foo,
  embedded_metadata_sig = "exif_01",
  geometry_spec = "simple_method",
  interpretive_constraint = "spectral_bands",
  flat_field_correction = simple_method,
  file = "registry_snapshot.yaml"
)
```

## On calibration

The functions `calibrate_lens()` and `extract_radiometry()` provide
support for implementing the simple calibration method described in:

Díaz, G. M., Lang, M., & Kaha, M. (2024).  
*Simple calibration of fisheye lenses for hemispherical photography of
the forest canopy.*  
Agricultural and Forest Meteorology, 352, 110020.  
<https://doi.org/10.1016/j.agrformet.2024.110020>

## Registry architecture

The registry project can be organised as a small modular suite of
packages:

rcaiman.registry → registry framework

rcaiman.registry.data → registry entries maintained by the
*rcaiman.registry* maintainer

rcaiman.registry.foo → registry entries maintained by individual groups

## Extending the registry

The `rcaiman.registry.<foo>` naming convention improves discoverability
and signals that the package extends the registry project. Replacing
`<foo>` with a lab or project name provides an intuitive naming scheme.

Packages following this pattern would typically depend on
`rcaiman.registry` and contain `hs_registry_entry` objects describing
specific imaging systems. Alternatively, when resources for creating a
full R package are not available, a simple repository containing YAML
snapshot files may be deemed appropriate.

These extension packages are encouraged to distribute:

- the registry entry describing the imaging system and its
  specifications
- raw image data
- the corresponding preprocessed output

Providing pairs of raw and preprocessed data enables testing,
validation, and reproducibility of registry entries.

GitHub repositories provide a convenient distribution channel for these
packages, particularly given the size limitations imposed by CRAN.
