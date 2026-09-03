# 01/07/2026
require(rcaiman.registry)
my_registry <- read_registry_snapshot(system.file("external/registry_snapshot.yaml", package = "rcaiman.registry"))

spec <- get_geometry_spec(my_registry, "exif_01", "simple_method")
z <- zenith_image(spec$horizon_radius*2, spec$parameters)
a <- azimuth_image(z)

r <- read_caim_raw("C:/Users/gdiaz/OneDrive/aa/auto_HSP/nikon_coolpix_5700_plus_fce9/DSCN6089.NEF",
                   cfa_pattern = spec$radiometry$spectral_bands$cfa_pattern)
zenith_col_row <- spec$zenith_col_row
zenith_col_row[2] <- zenith_col_row[2] + 13
upper_left <- zenith_col_row - spec$horizon_radius
side <- spec$horizon_radius * 2
r <- crop_caim(r, upper_left, side, side)

write_caim(normalize_minmax(r)*255, "flatfield_image", 8)
