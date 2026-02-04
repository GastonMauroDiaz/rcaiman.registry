## code to prepare `Canon_EOS_2000D.Sigma_4.5mm.UEF` dataset goes here

Canon_EOS_2000D.Sigma_4.5mm.UEF <- new_registry(
  id = "Canon_EOS_2000D.Sigma_4.5mm.UEF",
  body = "EOS 2000D",
  body_manufacturer = "Canon Inc",
  lens = "Sigma 4.5mm F2.8 EX",
  lens_manufacturer = "Sigma Corp",
  institution = "University of Eastern Finland"
)

usethis::use_data(Canon_EOS_2000D.Sigma_4.5mm.UEF, overwrite = TRUE)
