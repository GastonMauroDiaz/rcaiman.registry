generate_registry_structure <- function(output) {

  tree <- paste(
    "hs_registry_entry",
    "├─ instrument_metadata (unique, mandatory element)",
    "│  └─ (...)",
    "├─ file signature (1..n, indexed by a user-defined ID)",
    "│  ├─ id",
    "│  └─ (...)",
    "└─ embedded metadata signature (1..n, indexed by a user-defined ID)",
    "   ├─ id",
    "   ├─ file_sig  -> foreign key referencing [file signature]$id",
    "   └─ geometry (named container)",
    "      └─ geometry specification (0..n, indexed by a user-defined ID)",
    "         ├─ id",
    "         └─ radiometry (named container)",
    "            └─ radiometry specification (0..n, indexed by a user-defined ID)",
    "               ├─ id",
    "               └─ (...)",
    sep = "\n")

  ragg::agg_png(
    filename = output,
    width = 2300,
    height = 1000,
    res = 200
  )

  par(mar = c(0,0,0,0))
  plot.new()

  text(
    x = 0,
    y = 1,
    labels = tree,
    adj = c(0,1),
    family = "mono",
    cex = 1.4
  )

  dev.off()
}

generate_registry_structure(
  "man/figures/hs_registry_entry.png"
)
