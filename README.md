# rdocodeR

<img src="man/figures/logo.png" align="right" height="220" alt="rdocodeR hex logo" />

`rdocodeR` wraps your RDoC workflow into an installable R package and uses the
`ggplot2` circular plot as the default plotting backend.

## Install locally

```r
install.packages("devtools")
devtools::install("/Users/alessio/rdocodeR")
```

## Quick start (plot existing TSV results)

```r
library(rdocodeR)

df <- rdoc_example_data()
p <- plot_rdoc_gg(df, domain_palette = "Accent")
p
```

## Decode from an overlay (returns data frame)

```r
library(rdocodeR)

res <- rdoc_decode(
  fs_overlay = your_overlay,
  perm_method = "eigen",  # default
  cor_method = "pearson"  # default
)

# optional save
rdoc_decode(
  fs_overlay = your_overlay,
  perm_method = "eigen",
  cor_method = "pearson",
  save_results = TRUE,
  results_file = "~/Desktop/rdoc_decode_results.tsv"
)
```

Use `rdoc_available_palettes()` to inspect supported palette names.

## Logo

The package hex logo is included at:

- `man/figures/logo.png` (documentation/README rendering)
- `inst/hex/rdocodeR_hex_logo.png` (installed package asset)
