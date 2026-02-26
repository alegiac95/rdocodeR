# rdocodeR

<p align="center">
  <img src="man/figures/logo.png" height="260" alt="rdocodeR hex logo" />
</p>

`rdocodeR` is an R package for **RDoC-based** decoding and visualization workflows.

## What Is RDoC?

The **Research Domain Criteria (RDoC)** framework (NIMH) organizes mental function
across core domains (for example cognitive systems, valence systems, and arousal/regulatory systems)
instead of relying only on symptom-based diagnostic categories.

## Package Overview

`rdocodeR` provides tools to:

- decode an input brain overlay against bundled RDoC term maps (`rdoc_decode()`)
- save decoding results as a TSV table
- generate publication-style circular RDoC plots with `ggplot2` (`plot_rdoc_gg()`)
- customize labels, significance highlighting, palettes, and legends

## Install

```r
install.packages("devtools")
devtools::install_github("alegiac95/rdocodeR")
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
