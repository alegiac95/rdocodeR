#' List Available Domain Palettes
#'
#' Returns palette names supported by `rdocodeR`.
#'
#' @return Character vector with available palette names.
#' @examples
#' head(rdoc_available_palettes())
#' @export
rdoc_available_palettes <- function() {
  base::sort(unique(c(
    rdoc_met_palettes(),
    rownames(RColorBrewer::brewer.pal.info),
    rdoc_viridis_palettes()
  )))
}

rdoc_met_palettes <- function() {
  c(
    "Archambault", "Austria", "Benedictus", "Cassatt1", "Cassatt2", "Cross", "Degas",
    "Demuth", "Derain", "Egypt", "Gauguin", "Greek", "Hiroshige", "Hokusai1",
    "Hokusai2", "Hokusai3", "Homer1", "Homer2", "Ingres", "Isfahan1", "Isfahan2",
    "Java", "Johnson", "Juarez", "Kandinsky", "Klimt", "Lakota", "Manet", "Monet",
    "Moreau", "Morgenstern", "Nattier", "Navajo", "NewKingdom", "Nizami",
    "OKeeffe1", "OKeeffe2", "Paquin", "Peru1", "Peru2", "Pillement", "Pissaro",
    "Redon", "Renoir", "Signac", "Tam", "Tara", "Thomas", "Tiepolo", "Troy",
    "Tsimshian", "VanGogh1", "VanGogh2", "VanGogh3", "Veronese", "Wissing"
  )
}

rdoc_viridis_palettes <- function() {
  c(
    "A", "B", "C", "D", "E", "F", "G", "H",
    "viridis", "magma", "plasma", "inferno", "cividis",
    "mako", "rocket", "turbo"
  )
}

rdoc_get_domain_colors <- function(palette_name, n) {
  if (!is.character(palette_name) || length(palette_name) != 1L) {
    stop("`domain_palette` must be a single palette name.", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1L || n < 1L) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }
  n <- as.integer(n)

  met_palettes <- rdoc_met_palettes()
  viridis_palettes <- rdoc_viridis_palettes()
  rcb_palettes <- rownames(RColorBrewer::brewer.pal.info)

  if (palette_name %in% met_palettes) {
    return(MetBrewer::met.brewer(palette_name, n))
  }

  if (palette_name %in% viridis_palettes) {
    return(viridisLite::viridis(n, option = palette_name))
  }

  if (palette_name %in% rcb_palettes) {
    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    n_use <- min(n, max_colors)
    cols <- RColorBrewer::brewer.pal(n_use, palette_name)
    if (n > max_colors) {
      cols <- grDevices::colorRampPalette(cols)(n)
    }
    return(cols)
  }

  stop(
    sprintf(
      "Unknown palette '%s'. Use `rdoc_available_palettes()` to inspect valid options.",
      palette_name
    ),
    call. = FALSE
  )
}

rdoc_contrast_color <- function(hex_color) {
  cols <- as.character(hex_color)
  rgb <- grDevices::col2rgb(cols) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  out <- ifelse(lum > 0.6, "black", "white")
  unname(out)
}
