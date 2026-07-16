rdoc_correlation_legend_label <- function(correlation_label = c("pearson", "spearman")) {
  correlation_label <- match.arg(correlation_label)
  if (identical(correlation_label, "spearman")) {
    return(expression("Spearman " * rho))
  }
  "Pearson r"
}

rdoc_correlation_colors <- function() {
  c(
    low = "#2b83ba",
    mid = "#f3eee8",
    high = "#d7301f"
  )
}

rdoc_correlation_limits <- function(corr_data) {
  corr_list <- if (is.data.frame(corr_data)) list(corr_data) else corr_data
  r_values <- unlist(lapply(corr_list, function(x) x$r), use.names = FALSE)
  has_negative <- any(r_values < 0, na.rm = TRUE)
  has_positive <- any(r_values > 0, na.rm = TRUE)

  if (has_negative && has_positive) c(-1, 1) else if (has_negative) c(-1, 0) else c(0, 1)
}

rdoc_correlation_legend_guide <- function(legend_label,
                                          barheight_pt = 58,
                                          barwidth_pt = 16) {
  ggplot2::guide_colourbar(
    title = legend_label,
    frame.colour = "black",
    frame.linewidth = 0.6,
    ticks.colour = "black",
    title.position = "left",
    label.position = "right",
    title.theme = ggplot2::element_text(
      angle = 90,
      colour = "black",
      hjust = 0.5,
      vjust = 0.5,
      face = "bold"
    ),
    label.theme = ggplot2::element_text(colour = "black"),
    direction = "vertical",
    barheight = grid::unit(barheight_pt, "pt"),
    barwidth = grid::unit(barwidth_pt, "pt")
  )
}

rdoc_correlation_scale <- function(correlation_label = c("pearson", "spearman"),
                                   na_fill = "grey90",
                                   barheight_pt = 58,
                                   barwidth_pt = 16,
                                   limits = c(-1, 1)) {
  correlation_label <- match.arg(correlation_label)
  legend_label <- rdoc_correlation_legend_label(correlation_label)
  cols <- rdoc_correlation_colors()
  scale_cols <- if (identical(limits, c(0, 1))) cols[c("mid", "high")] else if (identical(limits, c(-1, 0))) cols[c("low", "mid")] else cols
  ggplot2::scale_fill_gradientn(
    colours = unname(scale_cols),
    limits = limits,
    name = legend_label,
    na.value = na_fill,
    guide = rdoc_correlation_legend_guide(
      legend_label = legend_label,
      barheight_pt = barheight_pt,
      barwidth_pt = barwidth_pt
    )
  )
}

rdoc_correlation_legend_theme <- function(title_size = 11, text_size = 11) {
  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(
      angle = 90,
      colour = "black",
      vjust = 0.5,
      hjust = 0.5,
      face = "bold",
      size = title_size
    ),
    legend.text = ggplot2::element_text(colour = "black", size = text_size),
    legend.background = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.6)
  )
}
