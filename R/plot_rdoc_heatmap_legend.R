#' Plot Standalone Term Legend for Heatmap Annotations
#'
#' Builds a standalone term-color legend using the same term annotation colors
#' as [rdoc_compare_heatplot()]. Labels are drawn below the tiles.
#'
#' @param corr_df Data frame (or list of data frames) containing at least
#'   columns `Domain` and `Term`. If a list is provided, the first element is used
#'   as legend reference.
#' @param domain_palette Palette name from [rdoc_available_palettes()].
#' @param domain_gap Horizontal gap inserted between domains.
#' @param tile_height Height of each legend tile.
#' @param label_offset Vertical offset between tiles and bottom labels.
#' @param label_angle Label angle in degrees.
#' @param label_size Label text size.
#' @param label_wrap_width Target width for multi-line term labels.
#' @param expand_term_abbreviations Logical; expand abbreviations in labels:
#'   `RL` to Reinforcement Learning, `CC` to Cognitive Control,
#'   and `WM` to Working Memory.
#' @param outline_color Border color for legend tiles.
#' @param outline_linewidth Border line width for legend tiles.
#' @param output_file Optional output path. If provided, plot is saved with
#'   [ggplot2::ggsave()].
#' @param width,height Figure size in inches when `output_file` is provided.
#' @param dpi Resolution when `output_file` is provided.
#'
#' @return A ggplot object.
#' @examples
#' p <- plot_rdoc_heatmap_legend(rdoc_example_data())
#' p
#' @export
plot_rdoc_heatmap_legend <- function(corr_df,
                                     domain_palette = "Accent",
                                     domain_gap = 0.55,
                                     tile_height = 0.42,
                                     label_offset = 0.3,
                                     label_angle = 45,
                                     label_size = 2.7,
                                     label_wrap_width = 18,
                                     expand_term_abbreviations = TRUE,
                                     outline_color = "grey45",
                                     outline_linewidth = 0.7,
                                     output_file = NULL,
                                     width = 12,
                                     height = 2.8,
                                     dpi = 300) {
  if (is.list(corr_df) && !is.data.frame(corr_df)) {
    if (length(corr_df) == 0L) {
      stop("`corr_df` list is empty.", call. = FALSE)
    }
    corr_df <- corr_df[[1]]
  }

  if (!is.data.frame(corr_df)) {
    stop("`corr_df` must be a data frame or a list of data frames.", call. = FALSE)
  }

  required_cols <- c("Domain", "Term")
  missing_cols <- setdiff(required_cols, names(corr_df))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("`corr_df` is missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  legend_df <- unique(corr_df[, c("Domain", "Term"), drop = FALSE])
  legend_df$Domain <- factor(legend_df$Domain, levels = unique(as.character(legend_df$Domain)))
  legend_df$Term <- factor(legend_df$Term, levels = unique(as.character(legend_df$Term)))
  legend_df <- legend_df[order(legend_df$Domain, legend_df$Term), , drop = FALSE]

  domain_levels <- levels(legend_df$Domain)
  term_levels <- levels(legend_df$Term)

  domain_cols <- stats::setNames(
    rdoc_get_domain_colors(domain_palette, length(domain_levels)),
    domain_levels
  )

  term_cols <- stats::setNames(rep(NA_character_, length(term_levels)), term_levels)
  for (dom in domain_levels) {
    terms_dom <- as.character(legend_df$Term[legend_df$Domain == dom])
    base_col <- unname(domain_cols[[dom]])
    term_cols[terms_dom] <- grDevices::colorRampPalette(
      c(colorspace::lighten(base_col, amount = 0.7), base_col)
    )(length(terms_dom))
  }

  legend_df$term_id <- seq_len(nrow(legend_df))
  legend_df$domain_id <- match(as.character(legend_df$Domain), domain_levels)
  legend_df$x <- legend_df$term_id + (legend_df$domain_id - 1) * domain_gap
  legend_df$y <- 1

  label_vals <- as.character(legend_df$Term)
  if (isTRUE(expand_term_abbreviations)) {
    label_vals <- rdoc_expand_label_abbreviations(label_vals)
  }
  legend_df$term_label <- vapply(
    label_vals,
    rdoc_make_two_line_label,
    character(1),
    wrap_width = label_wrap_width
  )

  hjust_val <- if (isTRUE(abs(label_angle) > 1e-8)) 1 else 0.5
  vjust_val <- if (isTRUE(abs(label_angle) > 1e-8)) 1 else 0.5
  label_y <- 1 - tile_height / 2 - label_offset

  p <- ggplot2::ggplot(legend_df) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(x = x, y = y, fill = Term),
      width = 0.95,
      height = tile_height,
      colour = outline_color,
      linewidth = outline_linewidth,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = term_cols) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(x = x, y = label_y, label = term_label),
      angle = label_angle,
      hjust = hjust_val,
      vjust = vjust_val,
      size = label_size,
      lineheight = 0.9
    ) +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      labels = NULL,
      limits = c(min(legend_df$x) - 0.6, max(legend_df$x) + 0.6),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      labels = NULL,
      limits = c(label_y - 1.2, 1 + tile_height / 2 + 0.2),
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(
      filename = output_file,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  }

  p
}
