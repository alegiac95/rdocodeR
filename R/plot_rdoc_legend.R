#' Plot Term Legend for Circular RDoC Plot
#'
#' Builds a standalone legend plot for RDoC terms using the same term colors as [plot_rdoc_gg()].
#'
#' @param corr_df Data frame with columns `Domain` and `Term`.
#' @param domain_palette Palette name from [rdoc_available_palettes()].
#' @param ncol_legend Number of legend columns.
#' @param output_file Optional output path. If provided, plot is saved with [ggplot2::ggsave()].
#' @param width,height Figure size in inches when `output_file` is provided.
#' @param dpi Resolution when `output_file` is provided.
#' @param label_inside Logical; place term labels inside the colored legend bars.
#' @param legend_label_size Text size for legend labels.
#' @param legend_border_color Border color for legend bars.
#' @param expand_term_abbreviations Logical; expand abbreviations in legend labels:
#'   `RL` to Reinforcement Learning, `CC` to Cognitive Control,
#'   and `WM` to Working Memory.
#' @param term_label_wrap_width Target width for two-line legend label wrapping.
#'
#' @return A ggplot object.
#' @examples
#' df <- rdoc_example_data()
#' p <- plot_rdoc_legend(df, domain_palette = "Accent", ncol_legend = 4)
#' p
#' @export
plot_rdoc_legend <- function(corr_df,
                             domain_palette = "Accent",
                             ncol_legend = 3,
                             output_file = NULL,
                             width = 6,
                             height = 4,
                             dpi = 300,
                             label_inside = TRUE,
                             legend_label_size = 2.3,
                             legend_border_color = "black",
                             expand_term_abbreviations = TRUE,
                             term_label_wrap_width = 24) {
  if (!is.data.frame(corr_df)) {
    stop("`corr_df` must be a data frame.", call. = FALSE)
  }

  required_cols <- c("Domain", "Term")
  missing_cols <- setdiff(required_cols, names(corr_df))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("`corr_df` is missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  corr_df$Domain <- factor(corr_df$Domain, levels = unique(as.character(corr_df$Domain)))
  corr_df$Term <- factor(corr_df$Term, levels = unique(as.character(corr_df$Term)))
  corr_df <- corr_df[order(corr_df$Domain, corr_df$Term), , drop = FALSE]
  legend_df <- unique(corr_df[, c("Domain", "Term"), drop = FALSE])

  n_domains <- nlevels(legend_df$Domain)
  domain_cols <- stats::setNames(
    rdoc_get_domain_colors(domain_palette, n_domains),
    levels(legend_df$Domain)
  )

  term_levels <- levels(legend_df$Term)
  term_cols <- stats::setNames(rep(NA_character_, length(term_levels)), term_levels)

  for (dom in levels(legend_df$Domain)) {
    terms_dom <- unique(as.character(legend_df$Term[legend_df$Domain == dom]))
    base_col <- unname(domain_cols[[dom]])
    term_cols[terms_dom] <- grDevices::colorRampPalette(
      c(colorspace::lighten(base_col, amount = 0.7), base_col)
    )(length(terms_dom))
  }

  legend_df <- legend_df[order(legend_df$Domain, legend_df$Term), , drop = FALSE]
  legend_df$idx <- seq_len(nrow(legend_df))
  legend_df$col <- (legend_df$idx - 1L) %% ncol_legend + 1L
  legend_df$row <- (legend_df$idx - 1L) %/% ncol_legend + 1L
  legend_labels <- as.character(legend_df$Term)
  if (isTRUE(expand_term_abbreviations)) {
    legend_labels <- rdoc_expand_label_abbreviations(legend_labels)
  }
  legend_df$legend_label <- vapply(
    legend_labels,
    rdoc_make_two_line_label,
    character(1),
    wrap_width = term_label_wrap_width
  )
  legend_df$label_col <- rdoc_contrast_color(term_cols[as.character(legend_df$Term)])

  max_row <- max(legend_df$row)

  p <- ggplot2::ggplot(legend_df, ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = Term),
      width = if (isTRUE(label_inside)) 0.95 else 0.4,
      height = 0.8,
      colour = legend_border_color,
      linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(values = term_cols, guide = "none") +
    {
      if (isTRUE(label_inside)) {
        ggplot2::geom_text(
          mapping = ggplot2::aes(label = legend_label, colour = label_col),
          hjust = 0.5,
          vjust = 0.5,
          size = legend_label_size,
          lineheight = 0.9
        )
      } else {
        ggplot2::geom_text(
          mapping = ggplot2::aes(x = col + 0.45, label = legend_label),
          hjust = 0,
          size = legend_label_size
        )
      }
    } +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::scale_y_reverse(
      limits = c(max_row + 0.5, 0.5),
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    ) +
    ggplot2::scale_x_continuous(
      limits = if (isTRUE(label_inside)) c(0.5, ncol_legend + 0.5) else c(0.5, ncol_legend + 1.8),
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 20, 10, 10))

  if (!is.null(output_file)) {
    ggplot2::ggsave(
      filename = output_file,
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  p
}

#' Backward-compatible Alias for [plot_rdoc_legend()]
#'
#' @inheritParams plot_rdoc_legend
#' @param ... Additional arguments passed to [plot_rdoc_legend()].
#' @return A ggplot object.
#' @export
plot_rdoc_construct_legend <- function(corr_df, ...) {
  plot_rdoc_legend(corr_df = corr_df, ...)
}
