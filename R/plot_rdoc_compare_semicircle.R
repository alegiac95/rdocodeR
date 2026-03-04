#' Plot RDoC Comparison as a Semi-Circular Tile Map
#'
#' Builds a multi-condition semi-circular comparison plot with:
#' correlation tiles (one ring per condition), a term annotation ring,
#' domain outline arcs, and inner decorative grey segmented rings.
#'
#' @param corr_list List of correlation data frames, each with columns `Domain`,
#'   `Term`, `r`, and optional `p`.
#' @param term_alignment How terms are aligned across conditions:
#'   `"union"` (default) keeps all terms, `"intersection"` keeps only shared terms,
#'   and `"strict"` requires identical term sets.
#' @param domain_palette Palette name from [rdoc_available_palettes()].
#' @param significance_level Threshold used for significance stars (`p < threshold`).
#' @param show_significance_stars Logical; draw `*` on significant cells.
#' @param show_significant_term_labels Logical; when `TRUE`, show labels for
#'   terms with at least one significant result (`p < significance_level`)
#'   outside the fan, similar to the circular plot.
#' @param correlation_label Label style used for the correlation legend:
#'   `"pearson"` (default, `Pearson r`) or `"spearman"` (`Spearman rho`).
#' @param show_correlation_legend Logical; show correlation color legend.
#'   When `TRUE` (default), the legend is placed in the lower-right corner.
#' @param cut_deg Trim angle (degrees) removed from each end of the semicircle.
#' @param output_file Optional output path. If provided, plot is saved with
#'   [ggplot2::ggsave()].
#' @param width,height Figure size in inches when `output_file` is provided.
#' @param dpi Resolution when `output_file` is provided.
#'
#' @return A ggplot object.
#' @examples
#' df1 <- rdoc_example_data()
#' df2 <- df1
#' set.seed(1)
#' df2$r <- pmax(pmin(df2$r + stats::rnorm(nrow(df2), sd = 0.1), 1), -1)
#' p <- rdoc_compare_fanplot(list(Sample_A = df1, Sample_B = df2))
#' p
#' @export
rdoc_compare_fanplot <- function(corr_list,
                                 term_alignment = c("union", "strict", "intersection"),
                                 domain_palette = "Accent",
                                 significance_level = 0.05,
                                 show_significance_stars = TRUE,
                                 show_significant_term_labels = FALSE,
                                 correlation_label = c("pearson", "spearman"),
                                 show_correlation_legend = TRUE,
                                 cut_deg = 4,
                                 output_file = NULL,
                                 width = 13,
                                 height = 8.4,
                                 dpi = 300) {
  term_alignment <- match.arg(term_alignment)
  correlation_label <- match.arg(correlation_label)

  validated <- rdoc_validate_compare_list(corr_list, term_alignment = term_alignment)
  corr_data <- validated$data
  cond_names <- validated$condition_names
  term_levels <- validated$terms
  domain_levels <- validated$domain_levels
  domain_by_term <- validated$domain_by_term

  n_terms <- length(term_levels)
  n_conditions <- length(cond_names)

  # Geometry tuned to the accepted mock style.
  corr_term_gap <- 0.02
  row_h <- 0.165
  term_ring_gap <- 0.08
  term_ring_h <- 0.128
  domain_gap <- 0.10
  heat_inner <- 0.90
  n_inner_segments <- 10
  inner_row_h <- 0.055
  inner_gap_to_heat <- 0.02
  heat_to_term_gap <- 0.018
  inner_tile_gap <- 0.18
  inner_tile_height_scale <- 0.84

  domain_cols <- stats::setNames(
    rdoc_get_domain_colors(domain_palette, length(domain_levels)),
    domain_levels
  )

  term_cols <- stats::setNames(rep(NA_character_, n_terms), term_levels)
  for (dom in domain_levels) {
    terms_dom <- term_levels[domain_by_term == dom]
    base_col <- unname(domain_cols[[dom]])
    term_cols[terms_dom] <- grDevices::colorRampPalette(
      c(colorspace::lighten(base_col, amount = 0.7), base_col)
    )(length(terms_dom))
  }

  domain_label_map <- c(
    "AR" = "Ar/Reg",
    "Arous./ Reg." = "Ar/Reg",
    "Arousal/Regulatory" = "Ar/Reg",
    "CS" = "Cognitive Systems",
    "Cognitive Systems" = "Cognitive Systems",
    "NV" = "Neg. Val.",
    "Negative Valence" = "Neg. Val.",
    "PV" = "Pos. Valence",
    "Positive Valence Systems" = "Pos. Valence",
    "SP" = "Social Proc.",
    "Systems for Social Processes" = "Social Proc.",
    "SS" = "Sensorimotor",
    "Sensorimotor Systems" = "Sensorimotor"
  )

  x_total <- 2 * n_terms
  scale_fac <- (180 - 2 * cut_deg) / 180
  x_offset <- ((1 - scale_fac) / 2) * n_terms
  map_x <- function(x) {
    0.5 + x_offset + (x - 0.5) * scale_fac
  }

  heat_df <- dplyr::bind_rows(lapply(seq_along(corr_data), function(i) {
    row_idx <- n_conditions - i + 1
    ymin <- heat_inner + (row_idx - 1) * row_h
    ymax <- ymin + row_h * 0.985

    data.frame(
      Condition = cond_names[[i]],
      Domain = domain_by_term,
      Term = term_levels,
      x = map_x(seq_len(n_terms)),
      xmin = map_x(seq_len(n_terms) - 0.5 + corr_term_gap / 2),
      xmax = map_x(seq_len(n_terms) + 0.5 - corr_term_gap / 2),
      ymin = ymin,
      ymax = ymax,
      r = corr_data[[i]]$r,
      p = corr_data[[i]]$p,
      stringsAsFactors = FALSE
    )
  }))
  heat_df$sig <- !is.na(heat_df$p) & heat_df$p < significance_level

  heat_outer <- heat_inner + n_conditions * row_h
  inner_outer <- heat_inner - inner_gap_to_heat
  inner_df <- dplyr::bind_rows(lapply(seq_len(n_inner_segments), function(i) {
    ymax <- inner_outer - (i - 1) * inner_row_h
    ymin <- ymax - inner_row_h * 0.92
    data.frame(
      layer = i,
      xmin = map_x(seq_len(n_terms) - 0.5 + inner_tile_gap / 2),
      xmax = map_x(seq_len(n_terms) + 0.5 - inner_tile_gap / 2),
      ymin = ymin,
      ymax = ymax
    )
  }))
  inner_df <- dplyr::mutate(
    inner_df,
    ymid = (ymin + ymax) / 2,
    yhalf = (ymax - ymin) * (inner_tile_height_scale / 2),
    ymin = ymid - yhalf,
    ymax = ymid + yhalf,
    shade = grDevices::gray(seq(0.76, 0.88, length.out = n_inner_segments)[layer])
  )
  inner_df <- dplyr::select(inner_df, xmin, xmax, ymin, ymax, shade)

  term_ring_inner <- heat_outer + heat_to_term_gap
  term_ring_outer <- term_ring_inner + term_ring_h

  term_ring_df <- data.frame(
    Term = term_levels,
    Domain = domain_by_term,
    xmin = map_x(seq_len(n_terms) - 0.5 + term_ring_gap / 2),
    xmax = map_x(seq_len(n_terms) + 0.5 - term_ring_gap / 2),
    ymin = term_ring_inner,
    ymax = term_ring_outer,
    stringsAsFactors = FALSE
  )

  domain_df <- dplyr::bind_rows(lapply(domain_levels, function(dom) {
    ids <- which(domain_by_term == dom)
    label <- as.character(dom)
    if (label %in% names(domain_label_map)) {
      label <- unname(domain_label_map[[label]])
    }

    data.frame(
      Domain = dom,
      Domain_label = label,
      xmin = map_x(min(ids) - 0.5 + domain_gap / 2),
      xmax = map_x(max(ids) + 0.5 - domain_gap / 2),
      stringsAsFactors = FALSE
    )
  }))
  domain_ring_inner <- term_ring_outer + 0.02
  domain_ring_outer <- domain_ring_inner + 0.132
  domain_ring_df <- domain_df
  domain_ring_df$ymin <- domain_ring_inner
  domain_ring_df$ymax <- domain_ring_outer
  domain_ring_df$text_col <- rdoc_contrast_color(domain_cols[domain_ring_df$Domain])

  domain_text_df <- dplyr::bind_rows(lapply(seq_len(nrow(domain_ring_df)), function(i) {
    x_seq <- seq(domain_ring_df$xmin[i], domain_ring_df$xmax[i], length.out = 260)
    if (identical(as.character(domain_ring_df$Domain_label[i]), "Ar/Reg")) {
      x_seq <- rev(x_seq)
    }
    data.frame(
      Domain = domain_ring_df$Domain[i],
      Domain_label = domain_ring_df$Domain_label[i],
      x = x_seq,
      y = (domain_ring_df$ymin[i] + domain_ring_df$ymax[i]) / 2,
      text_col = domain_ring_df$text_col[i],
      stringsAsFactors = FALSE
    )
  }))

  star_df <- heat_df[heat_df$sig, , drop = FALSE]
  star_df$star_y <- (star_df$ymin + star_df$ymax) / 2

  x_left_edge <- map_x(n_terms + 0.5)
  cond_label_df <- dplyr::group_by(heat_df, Condition)
  cond_label_df <- dplyr::summarise(cond_label_df, y = mean(range(ymin, ymax)), .groups = "drop")
  cond_label_df <- dplyr::mutate(
    cond_label_df,
    y_max = max(y),
    x = x_left_edge + 1.78 + (y_max - y) * 1.40,
    angle = 66
  )
  cond_label_df <- dplyr::select(cond_label_df, -y_max)

  sig_terms <- unique(heat_df$Term[heat_df$sig])
  term_label_df <- term_ring_df[term_ring_df$Term %in% sig_terms, , drop = FALSE]
  if (nrow(term_label_df) > 0) {
    term_label_df$id_num <- match(term_label_df$Term, term_levels)
    term_label_df$label <- vapply(
      rdoc_expand_label_abbreviations(as.character(term_label_df$Term)),
      rdoc_make_two_line_label,
      character(1),
      wrap_width = 28
    )
    term_label_df$y <- domain_ring_outer + 0.11
    term_label_df$angle <- 90 - (180 - 2 * cut_deg) * (term_label_df$id_num - 0.5) / n_terms - cut_deg
    term_label_df$hjust <- ifelse(term_label_df$angle < -90, 1, 0)
    term_label_df$angle <- ifelse(term_label_df$angle < -90, term_label_df$angle + 180, term_label_df$angle)
  }
  y_upper <- domain_ring_outer + if (isTRUE(show_significant_term_labels) && nrow(term_label_df) > 0) 0.21 else 0.07
  legend_position_value <- if (isTRUE(show_correlation_legend)) c(0.98, 0.03) else "none"

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = inner_df,
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = shade),
      colour = NA,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_identity() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(
      data = heat_df,
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = r),
      colour = "grey45",
      linewidth = 0.26
    ) +
    rdoc_correlation_scale(
      correlation_label = correlation_label,
      na_fill = "grey90",
      barheight_pt = 58,
      barwidth_pt = 16
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(
      data = term_ring_df,
      mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Term),
      colour = "grey82",
      linewidth = 0.32,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = term_cols) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(
      data = domain_ring_df,
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax,
        fill = Domain
      ),
      colour = "grey70",
      linewidth = 0.35,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = domain_cols) +
    geomtextpath::geom_textpath(
      data = domain_text_df,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        label = Domain_label,
        group = Domain,
        colour = text_col
      ),
      linecolour = NA,
      linewidth = 0,
      size = 3.35,
      fontface = "bold",
      upright = TRUE,
      text_smoothing = 30,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_identity() +
    {
      if (isTRUE(show_significance_stars)) {
        ggplot2::geom_text(
          data = star_df,
          mapping = ggplot2::aes(x = x, y = star_y),
          label = "*",
          size = 2.55,
          fontface = "bold"
        )
      } else {
        NULL
      }
    } +
    ggplot2::geom_text(
      data = cond_label_df,
      mapping = ggplot2::aes(x = x, y = y, label = Condition, angle = angle),
      hjust = 0.5,
      vjust = 0.5,
      size = 2.55,
      fontface = "bold",
      colour = "black"
    ) +
    {
      if (isTRUE(show_significant_term_labels) && nrow(term_label_df) > 0) {
        ggplot2::geom_text(
          data = term_label_df,
          mapping = ggplot2::aes(
            x = (xmin + xmax) / 2,
            y = y,
            label = label,
            angle = angle,
            hjust = hjust
          ),
          size = 2.5,
          lineheight = 0.9,
          colour = "black",
          fontface = "bold",
          vjust = 0.5
        )
      } else {
        NULL
      }
    } +
    ggplot2::scale_x_continuous(
      limits = c(0.5, x_total + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = -1, clip = "off") +
    ggplot2::ylim(min(inner_df$ymin) - 0.02, y_upper) +
    ggplot2::theme_void() +
    rdoc_correlation_legend_theme(title_size = 11, text_size = 11) +
    ggplot2::theme(
      legend.position = legend_position_value,
      legend.justification = c(1, 0),
      plot.margin = ggplot2::margin(36, 16, 4, 6)
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

#' Backward-Compatible Alias for [rdoc_compare_fanplot()]
#'
#' @inheritParams rdoc_compare_fanplot
#' @return A ggplot object.
#' @export
plot_rdoc_compare_semicircle <- function(corr_list,
                                         term_alignment = c("union", "strict", "intersection"),
                                         domain_palette = "Accent",
                                         significance_level = 0.05,
                                         show_significance_stars = TRUE,
                                         show_significant_term_labels = FALSE,
                                         correlation_label = c("pearson", "spearman"),
                                         show_correlation_legend = TRUE,
                                         cut_deg = 4,
                                         output_file = NULL,
                                         width = 13,
                                         height = 8.4,
                                         dpi = 300) {
  rdoc_compare_fanplot(
    corr_list = corr_list,
    term_alignment = term_alignment,
    domain_palette = domain_palette,
    significance_level = significance_level,
    show_significance_stars = show_significance_stars,
    show_significant_term_labels = show_significant_term_labels,
    correlation_label = correlation_label,
    show_correlation_legend = show_correlation_legend,
    cut_deg = cut_deg,
    output_file = output_file,
    width = width,
    height = height,
    dpi = dpi
  )
}
