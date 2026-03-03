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
#' @param correlation_label Label style used for the correlation legend:
#'   `"pearson"` (default, `Pearson r`) or `"spearman"` (`Spearman rho`).
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
                                 correlation_label = c("pearson", "spearman"),
                                 cut_deg = 4,
                                 output_file = NULL,
                                 width = 13,
                                 height = 8.4,
                                 dpi = 300) {
  term_alignment <- match.arg(term_alignment)
  correlation_label <- match.arg(correlation_label)
  legend_label <- rdoc_correlation_legend_label(correlation_label)

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
  row_h <- 0.125
  term_ring_gap <- 0.08
  term_ring_h <- 0.105
  domain_gap <- 0.10
  heat_inner <- 0.80
  n_inner_segments <- 10
  inner_row_h <- 0.05
  inner_gap_to_heat <- 0.03
  heat_to_term_gap <- 0.025
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
    "Arous./ Reg." = "Arous/Reg",
    "Arousal/Regulatory" = "Arous/Reg",
    "Cognitive Systems" = "Cog. Systems",
    "Negative Valence" = "Neg. Val.",
    "Positive Valence Systems" = "Pos. Val.",
    "Systems for Social Processes" = "Social Proc.",
    "Sensorimotor Systems" = "Sensorimotor"
  )
  domain_label_offset_map <- c(
    "Arous./ Reg." = 0.105,
    "Arousal/Regulatory" = 0.105,
    "Cognitive Systems" = 0.105,
    "Negative Valence" = 0.145,
    "Positive Valence Systems" = 0.125,
    "Systems for Social Processes" = 0.095,
    "Sensorimotor Systems" = 0.105
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
    offset <- 0.105
    if (as.character(dom) %in% names(domain_label_offset_map)) {
      offset <- unname(domain_label_offset_map[[as.character(dom)]])
    }

    data.frame(
      Domain = dom,
      Domain_label = label,
      label_offset = offset,
      xmin = map_x(min(ids) - 0.5 + domain_gap / 2),
      xmax = map_x(max(ids) + 0.5 - domain_gap / 2),
      stringsAsFactors = FALSE
    )
  }))

  domain_line_y <- term_ring_outer + 0.03
  domain_line_df <- dplyr::bind_rows(lapply(seq_len(nrow(domain_df)), function(i) {
    data.frame(
      Domain = domain_df$Domain[i],
      x = seq(domain_df$xmin[i], domain_df$xmax[i], length.out = 320),
      y = domain_line_y,
      stringsAsFactors = FALSE
    )
  }))

  domain_text_df <- dplyr::bind_rows(lapply(seq_len(nrow(domain_df)), function(i) {
    data.frame(
      Domain = domain_df$Domain[i],
      Domain_label = domain_df$Domain_label[i],
      x = seq(domain_df$xmax[i], domain_df$xmin[i], length.out = 260),
      y = term_ring_outer + domain_df$label_offset[i],
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
      colour = "grey97",
      linewidth = 0.26
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#2b83ba",
      mid = "#f3eee8",
      high = "#d7301f",
      midpoint = 0,
      limits = c(-1, 1),
      name = legend_label,
      guide = ggplot2::guide_colorbar(
        barheight = grid::unit(56, "pt"),
        barwidth = grid::unit(17, "pt"),
        title.position = "left",
        frame.colour = "grey35",
        frame.linewidth = 0.35,
        ticks.colour = "grey60"
      )
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
    ggplot2::geom_path(
      data = domain_line_df,
      mapping = ggplot2::aes(x = x, y = y, group = Domain, colour = Domain),
      linewidth = 1.65,
      lineend = "butt",
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = domain_cols) +
    geomtextpath::geom_textpath(
      data = domain_text_df,
      mapping = ggplot2::aes(x = x, y = y, label = Domain_label, group = Domain),
      colour = "black",
      linecolour = NA,
      linewidth = 0,
      size = 2.55,
      fontface = "bold",
      upright = FALSE,
      text_smoothing = 32,
      vjust = 0.5,
      show.legend = FALSE
    ) +
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
    ggplot2::scale_x_continuous(
      limits = c(0.5, x_total + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::coord_polar(theta = "x", start = -pi / 2, direction = -1, clip = "off") +
    ggplot2::ylim(min(inner_df$ymin) - 0.04, term_ring_outer + 0.26) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 11, angle = 90),
      legend.text = ggplot2::element_text(size = 11),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(6, 16, 6, 6)
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
                                         correlation_label = c("pearson", "spearman"),
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
    correlation_label = correlation_label,
    cut_deg = cut_deg,
    output_file = output_file,
    width = width,
    height = height,
    dpi = dpi
  )
}
