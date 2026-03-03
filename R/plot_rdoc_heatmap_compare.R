rdoc_validate_compare_list <- function(corr_list, term_alignment = c("union", "strict", "intersection")) {
  term_alignment <- match.arg(term_alignment)

  if (is.data.frame(corr_list)) {
    corr_list <- list(Condition_1 = corr_list)
  }

  if (!is.list(corr_list) || length(corr_list) < 2L) {
    stop("`corr_list` must be a list of at least two correlation data frames.", call. = FALSE)
  }

  cond_names <- names(corr_list)
  if (is.null(cond_names) || anyNA(cond_names) || any(cond_names == "")) {
    cond_names <- paste0("Condition_", seq_along(corr_list))
  }
  if (anyDuplicated(cond_names)) {
    cond_names <- make.unique(cond_names)
  }

  validated <- lapply(corr_list, rdoc_validate_corr_df)

  for (i in seq_along(validated)) {
    validated[[i]] <- validated[[i]][order(validated[[i]]$Domain, validated[[i]]$Term), , drop = FALSE]
  }

  domain_levels <- unique(unlist(lapply(validated, function(df) as.character(df$Domain)), use.names = FALSE))

  # Global term-domain map with consistency checks across conditions.
  all_terms_ordered <- unique(unlist(lapply(validated, function(df) as.character(df$Term)), use.names = FALSE))
  term_to_domain <- stats::setNames(rep(NA_character_, length(all_terms_ordered)), all_terms_ordered)

  for (i in seq_along(validated)) {
    df <- validated[[i]]
    terms_i <- as.character(df$Term)
    dom_i <- as.character(df$Domain)
    for (j in seq_along(terms_i)) {
      term_j <- terms_i[[j]]
      dom_j <- dom_i[[j]]
      current_dom <- term_to_domain[[term_j]]
      if (is.na(current_dom)) {
        term_to_domain[[term_j]] <- dom_j
      } else if (!identical(current_dom, dom_j)) {
        stop(
          sprintf(
            "Term `%s` is assigned to different domains across elements in `corr_list`.",
            term_j
          ),
          call. = FALSE
        )
      }
    }
  }

  terms_by_df <- lapply(validated, function(df) as.character(df$Term))
  ref_terms <- as.character(validated[[1]]$Term)

  if (identical(term_alignment, "strict")) {
    for (i in seq_along(terms_by_df)) {
      if (!setequal(terms_by_df[[i]], ref_terms)) {
        stop(
          sprintf(
            "All elements in `corr_list` must contain the same `Term` values in `strict` mode. Mismatch at element %d.",
            i
          ),
          call. = FALSE
        )
      }
    }
    final_terms <- ref_terms
  } else if (identical(term_alignment, "intersection")) {
    common_terms <- Reduce(intersect, terms_by_df)
    final_terms <- all_terms_ordered[all_terms_ordered %in% common_terms]
    if (length(final_terms) == 0L) {
      stop("No common terms across conditions with `term_alignment = 'intersection'`.", call. = FALSE)
    }
  } else {
    # union
    final_terms <- all_terms_ordered
  }

  # Keep term order aligned to circular plot ordering from the first input
  # (plus any additional terms appended for `union` mode).
  domain_by_term <- unname(term_to_domain[final_terms])
  domain_levels <- unique(domain_by_term)

  aligned <- vector("list", length(validated))
  for (i in seq_along(validated)) {
    df <- validated[[i]]
    terms_i <- as.character(df$Term)
    row_match <- match(final_terms, terms_i)

    out <- data.frame(
      Domain = domain_by_term,
      Term = final_terms,
      r = NA_real_,
      p = NA_real_,
      stringsAsFactors = FALSE
    )

    present <- !is.na(row_match)
    if (any(present)) {
      out$r[present] <- df$r[row_match[present]]
      out$p[present] <- df$p[row_match[present]]
    }

    out$Domain <- factor(out$Domain, levels = domain_levels)
    out$Term <- factor(out$Term, levels = final_terms)
    out$Condition <- cond_names[[i]]
    aligned[[i]] <- out
  }

  list(
    data = aligned,
    condition_names = cond_names,
    terms = final_terms,
    domain_levels = domain_levels,
    domain_by_term = domain_by_term
  )
}

rdoc_short_domain_labels <- function(x) {
  x <- as.character(x)
  domain_map <- c(
    "Arous./ Reg." = "AR",
    "Arousal/Regulatory" = "AR",
    "Cognitive Systems" = "CS",
    "Negative Valence" = "NV",
    "Positive Valence Systems" = "PV",
    "Systems for Social Processes" = "SP",
    "Sensorimotor Systems" = "SS"
  )
  idx <- x %in% names(domain_map)
  x[idx] <- unname(domain_map[x[idx]])
  x
}

rdoc_wrap_multiline <- function(x, width = 22) {
  x <- as.character(x)
  width <- max(1L, as.integer(width))
  vapply(
    x,
    function(lbl) {
      parts <- strwrap(lbl, width = width)
      if (length(parts) == 0L) {
        ""
      } else {
        paste(parts, collapse = "\n")
      }
    },
    character(1)
  )
}

#' Plot RDoC Comparison Heatmap Across Conditions
#'
#' Builds a term-by-condition heatmap from multiple RDoC correlation tables,
#' with two annotation rows that match the circular plot colors:
#' a domain-color row and a term-color row.
#'
#' @param corr_list List of correlation data frames, each with columns `Domain`,
#'   `Term`, `r`, and optional `p`.
#' @param term_alignment How terms are aligned across conditions:
#'   `"union"` (default) keeps all terms, `"intersection"` keeps only shared terms,
#'   and `"strict"` requires identical term sets.
#' @param domain_palette Palette name from [rdoc_available_palettes()].
#' @param significance_level Threshold used for significance stars (`p < threshold`).
#' @param show_significance_stars Logical; draw `*` on significant cells.
#' @param domain_gap Horizontal gap inserted between domains.
#' @param annotation_height Height of the domain/term annotation rows.
#' @param annotation_to_heatmap_gap Vertical gap between annotation rows and
#'   the first condition row.
#' @param compact_small_domain_labels Logical; when `TRUE`, domains with fewer
#'   than `small_domain_threshold` terms use short labels (`AR`, `NV`, `SP`, etc.).
#' @param small_domain_threshold Integer threshold used with
#'   `compact_small_domain_labels`.
#' @param domain_label_wrap_width Wrap width for domain labels.
#' @param show_term_labels Logical; show term labels on the bottom axis.
#' @param show_significant_term_labels Logical; show only terms with at least one
#'   significant result (`p < significance_level`) on the bottom x-axis.
#'   Ignored when `show_term_labels = TRUE`.
#' @param term_label_wrap_width Target wrap width if `show_term_labels = TRUE`.
#' @param significant_term_label_angle Angle (in degrees) used when
#'   `show_significant_term_labels = TRUE`.
#' @param na_fill Fill color for missing term-condition cells (for example when
#'   `term_alignment = "union"` and a term is absent in one condition).
#' @param correlation_label Label style used for the correlation legend:
#'   `"pearson"` (default, `Pearson r`) or `"spearman"` (`Spearman rho`).
#' @param output_file Optional output path. If provided, plot is saved with
#'   [ggplot2::ggsave()].
#' @param width,height Figure size in inches when `output_file` is provided.
#' @param dpi Resolution when `output_file` is provided.
#'
#' @return A ggplot object.
#' @examples
#' df1 <- rdoc_example_data()
#' df2 <- df1
#' df3 <- df1
#' set.seed(1)
#' df2$r <- pmax(pmin(df2$r + stats::rnorm(nrow(df2), sd = 0.1), 1), -1)
#' df3$r <- pmax(pmin(df3$r + stats::rnorm(nrow(df3), sd = 0.15), 1), -1)
#' p <- rdoc_compare_heatplot(list(Sample_A = df1, Sample_B = df2, Sample_C = df3))
#' p
#' @export
rdoc_compare_heatplot <- function(corr_list,
                                  term_alignment = c("union", "strict", "intersection"),
                                  domain_palette = "Accent",
                                  significance_level = 0.05,
                                  show_significance_stars = TRUE,
                                  domain_gap = 0.55,
                                  annotation_height = 0.48,
                                  annotation_to_heatmap_gap = 1.2,
                                  compact_small_domain_labels = FALSE,
                                  small_domain_threshold = 5,
                                  domain_label_wrap_width = 22,
                                  show_term_labels = FALSE,
                                  show_significant_term_labels = FALSE,
                                  term_label_wrap_width = 18,
                                  significant_term_label_angle = 45,
                                  na_fill = "grey90",
                                  correlation_label = c("pearson", "spearman"),
                                  output_file = NULL,
                                  width = 12,
                                  height = 5.5,
                                  dpi = 300) {
  term_alignment <- match.arg(term_alignment)
  correlation_label <- match.arg(correlation_label)
  validated <- rdoc_validate_compare_list(corr_list, term_alignment = term_alignment)
  corr_data <- validated$data
  cond_names <- validated$condition_names
  term_levels <- validated$terms
  domain_levels <- validated$domain_levels

  term_order_df <- data.frame(
    Domain = validated$domain_by_term,
    Term = term_levels,
    stringsAsFactors = FALSE
  )
  term_order_df$term_id <- seq_len(nrow(term_order_df))
  term_order_df$domain_id <- match(term_order_df$Domain, domain_levels)
  term_order_df$x <- term_order_df$term_id + (term_order_df$domain_id - 1) * domain_gap

  x_map <- stats::setNames(term_order_df$x, term_order_df$Term)

  domain_cols <- stats::setNames(
    rdoc_get_domain_colors(domain_palette, length(domain_levels)),
    domain_levels
  )

  term_cols <- stats::setNames(rep(NA_character_, length(term_levels)), term_levels)
  for (dom in domain_levels) {
    terms_dom <- term_order_df$Term[term_order_df$Domain == dom]
    base_col <- unname(domain_cols[[dom]])
    term_cols[terms_dom] <- grDevices::colorRampPalette(
      c(colorspace::lighten(base_col, amount = 0.7), base_col)
    )(length(terms_dom))
  }

  n_conditions <- length(cond_names)
  y_values <- rev(seq_len(n_conditions))
  condition_y_map <- stats::setNames(y_values, cond_names)

  combined <- do.call(rbind, corr_data)
  combined$x <- as.numeric(x_map[as.character(combined$Term)])
  combined$y <- as.numeric(condition_y_map[as.character(combined$Condition)])
  combined$sig <- !is.na(combined$p) & combined$p < significance_level

  sig_terms <- unique(as.character(combined$Term[combined$sig]))
  sig_terms <- term_levels[term_levels %in% sig_terms]

  term_row_y <- max(y_values) + annotation_to_heatmap_gap
  domain_row_y <- term_row_y + annotation_height

  domain_tiles <- term_order_df[, c("Domain", "Term", "x")]
  domain_tiles$y <- domain_row_y
  term_tiles <- term_order_df[, c("Term", "x")]
  term_tiles$y <- term_row_y

  domain_bounds <- data.frame(Domain = domain_levels, stringsAsFactors = FALSE)
  domain_bounds$xmin <- NA_real_
  domain_bounds$xmax <- NA_real_
  domain_bounds$x_center <- NA_real_
  domain_bounds$n_terms <- NA_integer_
  for (i in seq_len(nrow(domain_bounds))) {
    dom <- domain_bounds$Domain[[i]]
    x_dom <- term_order_df$x[term_order_df$Domain == dom]
    domain_bounds$xmin[[i]] <- min(x_dom) - 0.5
    domain_bounds$xmax[[i]] <- max(x_dom) + 0.5
    domain_bounds$x_center[[i]] <- mean(range(x_dom))
    domain_bounds$n_terms[[i]] <- length(x_dom)
  }

  label_base <- as.character(domain_bounds$Domain)
  if (isTRUE(compact_small_domain_labels)) {
    label_base <- ifelse(
      domain_bounds$n_terms < as.integer(small_domain_threshold),
      rdoc_short_domain_labels(domain_bounds$Domain),
      domain_bounds$Domain
    )
  }
  domain_bounds$label <- rdoc_wrap_multiline(label_base, width = domain_label_wrap_width)

  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = domain_tiles,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        fill = Domain
      ),
      width = 0.95,
      height = annotation_height,
      colour = "grey45",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = domain_cols) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(
      data = term_tiles,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        fill = Term
      ),
      width = 0.95,
      height = annotation_height,
      colour = "grey45",
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = term_cols) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(
      data = combined,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        fill = r
      ),
      width = 0.95,
      height = 0.95,
      colour = "grey45",
      linewidth = 0.8
    ) +
    {
      if (isTRUE(show_significance_stars)) {
        ggplot2::geom_text(
          data = combined[combined$sig, , drop = FALSE],
          mapping = ggplot2::aes(x = x, y = y),
          label = "*",
          size = 4,
          fontface = "bold"
        )
      } else {
        NULL
      }
    } +
    ggplot2::geom_text(
      data = domain_bounds,
      mapping = ggplot2::aes(
        x = x_center,
        y = domain_row_y + annotation_height / 2 + 0.12,
        label = label
      ),
      angle = 0,
      fontface = "bold",
      size = 3.6,
      lineheight = 0.9,
      vjust = 0,
      hjust = 0.5
    ) +
    rdoc_correlation_scale(
      correlation_label = correlation_label,
      na_fill = na_fill
    ) +
    ggplot2::scale_y_continuous(
      breaks = as.numeric(condition_y_map[cond_names]),
      labels = cond_names,
      limits = c(min(y_values) - 0.5, domain_row_y + annotation_height / 2 + 0.8),
      expand = c(0, 0)
    ) +
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold", size = 11),
      plot.margin = ggplot2::margin(24, 16, 14, 16)
    ) +
    rdoc_correlation_legend_theme(title_size = 11, text_size = 11)

  if (isTRUE(show_term_labels)) {
    term_labels <- vapply(
      term_levels,
      rdoc_make_two_line_label,
      character(1),
      wrap_width = term_label_wrap_width
    )
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = term_order_df$x,
        labels = term_labels,
        position = "bottom",
        limits = c(min(term_order_df$x) - 0.5, max(term_order_df$x) + 0.5),
        expand = c(0, 0)
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size = 7.2, angle = 90, hjust = 0, vjust = 0.5),
        axis.ticks.x = ggplot2::element_blank()
      )
  } else if (isTRUE(show_significant_term_labels)) {
    sig_breaks <- term_order_df$x[term_order_df$Term %in% sig_terms]
    sig_labels <- vapply(
      term_order_df$Term[term_order_df$Term %in% sig_terms],
      rdoc_make_two_line_label,
      character(1),
      wrap_width = term_label_wrap_width
    )
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = sig_breaks,
        labels = sig_labels,
        position = "bottom",
        limits = c(min(term_order_df$x) - 0.5, max(term_order_df$x) + 0.5),
        expand = c(0, 0)
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          size = 8,
          angle = significant_term_label_angle,
          hjust = 0,
          vjust = 0
        ),
        axis.ticks.x = ggplot2::element_blank()
      )
  } else {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = NULL,
        labels = NULL,
        limits = c(min(term_order_df$x) - 0.5, max(term_order_df$x) + 0.5),
        expand = c(0, 0)
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

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

#' Backward-Compatible Alias for [rdoc_compare_heatplot()]
#'
#' @inheritParams rdoc_compare_heatplot
#' @param ... Additional arguments passed to [rdoc_compare_heatplot()].
#' @return A ggplot object.
#' @export
plot_rdoc_compare_heatmap <- function(corr_list, ...) {
  rdoc_compare_heatplot(corr_list = corr_list, ...)
}

#' Backward-Compatible Alias for [rdoc_compare_heatplot()]
#'
#' @inheritParams rdoc_compare_heatplot
#' @param ... Additional arguments passed to [rdoc_compare_heatplot()].
#' @return A ggplot object.
#' @export
plot_rdoc_heatmap_compare <- function(corr_list, ...) {
  rdoc_compare_heatplot(corr_list = corr_list, ...)
}
