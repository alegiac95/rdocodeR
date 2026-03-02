rdoc_validate_corr_df <- function(corr_df) {
  if (!is.data.frame(corr_df)) {
    stop("`corr_df` must be a data frame.", call. = FALSE)
  }

  required_cols <- c("Domain", "Term", "r")
  missing_cols <- setdiff(required_cols, names(corr_df))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("`corr_df` is missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  if (anyDuplicated(corr_df$Term)) {
    stop("Each `Term` must appear only once in `corr_df`.", call. = FALSE)
  }

  if (!("p" %in% names(corr_df))) {
    corr_df$p <- NA_real_
  }

  corr_df$r <- suppressWarnings(as.numeric(corr_df$r))
  corr_df$p <- suppressWarnings(as.numeric(corr_df$p))

  if (anyNA(corr_df$r)) {
    stop("Column `r` must be numeric and non-missing.", call. = FALSE)
  }

  corr_df$Domain <- factor(corr_df$Domain, levels = unique(as.character(corr_df$Domain)))
  corr_df$Term <- factor(corr_df$Term, levels = unique(as.character(corr_df$Term)))

  corr_df
}

rdoc_expand_label_abbreviations <- function(x) {
  x <- as.character(x)
  x <- gsub("\\bRL\\b", "Reinforcement Learning", x, perl = TRUE)
  x <- gsub("\\bCC\\b", "Cognitive Control", x, perl = TRUE)
  x <- gsub("\\bWM\\b", "Working Memory", x, perl = TRUE)
  x
}

rdoc_expand_domain_labels <- function(x) {
  x <- as.character(x)
  domain_map <- c(
    AR = "Arousal/Reg.",
    CS = "Cognitive Systems",
    NV = "Negative Valence",
    PV = "Positive Valence Systems",
    SP = "Systems for Social Processes",
    SS = "Sensorimotor Systems"
  )
  idx <- x %in% names(domain_map)
  x[idx] <- unname(domain_map[x[idx]])
  x
}

rdoc_make_two_line_label <- function(x, wrap_width = 28) {
  x <- trimws(gsub("\\s+", " ", as.character(x)))

  # Prefer semantic splits first.
  x <- sub("\\s+-\\s+", " -\n", x, perl = TRUE)
  x <- sub("/", "/\n", x, fixed = TRUE)

  if (grepl("\n", x, fixed = TRUE)) {
    parts <- strsplit(x, "\n", fixed = TRUE)[[1]]
    if (length(parts) <= 2L) {
      return(paste(parts, collapse = "\n"))
    }
    return(paste(parts[1], paste(parts[-1], collapse = " "), sep = "\n"))
  }

  wrapped <- strwrap(x, width = wrap_width)
  if (length(wrapped) <= 1L) {
    return(x)
  }
  if (length(wrapped) == 2L) {
    return(paste(wrapped, collapse = "\n"))
  }
  paste(wrapped[1], paste(wrapped[-1], collapse = " "), sep = "\n")
}

#' Plot Circular RDoC Correlations with ggplot2
#'
#' Creates a circular RDoC plot using ggplot2 with three bands:
#' outer domains, middle constructs, and inner correlation bars.
#'
#' @param corr_df Data frame with columns `Domain`, `Term`, `r`, and optional `p`.
#'   `Term` must be unique.
#' @param domain_palette Palette name from [rdoc_available_palettes()].
#' @param output_file Optional output path. If provided, plot is saved with [ggplot2::ggsave()].
#' @param width,height Figure size in inches when `output_file` is provided.
#' @param dpi Resolution when `output_file` is provided.
#' @param angular_gap_term Angular gap between adjacent terms in the middle ring.
#' @param angular_gap_domain Angular gap between adjacent domains in the outer ring.
#' @param radial_gap_domains Radial gap between middle and outer rings.
#' @param significance_level Threshold used to draw significance stars (`p < threshold`).
#' @param show_term_labels Logical; draw term labels outside the domain ribbon.
#' @param expand_domain_labels Logical; when `TRUE`, domain codes are expanded
#'   in the outer ring labels (for example `CS` to `Cognitive Systems`).
#' @param highlight_significant_terms Logical; when `TRUE`, term labels with
#'   `p < significance_level` are drawn in bold.
#' @param show_only_significant_term_labels Logical; when `TRUE`, only labels
#'   for terms with `p < significance_level` are drawn.
#' @param show_bar_values Logical; when `TRUE`, show correlation values for
#'   significant bars (`p < significance_level`) inside the bars.
#' @param bar_value_digits Number of decimals used for bar value labels.
#' @param bar_value_size Text size for bar value labels.
#' @param term_label_offset Radial offset for external term labels.
#' @param term_label_size Text size for external term labels.
#' @param expand_term_abbreviations Logical; expand abbreviations in term labels:
#'   `RL` to Reinforcement Learning, `CC` to Cognitive Control,
#'   and `WM` to Working Memory.
#' @param term_label_wrap_width Target width for two-line label wrapping.
#'
#' @return A ggplot object.
#' @examples
#' df <- rdoc_example_data()
#' p <- plot_rdoc_gg(df, domain_palette = "Accent")
#' p
#' @export
plot_rdoc_gg <- function(corr_df,
                         domain_palette = "Accent",
                         output_file = NULL,
                         width = 10,
                         height = 10,
                         dpi = 300,
                         angular_gap_term = 0.10,
                         angular_gap_domain = 0.10,
                         radial_gap_domains = 0.05,
                         significance_level = 0.05,
                         show_term_labels = TRUE,
                         expand_domain_labels = FALSE,
                         highlight_significant_terms = FALSE,
                         show_only_significant_term_labels = FALSE,
                         show_bar_values = FALSE,
                         bar_value_digits = 2,
                         bar_value_size = 2.3,
                         term_label_offset = 0.16,
                         term_label_size = 3.2,
                         expand_term_abbreviations = TRUE,
                         term_label_wrap_width = 28) {
  corr_df <- rdoc_validate_corr_df(corr_df)
  corr_df <- corr_df[order(corr_df$Domain, corr_df$Term), , drop = FALSE]
  corr_df$id_num <- seq_len(nrow(corr_df))

  n_terms <- nrow(corr_df)
  n_domains <- nlevels(corr_df$Domain)

  domain_cols <- stats::setNames(
    rdoc_get_domain_colors(domain_palette, n_domains),
    levels(corr_df$Domain)
  )

  term_levels <- levels(corr_df$Term)
  term_cols <- stats::setNames(rep(NA_character_, length(term_levels)), term_levels)

  for (dom in levels(corr_df$Domain)) {
    terms_dom <- unique(as.character(corr_df$Term[corr_df$Domain == dom]))
    base_col <- unname(domain_cols[[dom]])
    term_cols[terms_dom] <- grDevices::colorRampPalette(
      c(colorspace::lighten(base_col, amount = 0.7), base_col)
    )(length(terms_dom))
  }

  bar_inner <- 0.7
  bar_max_height <- 1.0
  gap_after_bars <- 0.15
  construct_thick <- 0.25
  gap_after_construct <- radial_gap_domains
  domain_thick <- 0.40

  max_abs_r <- max(abs(corr_df$r), na.rm = TRUE)
  if (!is.finite(max_abs_r) || max_abs_r == 0) {
    max_abs_r <- 1
  }

  corr_df$r_height <- abs(corr_df$r) / max_abs_r
  corr_df$bar_ymin <- bar_inner
  corr_df$bar_ymax <- bar_inner + bar_max_height * corr_df$r_height
  corr_df$bar_xmin <- corr_df$id_num - 0.4
  corr_df$bar_xmax <- corr_df$id_num + 0.4

  construct_inner <- bar_inner + bar_max_height + gap_after_bars
  construct_outer <- construct_inner + construct_thick

  construct_df <- data.frame(
    Domain = corr_df$Domain,
    Term = corr_df$Term,
    id_num = corr_df$id_num,
    xmin = corr_df$id_num - 0.5 + angular_gap_term / 2,
    xmax = corr_df$id_num + 0.5 - angular_gap_term / 2,
    ymin = construct_inner,
    ymax = construct_outer
  )

  domain_inner <- construct_outer + gap_after_construct
  domain_outer <- domain_inner + domain_thick

  domain_df <- do.call(
    rbind,
    lapply(levels(corr_df$Domain), function(dom) {
      ids <- corr_df$id_num[corr_df$Domain == dom]
      data.frame(
        Domain = dom,
        xmin_raw = min(ids) - 0.5,
        xmax_raw = max(ids) + 0.5,
        ymin = domain_inner,
        ymax = domain_outer,
        stringsAsFactors = FALSE
      )
    })
  )

  domain_df$xmin <- domain_df$xmin_raw + angular_gap_domain / 2
  domain_df$xmax <- domain_df$xmax_raw - angular_gap_domain / 2
  domain_df$y_mid <- (domain_df$ymin + domain_df$ymax) / 2
  domain_df$text_col <- rdoc_contrast_color(domain_cols[domain_df$Domain])
  domain_df$Domain_label <- if (isTRUE(expand_domain_labels)) {
    rdoc_expand_domain_labels(domain_df$Domain)
  } else {
    as.character(domain_df$Domain)
  }

  domain_paths <- do.call(
    rbind,
    lapply(seq_len(nrow(domain_df)), function(i) {
      data.frame(
        Domain = as.character(domain_df$Domain[i]),
        Domain_label = as.character(domain_df$Domain_label[i]),
        x = seq(domain_df$xmin[i], domain_df$xmax[i], length.out = 200),
        y = domain_df$y_mid[i],
        text_col = domain_df$text_col[i],
        stringsAsFactors = FALSE
      )
    })
  )

  term_label_df <- corr_df
  term_labels <- as.character(term_label_df$Term)
  if (isTRUE(expand_term_abbreviations)) {
    term_labels <- rdoc_expand_label_abbreviations(term_labels)
  }
  term_label_df$term_label <- vapply(
    term_labels,
    rdoc_make_two_line_label,
    character(1),
    wrap_width = term_label_wrap_width
  )
  term_label_df$label_y <- domain_outer + term_label_offset
  term_label_df$angle <- 90 - 360 * (term_label_df$id_num - 0.5) / n_terms
  term_label_df$label_hjust <- ifelse(term_label_df$angle < -90, 1, 0)
  term_label_df$angle <- ifelse(term_label_df$angle < -90, term_label_df$angle + 180, term_label_df$angle)
  term_label_df$label_face <- "plain"
  if (isTRUE(highlight_significant_terms)) {
    term_label_df$label_face <- ifelse(
      !is.na(term_label_df$p) & term_label_df$p < significance_level,
      "bold",
      "plain"
    )
  }
  if (isTRUE(show_only_significant_term_labels)) {
    term_label_df <- term_label_df[
      !is.na(term_label_df$p) & term_label_df$p < significance_level,
      ,
      drop = FALSE
    ]
  }

  radius_max <- domain_outer + 0.1
  if (isTRUE(show_term_labels)) {
    radius_max <- max(radius_max, domain_outer + term_label_offset + 0.35)
  }
  star_df <- corr_df[!is.na(corr_df$p) & corr_df$p < significance_level, , drop = FALSE]
  bar_value_df <- corr_df[
    !is.na(corr_df$r) & !is.na(corr_df$p) & corr_df$p < significance_level,
    ,
    drop = FALSE
  ]
  bar_value_df$bar_label <- sprintf(paste0("%.", as.integer(bar_value_digits), "f"), bar_value_df$r)
  bar_value_df$bar_label_y <- (bar_value_df$bar_ymin + bar_value_df$bar_ymax) / 2
  bar_value_df$bar_label_col <- ifelse(abs(bar_value_df$r) >= 0.45, "white", "black")
  bar_value_df$bar_label_angle <- 90 - 360 * (bar_value_df$id_num - 0.5) / n_terms
  bar_value_df$bar_label_angle <- ifelse(
    bar_value_df$bar_label_angle < -90,
    bar_value_df$bar_label_angle + 180,
    bar_value_df$bar_label_angle
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = domain_df,
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax,
        fill = Domain
      ),
      colour = "grey70",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = domain_cols) +
    geomtextpath::geom_textpath(
      data = domain_paths,
      mapping = ggplot2::aes(
        x = x,
        y = y,
        label = Domain_label,
        group = Domain,
        colour = text_col
      ),
      size = if (isTRUE(expand_domain_labels)) 2.6 else 3,
      text_smoothing = 30,
      vjust = 0.5,
      upright = TRUE,
      linewidth = 0,
      linecolour = NA,
      show.legend = FALSE,
      fontface = "bold"
    ) +
    ggplot2::scale_colour_identity() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(
      data = construct_df,
      mapping = ggplot2::aes(
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax,
        fill = Term
      ),
      colour = "grey70",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = term_cols) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(
      data = corr_df,
      mapping = ggplot2::aes(
        xmin = bar_xmin, xmax = bar_xmax,
        ymin = bar_ymin, ymax = bar_ymax,
        fill = r
      ),
      colour = "grey50"
    ) +
    ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1),
      name = expression("Spearman " * rho),
      guide = ggplot2::guide_colourbar(
        title = expression("Spearman " * rho),
        frame.colour = "black",
        frame.linewidth = 0.6,
        ticks.colour = "black",
        title.position = "left",
        label.position = "right",
        title.theme = ggplot2::element_text(
          angle = 90,
          colour = "black",
          hjust = 0.5,
          vjust = 0.5
        ),
        label.theme = ggplot2::element_text(colour = "black"),
        direction = "vertical"
      )
    ) +
    ggplot2::geom_text(
      data = star_df,
      mapping = ggplot2::aes(
        x = id_num,
        y = bar_ymax + 0.07
      ),
      label = "*",
      size = 5,
      fontface = "bold"
    ) +
    {
      if (isTRUE(show_bar_values)) {
        ggplot2::geom_text(
          data = bar_value_df,
          mapping = ggplot2::aes(
            x = id_num,
            y = bar_label_y,
            label = bar_label,
            colour = bar_label_col,
            angle = bar_label_angle
          ),
          size = bar_value_size,
          fontface = "bold",
          hjust = 0.5,
          vjust = 0.5
        )
      } else {
        NULL
      }
    } +
    {
      if (isTRUE(show_term_labels)) {
        ggplot2::geom_text(
          data = term_label_df,
          mapping = ggplot2::aes(
            x = id_num,
            y = label_y,
            label = term_label,
            angle = angle,
            hjust = label_hjust,
            fontface = label_face
          ),
          size = term_label_size,
          colour = "black",
          vjust = 0.5,
          lineheight = 0.9
        )
      } else {
        NULL
      }
    } +
    ggplot2::scale_x_continuous(
      limits = c(0.5, n_terms + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::coord_polar(theta = "x", clip = "off") +
    ggplot2::ylim(0, radius_max) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(
        angle = 90,
        colour = "black",
        vjust = 0.5,
        hjust = 0.5
      ),
      legend.text = ggplot2::element_text(colour = "black"),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.frame = ggplot2::element_rect(fill = NA, colour = "black", linewidth = 0.6),
      legend.key = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.6),
      plot.margin = ggplot2::margin(10, 30, 10, 10)
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

#' Alias for [plot_rdoc_gg()]
#'
#' @inheritParams plot_rdoc_gg
#' @param ... Additional arguments passed to [plot_rdoc_gg()].
#' @return A ggplot object.
#' @export
plot_rdoc <- function(corr_df, ...) {
  plot_rdoc_gg(corr_df = corr_df, ...)
}
