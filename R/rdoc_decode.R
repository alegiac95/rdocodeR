#' Decode a Brain Overlay Against RDoC Term Maps
#'
#' Computes spatial correlations between a FreeSurfer overlay and internal RDoC term maps,
#' and optionally writes a TSV file with the results.
#'
#' @param fs_overlay FreeSurfer overlay object accepted by `fsdecode`.
#' @param save_results Logical; write a TSV with correlations.
#' @param results_file Optional output path for the TSV file when `save_results = TRUE`.
#'   If `NULL`, defaults to `rdoc_decode_results.tsv` in the current working directory.
#' @param perm_method Correlation permutation method (`"eigen"`, `"spins"`, or `"nulls"`).
#'   Default is `"eigen"`.
#' @param fs_surrogate Optional precomputed surrogate matrix for
#'   `perm_method = "nulls"` or `"eigen"`. For `"eigen"`, you can also
#'   pass the raw list returned by `fsnull::fs_create_eigenstraps()`
#'   (with `lh` and `rh`), which will be combined automatically.
#' @param cor_method Correlation method passed to `fsdecode` (`"pearson"` or `"spearman"`).
#'   Default is `"pearson"`.
#' @param absolute_r Logical; if `TRUE`, stores absolute values of `r`.
#' @param mc_cores Number of cores used for Unix `mclapply`.
#'
#' @return A data frame with columns `Domain`, `Term`, `r`, and `p`.
#' @export
rdoc_decode <- function(fs_overlay,
                        save_results = FALSE,
                        results_file = NULL,
                        perm_method = c("eigen", "spins", "nulls"),
                        fs_surrogate = NULL,
                        cor_method = c("pearson", "spearman"),
                        absolute_r = FALSE,
                        mc_cores = max(1L, parallel::detectCores() - 1L)) {
  perm_method <- match.arg(perm_method)
  cor_method <- match.arg(cor_method)

  if (!requireNamespace("fsdecode", quietly = TRUE)) {
    stop("Package `fsdecode` is required for `rdoc_decode()`.", call. = FALSE)
  }

  terms_path <- rdoc_terms_file()
  terms <- readRDS(terms_path)
  ref <- rdoc_terms_reference()

  if (length(terms) != nrow(ref)) {
    stop(
      sprintf(
        "`terms_file` contains %d term maps, expected %d.",
        length(terms),
        nrow(ref)
      ),
      call. = FALSE
    )
  }

  if (perm_method %in% c("nulls", "eigen") && is.null(fs_surrogate)) {
    if (!requireNamespace("fsnull", quietly = TRUE)) {
      stop(
        "Package `fsnull` is required when `perm_method` is `\"nulls\"` or `\"eigen\"` and no `fs_surrogate` is supplied.",
        call. = FALSE
      )
    }
    if (perm_method == "nulls") {
      optimal_knn <- fsnull::get_optimal_knn(
        fs_overlay,
        hemi = "both",
        n.surr = 100L
      )
      fs_surrogate <- fsnull::fs_create_surrogates(
        fs_overlay,
        hemi = "both",
        knn = optimal_knn,
        n.surr = 1000L
      )
    }
    if (perm_method == "eigen") {
      eigenstraps <- fsnull::fs_create_eigenstraps(fs.overlay = fs_overlay)
      fs_surrogate <- cbind(eigenstraps$lh, eigenstraps$rh)
    }
  }
  if (perm_method == "eigen" &&
      !is.null(fs_surrogate) &&
      is.list(fs_surrogate) &&
      all(c("lh", "rh") %in% names(fs_surrogate))) {
    fs_surrogate <- cbind(fs_surrogate$lh, fs_surrogate$rh)
  }

  cor_fun <- switch(
    perm_method,
    eigen = function(i) {
      fsdecode::fs_spatial_cor_with_nulls(
        fs_overlay,
        terms[[i]],
        fs.x.nulls = fs_surrogate,
        method = cor_method
      )
    },
    spins = function(i) {
      fsdecode::fs_spatial_cor_spins(fs_overlay, terms[[i]], method = cor_method)
    },
    nulls = function(i) {
      fsdecode::fs_spatial_cor_with_nulls(
        fs_overlay,
        terms[[i]],
        fs.x.nulls = fs_surrogate,
        method = cor_method
      )
    }
  )

  worker <- function(i) {
    test <- tryCatch(
      cor_fun(i),
      error = function(e) NULL
    )

    if (is.null(test)) {
      return(data.frame(
        Domain = ref$Domain[i],
        Term = ref$Term[i],
        r = NA_real_,
        p = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      Domain = ref$Domain[i],
      Term = ref$Term[i],
      r = as.numeric(test$r.obs),
      p = as.numeric(test$p.perm),
      stringsAsFactors = FALSE
    )
  }

  idx <- seq_along(terms)
  if (.Platform$OS.type == "windows") {
    rows <- lapply(idx, worker)
  } else {
    rows <- parallel::mclapply(idx, worker, mc.cores = as.integer(mc_cores))
  }

  corr_df <- do.call(rbind, rows)

  if (absolute_r) {
    corr_df$r <- abs(corr_df$r)
  }

  if (isTRUE(save_results)) {
    if (is.null(results_file) || identical(results_file, "")) {
      results_file <- file.path(getwd(), "rdoc_decode_results.tsv")
    }
    dir.create(dirname(results_file), recursive = TRUE, showWarnings = FALSE)
    utils::write.table(
      corr_df,
      file = results_file,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    )
  }

  corr_df
}
