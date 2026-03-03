rdoc_correlation_legend_label <- function(correlation_label = c("pearson", "spearman")) {
  correlation_label <- match.arg(correlation_label)
  if (identical(correlation_label, "spearman")) {
    return(expression("Spearman " * rho))
  }
  "Pearson r"
}

