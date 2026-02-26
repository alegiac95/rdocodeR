# Suppress R CMD check notes for non-standard evaluation in ggplot2 aesthetics.
utils::globalVariables(
  c(
    "angle",
    "Domain", "Term", "bar_xmax", "bar_xmin", "bar_ymax", "bar_ymin",
    "bar_label", "bar_label_angle", "bar_label_col", "bar_label_y", "col", "id_num", "label_col",
    "label_face", "label_hjust", "label_y", "legend_label", "r", "row", "term_label",
    "text_col", "x", "xmax", "xmin", "y",
    "ymax", "ymin"
  )
)
