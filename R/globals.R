# Suppress R CMD check notes for non-standard evaluation in ggplot2 aesthetics.
utils::globalVariables(
  c(
    "angle",
    "Condition", "Domain", "Domain_label", "Term", "bar_xmax", "bar_xmin", "bar_ymax", "bar_ymin",
    "bar_label", "bar_label_angle", "bar_label_col", "bar_label_y", "col", "id_num", "label_col",
    "label", "label_face", "label_hjust", "label_y", "legend_label", "r", "row", "shade", "sig",
    "star_y", "term_label", "text_col", "x", "x_center", "xmax", "xmin", "y",
    "ymax", "ymin"
  )
)
