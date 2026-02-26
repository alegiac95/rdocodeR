# RDoC-focused hex sticker mockup for rdocodeR
# Generates: /Users/alessio/rdocodeR/rdocodeR_hex_mockup.png

png(
  "/Users/alessio/rdocodeR/rdocodeR_hex_mockup.png",
  width = 1400, height = 1600, bg = "transparent", res = 220
)
par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)

hex_xy <- function(cx, cy, r, rot = pi / 6) {
  th <- seq(0, 2 * pi, length.out = 7) + rot
  cbind(cx + r * cos(th), cy + r * sin(th))
}

draw_rotated_gradient_text <- function(label, x, y, srt = 0, cex = 1, font = 2, cols) {
  chars <- strsplit(label, "", fixed = TRUE)[[1]]
  n <- length(chars)
  if (length(cols) < n) {
    cols <- grDevices::colorRampPalette(cols)(n)
  } else {
    cols <- cols[seq_len(n)]
  }

  widths <- vapply(chars, strwidth, numeric(1), cex = cex, units = "user", font = font)
  total_w <- sum(widths)
  x0 <- x - total_w / 2
  ang <- srt * pi / 180
  dirx <- cos(ang)
  diry <- sin(ang)

  offset <- 0
  for (i in seq_len(n)) {
    xi <- x0 + offset + widths[i] / 2
    x_char <- x + (xi - x) * dirx
    y_char <- y + (xi - x) * diry
    text(x_char, y_char, labels = chars[i], srt = srt, cex = cex, font = font, col = cols[i])
    offset <- offset + widths[i]
  }
}

cx <- 0.5
cy <- 0.56

# Hex shell
h_outer <- hex_xy(cx, cy, 0.47)
polygon(h_outer, col = "white", border = "#2E3440", lwd = 6)
h_inner <- hex_xy(cx, cy, 0.445)
polygon(h_inner, col = "#071330", border = NA)

# RDoC domains (network style instead of circular barplot)
domain_labels <- c(
  "Arousal/\nRegulatory",
  "Cognitive\nSystems",
  "Negative\nValence",
  "Positive\nValence",
  "Social\nProcesses",
  "Sensorimotor"
)
domain_cols <- c("#71B86B", "#6E89C4", "#D46A6A", "#E3B35A", "#8C79C8", "#56ABC1")

angles <- seq(pi / 2, pi / 2 - 2 * pi + 2 * pi / 6, length.out = 6)
r_nodes <- 0.23
node_r <- 0.055
xs <- cx + r_nodes * cos(angles)
ys <- cy + r_nodes * sin(angles)

# Framework links
for (i in seq_along(xs)) {
  segments(cx, cy, xs[i], ys[i], col = "#94A3B8", lwd = 2.2)
  j <- if (i == length(xs)) 1 else i + 1
  segments(xs[i], ys[i], xs[j], ys[j], col = "#475569", lwd = 1.4)
}

# Subtle construct points cloud to imply multidimensional space
set.seed(42)
n_pts <- 110
th <- runif(n_pts, 0, 2 * pi)
rr <- runif(n_pts, 0.02, 0.17)
px <- cx + rr * cos(th)
py <- cy + rr * sin(th)
points(px, py, pch = 16, cex = 0.33, col = "#8DA2C133")

# Domain nodes
symbols(xs, ys, circles = rep(node_r, length(xs)), inches = FALSE, add = TRUE,
        bg = domain_cols, fg = "white", lwd = 1.8)
for (i in seq_along(xs)) {
  text(xs[i], ys[i], labels = domain_labels[i], cex = 0.42, font = 2, col = "white")
}

# Center badge
symbols(cx, cy, circles = 0.10, inches = FALSE, add = TRUE,
        bg = "#0B132B", fg = "#94A3B8", lwd = 2.2)
text(cx, cy + 0.02, labels = "RDoC", cex = 1.35, font = 2, col = "#F3F4F6")
text(cx, cy - 0.02, labels = "Research Domain Criteria", cex = 0.5, font = 1, col = "#CBD5E1")

# Branding text
text(0.5, 0.90, labels = "RDoC Framework", col = "#E5E7EB", cex = 1.0, font = 2)
# Package name inside lower-right hex segment, parallel to edge
draw_rotated_gradient_text(
  label = "rdocodeR",
  x = 0.675, y = 0.255,
  srt = 30,
  cex = 1.45,
  font = 2,
  cols = domain_cols
)

box(col = NA)
dev.off()
