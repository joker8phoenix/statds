draw_key_hatch <- function(data, ...) {
  with(data, grobTree(
    linesGrob(x=c(0.125,0.125), y=c(0.875,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.125,0.250), y=c(0.750,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.125,0.375), y=c(0.625,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.125,0.500), y=c(0.500,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.125,0.625), y=c(0.375,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.125,0.750), y=c(0.250,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.125,0.875), y=c(0.125,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.250,0.875), y=c(0.125,0.750), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.375,0.875), y=c(0.125,0.625), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.500,0.875), y=c(0.125,0.500), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.625,0.875), y=c(0.125,0.375), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.750,0.875), y=c(0.125,0.250), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    linesGrob(x=c(0.875,0.875), y=c(0.125,0.125), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
    rectGrob(height=0.750, width=0.750, gp = gpar(col = alpha(colour, alpha), fill = NA, lty = linetype))
  ))
}

draw_key_boxplot <- function(data, params) {
  grobTree(
    linesGrob(0.5, c(0.1, 0.25)),
    linesGrob(0.5, c(0.75, 0.9)),
    rectGrob(height = 0.5, width = 0.75),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype
    )
  )
}
