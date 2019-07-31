#' Histogram using geom_barbed
#'
geom_histbarbed <- function (
  mapping = NULL, data = NULL, stat = "binJ",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarbed,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
