#' geom_pointJ
#' 
geom_histline <- function (
  mapping = NULL, data = NULL, stat = "binJ",
  position = "identity", 
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
