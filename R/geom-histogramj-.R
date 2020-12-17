#' Histograms and frequency polygons.
#'
geom_histogramj <- function(
  # mapping = NULL, data = NULL, stat = "binJ",
  # binwidth = NULL, bins = NULL, origin = NULL, right = FALSE,
  # position = "stack", show.legend = NA, inherit.aes = TRUE, ...,
  # na.rm = FALSE) {
  mapping = NULL, data = NULL,
  stat = "binj", position = "stack",
  ...,
  binwidth = NULL,
  bins = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE){
  ggplot2::layer(
    # data = data,
    # mapping = mapping,
    # stat = stat,
    # geom = GeomBarJ,
    # position = position,
    # show.legend = show.legend,
    # inherit.aes = inherit.aes,
    # params = list(
    #   binwidth = binwidth,
    #   bins = bins,
    #   origin = origin,
    #   right = right,
    #   na.rm = na.rm,
    #   ...
    # )
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarJ,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
      ...
    )
  )
}
