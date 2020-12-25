geom_histogramh <- function(
  mapping = NULL, data = NULL,
  stat = "binH", position = "stack",
  ...,
  hatch = NULL,
  binwidth = NULL,
  bins = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE){
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarH,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      hatch = hatch,
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
      ...
    )
  )
}
