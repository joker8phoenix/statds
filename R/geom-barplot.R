#' geom_barH
geom_barplot <- function (
  mapping = NULL, data = NULL, stat = "identity", 
  position = "identity", width=NULL, ...,
  show.legend = NA, inherit.aes = TRUE,na.rm=FALSE) {
  ggplot2::layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes, 
    params = list(
      width = width,
      na.rm=na.rm,
      ...
    )
  )
}
