#' geom_pointJ
#' 
geom_pointj <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointJ,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomPointJ <- ggplot2::ggproto(
  "GeomPointJ", 
  ggplot2:::GeomPoint, 
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(
    shape = 1, 
    colour = "black", 
    size = 1.5, 
    fill = NA, 
    alpha = NA,
    stroke = 1)
)
