#' Points, jittered to reduce overplotting.
#'
geom_jitterj <-
  function(mapping = NULL, data = NULL,
  width = NULL, height = NULL, stat = "identity", position = "jitter", ...,
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  if (!missing(width) || !missing(height)) {
    if (!missing(position)) {
      stop("Specify either `position` or `width`/`height`", call. = FALSE)
    }

    position <- position_jitter(width = width, height = height)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointJ,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(..., na.rm = na.rm)
  )
}
