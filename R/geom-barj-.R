geom_barj <-
  function(mapping = NULL, data = NULL, stat = "barJ",
           position = "stack", width = NULL, ...,
           show.legend = NA, inherit.aes = TRUE, na.rm = FALSE) {
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
        na.rm = na.rm,
        ...
      )
    )
  }

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.r
GeomBarJ <- ggplot2::ggproto(
  "GeomBarJ", ggplot2::GeomRect,
  required_aes = "x",
  # default_aes = ggplot2::aes(y = ..count.., colour="white"),

  setup_data = function(data, params) {
    # cat("GeomBarJ's setup_data\n")
    # print(data)
    if(is.null(data$colour)) data$colour = "white"
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data,
              ymin = pmin(y, 0), ymax = pmax(y, 0),
              xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },

  draw_panel = function(self, data, panel_scales, coord, width = NULL) {
    # Hack to ensure that width is detected as a parameter
    ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(data, panel_scales, coord)
  }
)

#' @export
#' @rdname geom_bar
#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
stat_bar <- function(mapping = NULL, data = NULL, geom = "barJ",
                     position = "stack", width = NULL, ...,
                     show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBarJ,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
StatBarJ <- ggplot2::ggproto(
  "StatBarJ", ggplot2::Stat,
  required_aes = "x",
  default_aes = ggplot2::aes(y = ..count.., colour="white"),

  setup_params = function(data, params) {
    if (!is.null(data$y) || !is.null(params$y)) {
      stop("stat_bar() must not be used with a y aesthetic.", call. = FALSE)
    }
    params
  },

  compute_group = function(self, data, scales, width = NULL) {
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))
    width <- width %||% (resolution(x) * 0.9)

    count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
    count[is.na(count)] <- 0

    data.frame(
      count = count,
      prop = count / sum(abs(count)),
      x = unique(x),
      width = width
    )
  }
)
