#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
#' @seealso [stat_bin()], which bins data in ranges and counts the
#'   cases in each range. It differs from `stat_count()`, which counts the
#'   number of cases at each `x` position (without binning into ranges).
#'   [stat_bin()] requires continuous `x` data, whereas
#'   `stat_count()` can be used for both discrete and continuous `x` data.
#'
#' @export
#' @rdname geom_bar
stat_count <- function(mapping = NULL, data = NULL,
                       geom = "barj", position = "stack",
                       ...,
                       width = NULL,
                       na.rm = FALSE,
                       orientation = NA,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    orientation = orientation,
    width = width,
    ...
  )
  if (!is.null(params$y)) {
    abort("stat_countj() must not be used with a y aesthetic.")
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatCountJ,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
StatCountJ <- ggplot2::ggproto("StatCountJ", Stat,
  required_aes = "x|y",

  default_aes = aes(x = ggplot2::after_stat(count), y = ggplot2::after_stat(count), weight = 1),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      abort("stat_countj() requires an x or y aesthetic.")
    }
    if (has_x && has_y) {
      abort("stat_countj() can only have an x or y aesthetic.")
    }

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(self, data, scales, width = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    x <- data$x
    weight <- data$weight %||% rep(1, length(x))
    width <- width %||% (resolution(x) * 0.9)

    count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
    count[is.na(count)] <- 0

    bars <- new_data_frame(list(
      count = count,
      prop = count / sum(abs(count)),
      x = sort(unique(x)),
      width = width,
      flipped_aes = flipped_aes
    ), n = length(count))
    flip_data(bars, flipped_aes)
  }
)

#' #' @export
#' #' @rdname geom_bar
#' #' @section Computed variables:
#' #' \describe{
#' #'   \item{count}{number of points in bin}
#' #'   \item{prop}{groupwise proportion}
#' #' }
#' stat_bar <- function(mapping = NULL,
#'                      data = NULL,
#'                      geom = "barJ",
#'                      position = "stack",
#'                      width = NULL,
#'                      ...,
#'                      show.legend = NA,
#'                      inherit.aes = TRUE) {
#'   ggplot2::layer(
#'     data = data,
#'     mapping = mapping,
#'     stat = StatBarJ,
#'     geom = geom,
#'     position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = list(width = width,
#'                   ...)
#'   )
#' }
#'

#' #' @rdname ggplot2-ggproto
#' #' @format NULL
#' #' @usage NULL
#' #' @export
#' #' @include stat-.r
#' StatBarJ <- ggplot2::ggproto(
#'   "StatBarJ",
#'   ggplot2::Stat,
#'   required_aes = "x",
#'   default_aes = ggplot2::aes(y = ..count.., colour = "white"),
#'
#'   setup_params = function(data, params) {
#'     if (!is.null(data$y) || !is.null(params$y)) {
#'       stop("stat_bar() must not be used with a y aesthetic.", call. = FALSE)
#'     }
#'     params
#'   },
#'
#'   compute_group = function(self, data, scales, width = NULL) {
#'     x <- data$x
#'     weight <- data$weight %||% rep(1, length(x))
#'     width <- width %||% (resolution(x) * 0.9)
#'
#'     count <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
#'     count[is.na(count)] <- 0
#'
#'     data.frame(
#'       count = count,
#'       prop = count / sum(abs(count)),
#'       x = unique(x),
#'       width = width
#'     )
#'   }
#' )