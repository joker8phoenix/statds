stat_binj <- function(
  mapping = NULL, data = NULL,
  geom = "barj", position = "stack",
  ...,
  binwidth = NULL,
  bins = NULL,
  center = NULL,
  boundary = NULL,
  breaks = NULL,
  closed = c("right", "left"),
  pad = FALSE,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBinJ,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      # width = width,
      # drop = drop,
      # right = right,
      # bins = bins,
      # binwidth = binwidth,
      # origin = origin,
      # breaks = breaks,
      # na.rm = na.rm,
      # ...
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      breaks = breaks,
      closed = closed,
      pad = pad,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBinJ <- ggplot2::ggproto(
  "StatBinJ", ggplot2:::Stat,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      abort("stat_bin() requires an x or y aesthetic.")
    }
    if (has_x && has_y) {
      abort("stat_bin() can only have an x or y aesthetic.")
    }
    x <- flipped_names(params$flipped_aes)$x
    if (is.integer(data[[x]])) {
      abort(glue("StatBin requires a continuous {x} variable: the {x} variable is discrete.",
                 "Perhaps you want stat=\"count\"?"))
    }

    if (!is.null(params$drop)) {
      warn("`drop` is deprecated. Please use `pad` instead.")
      params$drop <- NULL
    }
    if (!is.null(params$origin)) {
      warn("`origin` is deprecated. Please use `boundary` instead.")
      params$boundary <- params$origin
      params$origin <- NULL
    }
    if (!is.null(params$right)) {
      warn("`right` is deprecated. Please use `closed` instead.")
      params$closed <- if (params$right) "right" else "left"
      params$right <- NULL
    }
    if (!is.null(params$width)) {
      abort("`width` is deprecated. Do you want `geom_bar()`?")
    }
    if (!is.null(params$boundary) && !is.null(params$center)) {
      abort("Only one of `boundary` and `center` may be specified.")
    }

    # if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
    #   message_wrap("`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.")
    #   params$bins <- 30
    # }
    #
    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message_wrap("`stat_binj()` using `bins = freedman`. Pick better value with `binwidth`.")
      params$bins <- "fr"
    }
    # print(params)
    params
  },

  extra_params = c("na.rm", "orientation"),

  #   compute_group = function(
  # 	  data, scales, binwidth=NULL, bins = NULL,
  # 	  origin=NULL, breaks=NULL, width=0.9, drop = FALSE,
  # 	  right = FALSE) {
  compute_group = function(
    data, scales, binwidth = NULL, bins = NULL,
    center = NULL, boundary = NULL,
    closed = c("right", "left"), pad = FALSE,
    breaks = NULL, flipped_aes = FALSE,
    # The following arguments are not used, but must
    # be listed so parameters are computed correctly
    origin = NULL, right = NULL,
    drop = NULL, width = NULL) {
    # print(head(data))
    # print(scales)
    # range <- scales$x$dimension()
    #
    # binJ(data$x, data$weight, binwidth=binwidth, bins = bins,
    #      origin=origin, breaks=breaks, range=range, width=width,
    #      drop = drop, right = right)
    x <- flipped_names(flipped_aes)$x
    if (!is.null(breaks)) {
      if (!scales[[x]]$is_discrete()) {
        breaks <- scales[[x]]$transform(breaks)
      }
      bins <- bin_breaks(breaks, closed)
    } else if (!is.null(binwidth)) {
      if (is.function(binwidth)) {
        binwidth <- binwidth(data[[x]])
      }
      bins <- bin_breaks_width(scales[[x]]$dimension(), binwidth, center = center,
                               boundary = boundary, closed = closed)
    } else {
      # print(data$x)
      bins <- bin_breaks_binsj(data[[x]], scales[[x]]$dimension(), bins, center = center,
                              boundary = boundary, closed = closed)
    }
    bins <- bin_vector(data[[x]], bins, weight = data$weight, pad = pad)
    bins$flipped_aes <- flipped_aes
    flip_data(bins, flipped_aes)
  },

  # default_aes = ggplot2::aes(y = ..count.., colour="white"),
  # required_aes = c("x")
  default_aes = aes(x = after_stat(count), y = after_stat(count), weight = 1, colour="white"),

  required_aes = "x|y"

)

