stat_binj <- function(
  mapping = NULL, data = NULL,
  geom = "barJ", position = "stack",
  ...,
  binwidth = NULL,
  bins = NULL,
  center = NULL,
  boundary = NULL,
  closed = c("right", "left"),
  pad = FALSE,
  na.rm = FALSE,
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
      closed = closed,
      pad = pad,
      na.rm = na.rm,
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
    if (!is.null(data$y) || !is.null(params$y)) {
      stop("stat_binJ() must not be used with a y aesthetic.", call. = FALSE)
    }
    if (is.integer(data$x)) {
      stop('StatBin requires a continuous x variable the x variable is discrete. Perhaps you want stat="count"?',
           call. = FALSE)
    }
    if (!is.null(params$drop)) {
      warning("`drop` is deprecated. Please use `pad` instead.", call. = FALSE)
      params$drop <- NULL
    }
    if (!is.null(params$origin)) {
      warning("`origin` is deprecated. Please use `boundary` instead.", call. = FALSE)
      params$boundary <- params$origin
      params$origin <- NULL
    }
    if (!is.null(params$right)) {
      warning("`right` is deprecated. Please use `closed` instead.", call. = FALSE)
      params$closed <- if (params$right) "right" else "left"
      params$right <- NULL
    }
    if (!is.null(params$width)) {
      stop("`width` is deprecated. Do you want `geom_bar()`?", call. = FALSE)
    }
    if (!is.null(params$boundary) && !is.null(params$center)) {
      stop("Only one of `boundary` and `center` may be specified.", call. = FALSE)
    }

    # if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
    #   message_wrap("`stat_binJ()` using `bins = 30`. Pick better value with `binwidth`.")
    # }
    if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
      message_wrap("`stat_bin()` using `bins = freedman`. Pick better value with `binwidth`.")
      params$bins <- "fr"
    }
    # print(params)
    params
  },

  #   compute_group = function(
  # 	  data, scales, binwidth=NULL, bins = NULL,
  # 	  origin=NULL, breaks=NULL, width=0.9, drop = FALSE,
  # 	  right = FALSE) {
  compute_group = function(
    data, scales, binwidth = NULL, bins = NULL,
    center = NULL, boundary = NULL,
    closed = c("right", "left"), pad = FALSE,
    # The following arguments are not used, but must
    # be listed so parameters are computed correctly
    breaks = NULL, origin = NULL, right = NULL,
    drop = NULL, width = NULL) {
    # print(head(data))
    # print(scales)
    # range <- scales$x$dimension()
    #
    # binJ(data$x, data$weight, binwidth=binwidth, bins = bins,
    #      origin=origin, breaks=breaks, range=range, width=width,
    #      drop = drop, right = right)
    if (!is.null(breaks)) {
      bins <- bin_breaks(breaks, closed)
    } else if (!is.null(binwidth)) {
      bins <- bin_breaks_width(scales$x$dimension(), binwidth, center = center,
                               boundary = boundary, closed = closed)
    } else {
      # print(data$x)
      bins <- bin_breaks_binsj(data$x, scales$x$dimension(), bins, center = center,
                              boundary = boundary, closed = closed)
    }
    bin_vector(data$x, bins, weight = data$weight, pad = pad)
  },

  default_aes = ggplot2::aes(y = ..count.., colour="white"),
  required_aes = c("x")

)

