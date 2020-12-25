#' geom_barH
geom_barh <-
  function (mapping = NULL,
            data = NULL,
            stat = "count",
            # stat = "barH",
            position = "stack",
            ...,
            width = NULL,
            binwidth = NULL,
            na.rm = FALSE,
            orientation = NA,
            hatch = 3,
            show.legend = NA,
            inherit.aes = TRUE) {
    if (!is.null(binwidth)) {
      warn("`geom_barh()` no longer has a `binwidth` parameter. Please use `geom_histogramh()` instead.")
      return(geom_histogramh(mapping = mapping, data = data,
                             position = position, width = width, binwidth = binwidth, hatch = hatch,
                             ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes))
    }
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomBarH,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        width = width,
        na.rm = na.rm,
        orientation = orientation,
        hatch = hatch,
        ...
      )
    )
  }

# GeomBarH <- proto(GeomBarJ, {
#   objname <- "barH"
#
#   default_stat <- function(.) StatBinH
#   default_aes <- function(.) aes(colour="black", fill="black", size=0.5, linetype=1, weight = 1, alpha = NA)
#
#   draw_groups <- function(., data, scales, coordinates, ...) {
#     GeomRectH$draw_groups(data, scales, coordinates, ...)
#   }
#
# })
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.r
GeomBarH <- ggplot2::ggproto(
  "GeomBarH",
  GeomRectH,
  required_aes = c("x", "y"),

  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    # print(params)
    # data$hatch = params$hatch
    # print(data)
    transform(
      data,
      ymin = pmin(y, 0),
      ymax = pmax(y, 0),
      xmin = x - width / 2,
      xmax = x + width / 2,
      width = NULL
    )
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE) {
    # Hack to ensure that width is detected as a parameter
    # data2 <- hatch2()
    # print(data)
    #print(coord)
    ggplot2::ggproto_parent(GeomRectH, self)$draw_panel(data, panel_params, coord)
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
StatBarH <- ggplot2::ggproto(
  "StatBarH",
  ggplot2::Stat,
  required_aes = "x",
  default_aes = ggplot2::aes(y = ..count.., colour = "white", hatch = 3),

  setup_params = function(data, params) {
    if (!is.null(data$y) || !is.null(params$y)) {
      stop("stat_barh() must not be used with a y aesthetic.", call. = FALSE)
    }
    # print(params)
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
      width = width,
      hatch = hatch
    )
  }
)
