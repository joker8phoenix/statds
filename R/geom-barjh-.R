#' geom_barH
geom_barh <- function (
  mapping = NULL, data = NULL, stat = "barH", 
  position = "stack", width=NULL, hatch=NULL,...,
  show.legend = NA, inherit.aes = TRUE,na.rm=FALSE) {
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
  "GeomBarH", GeomRectH,
  required_aes = "x",
  
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    # print(params)
    # data$hatch = params$hatch
    # print(data)
    transform(data,
              ymin = pmin(y, 0), ymax = pmax(y, 0),
              xmin = x - width / 2, xmax = x + width / 2, 
              width = NULL
    )
  },
  
  draw_panel = function(self, data, panel_scales, coord, width = NULL) {
    # Hack to ensure that width is detected as a parameter
    # data2 <- hatch2()
    # print(data)
    #print(coord)
    ggplot2::ggproto_parent(GeomRectH, self)$draw_panel(
      data, panel_scales, coord)
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
StatBarH <- ggplot2::ggproto(
  "StatBarH", ggplot2::Stat,
  required_aes = "x",
  default_aes = ggplot2::aes(y = ..count.., colour="white"),
  
  setup_params = function(data, params) {
    if (!is.null(data$y) || !is.null(params$y)) {
      stop("stat_bar() must not be used with a y aesthetic.", call. = FALSE)
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
