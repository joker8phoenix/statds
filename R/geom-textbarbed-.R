#' Barbed with Text
#'
#' plot lines and symbols
#'
#' @inheritParams geom_point
#' @param linetype Line style
#' @param linewidth Line size
#' @param size point size
#' @param shape point shape
#' @param arrow Arrow specification, as created by ?grid::arrow
#' @seealso \code{\link{geom_line}}: Functional (ordered) lines;
#'  \code{\link{geom_polygon}}: Filled paths (polygons);
#'  \code{\link{geom_segment}}: Line segments
#' @export
#' @examples
#' \donttest{
#' # Generate data
#' library(plyr)
#' myear <- ddply(movies, .(year), colwise(mean, .(length, rating)))
#' p <- ggplot(myear, aes(length, rating))
#' p + geom_barbed()
#' p + geom_barbed(size=3)
#' p + geom_barbed(size=4)
#' p + geom_barbed(size=5)
#' p + geom_barbed(size=1,space=1)
#' p + geom_barbed(size=1,space=2)
#' p + geom_barbed(size=1,space=3)
#' p + geom_barbed(size=2,linewidth=1,space=2)
#' p + geom_barbed(size=3,linewidth=2,space=3)
#' p + geom_barbed(size=4,linewidth=3,space=4)
#' p + geom_barbed(linetype=2)
#' p + geom_barbed(linetype=3)
#' p + geom_barbed(linetype=4)
#' p + geom_barbed(shape=2)
#' p + geom_barbed(shape=3)
#' p + geom_barbed(shape=4)
#'
#' #' Add aesthetic mappings
#' p + geom_barbed(aes(size = year))
#' p + geom_barbed(aes(colour = year))
#' p + geom_barbed(aes(colour = year, size=year))
#' p + geom_barbed(aes(colour=(year)),shape=20)
#'
#' #' Change scale
#' p + geom_barbed(aes(size = year)) + scale_size(range = c(1, 3))
#'
#' #' Set aesthetics to fixed value
#' p + geom_barbed(colour = "green")
#'
#' #' Control line join parameters
#' df <- data.frame(x = 1:3, y = c(4, 1, 9))
#' base <- ggplot(df, aes(x, y))
#' base + geom_barbed(size = 10)
#' base + geom_barbed(size = 10, linewidth=8, linejoin = "mitre", lineend = "butt")
#'
#' #' Use qplot instead
#' qplot(length, rating, data=myear, geom="barbed")
#'
#' #' Using economic data:
#' #' How is unemployment and personal savings rate related?
#' qplot(unemploy/pop, psavert, data=economics, geom="barbed")
#' qplot(unemploy/pop, psavert, data=economics, geom="barbed", size=as.numeric(date))
#'
#' #' How is rate of unemployment and length of unemployment?
#' qplot(unemploy/pop, uempmed, data=economics, geom="barbed")
#'
#' #' geom_barbed removes missing values on the ends of a line.
#' #' use na.rm = T to suppress the warning message
#' df <- data.frame(
#'   x = 1:5,
#'   y1 = c(1, 2, 3, 4, NA),
#'   y2 = c(NA, 2, 3, 4, 5),
#'   y3 = c(1, 2, NA, 4, 5),
#'   y4 = c(1, 2, 3, 4, 5))
#' qplot(x, y1, data = df, geom = "barbed", arrow=arrow())
#' qplot(x, y2, data = df, geom = "barbed", arrow=arrow())
#' qplot(x, y3, data = df, geom = "barbed", arrow=arrow(), na.rm=T)
#' qplot(x, y4, data = df, geom = "barbed", arrow=arrow())
#'
#' #' Setting line type vs colour/size
#' #' Line type needs to be applied to a line as a whole, so it can
#' #' not be used with colour or size that vary across a line
#'
#' x <- seq(0.01, .99, length=100)
#' df <- data.frame(x = rep(x, 2), y = c(qlogis(x), 2 * qlogis(x)), group = rep(c("a","b"), each=100))
#' p <- ggplot(df, aes(x=x, y=y, group=group))
#'
#' #' Should work
#' p + geom_barbed(aes(colour = x))
#'
#' #' Should fail
#' should_stop(p + geom_line(aes(colour = x), linetype=2))
#'
#' #' Summarise number of movie ratings by year of movie
#' mry <- do.call(rbind, by(movies, round(movies$rating), function(df) {
#'   nums <- tapply(df$length, df$year, length)
#'   data.frame(rating=round(df$rating[1]), year = as.numeric(names(nums)), number=as.vector(nums))
#' }))
#'
#' p <- ggplot(mry, aes(x=year, y=number, group=rating))
#' p + geom_barbed()
#'
#' #' Add aesthetic mappings
#' p + geom_barbed(aes(size = rating))
#' p + geom_barbed(aes(colour = rating))
#' p + geom_barbed(aes(linewidth = rating))
#'
#' #' Change scale
#' p + geom_barbed(aes(colour = rating)) + scale_colour_gradient(low="red")
#' p + geom_barbed(aes(size = rating)) + scale_size(range = c(0.1, 3))
#'
#' #' Set aesthetics to fixed value
#' p + geom_barbed(colour = "red", size = 1)
#'
#' #' Use qplot instead
#' qplot(year, number, data=mry, group=rating, geom="barbed")
#'
#' #' Using a time series
#' qplot(date, pop, data=economics, geom="barbed")
#'
#' qplot(date, pop, data=economics, geom="barbed", log="y")
#' qplot(date, pop, data=subset(economics, date > as.Date("2006-1-1")), geom="barbed")
#' qplot(date, pop, data=economics, size=unemploy/pop, geom="barbed")
#'
#' #' A simple pcp example
#' y2005 <- runif(300, 20, 120)
#' y2010 <- y2005 * runif(300, -1.05, 1.5)
#' group <- rep(LETTERS[1:3], each = 100)
#'
#' df <- data.table(id = seq_along(group), group, y2005, y2010)
#' library(reshape2) #' for melt
#' dfm <- melt(df, id.var = c("id", "group"))
#' ggplot(dfm, aes(variable, value, group = id, colour = group)) +
#'   geom_barbed(aes(size=(id)),alpha = 0.5, space=2)
#' }
geom_textbarbed <- function (mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm=FALSE, lineend = "butt",
                             show.legend=NA, inherit.aes=TRUE, ...,
                             arrow = NULL) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextBarbed,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      lineend = lineend,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextBarbed <- ggplot2::ggproto(
  "GeomTextBarbed", ggplot2:::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    fill=NA, colour="black", alpha = NA,
    linewidth=.5, linetype=1, space=1, size=3.88,
    angle = 0, hjust = 0.5, vjust = 0.5,
    family = "", fontface = 1, lineheight = 1.2),

  draw_group = function(data, panel_scales, coord, arrow = NULL,
                        lineend="butt", linejoin="round", linemitre=1,
                        parse = FALSE,
                        na.rm = FALSE, label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines")) {

    data$shape = NA

    ggname("geom_textbarbed", grobTree(
      GeomText$draw_panel(data,panel_scales,coord),
      GeomBarbed$draw_group(data,panel_scales,coord)
      ))
  },

  draw_key = function(data, ...) {
    data$arrow <- NULL
    with(data,grobTree(
      barbedGrob(
        c(.2,.8), c(.5,.5), size=unit(size, "mm"), pch=NA,
        arrow = NULL, space = space, default.units="npc",
        gp=gpar(col=alpha(colour, alpha),
                lwd=linewidth * .pt,
                lty=linetype,
                fill = alpha(fill, alpha),
                fontsize = size * .pt)),
      textGrob(
        "a",
        c(.2,.8), c(.5,.5), default.units = "native",
        hjust = hjust, vjust = vjust,
        rot = angle,
        gp = gpar(
          col = alpha(colour, alpha),
          fontsize = size * .pt,
          fontfamily = family,
          fontface = fontface,
          lineheight = lineheight
        ),
        check.overlap = FALSE
      )
    )
    )
  }
)





# barbedGrob <- function (
#   x = stats::runif(10), y = stats::runif(10),
#   size = unit(sample(1:4, 10, replace = TRUE), "char"),
#   pch = 21, arrow = NULL, space = 1, only.lines = FALSE,
#   gp = gpar(), name = NULL, default.units = "npc",
#   vp = NULL, id=NULL)
# {
#   n <- length(x)
#   if (n > 1) {
#     if (length(size) < n)
#       size <- rep(size, length = n)
#     dx <- diff(x)
#     dy <- diff(y)
#     new.x <- rep(x, each = 2)[-c(1, 2 * length(x))]
#     new.y <- rep(y, each = 2)[-c(1, 2 * length(y))]
#     sizex <- convertUnit(size, default.units, "x", "dimension",
#                          valueOnly = TRUE)
#     sizey <- convertUnit(size, default.units, "y", "dimension",
#                          valueOnly = TRUE)
#     new.sizex <- rep(sizex, each = 2, length = 2 * n)[-c(1,
#                                                          2 * n)]
#     new.sizey <- rep(sizey, each = 2, length = 2 * n)[-c(1,
#                                                          2 * n)]
#     length <- sqrt(dx^2 + dy^2)
#     exclusionx <- 0.5 * rep(space, length(new.sizex)) * new.sizex
#     exclusiony <- 0.5 * rep(space, length(new.sizey)) * new.sizey
#     length.exc <- sqrt(exclusionx^2 + exclusiony^2)
#     scaling <- length.exc/rep(length, each = 2)
#     start <- seq(1, by = 2, length(new.x))
#     end <- seq(2, by = 2, length(new.x))
#     x.start <- scaling[start] * dx[(start + 1)/2] + new.x[start]
#     y.start <- scaling[start] * dy[(start + 1)/2] + new.y[start]
#     x.end <- new.x[end] - scaling[end] * dx[end/2]
#     y.end <- new.y[end] - scaling[end] * dy[end/2]
#     grob.lines <- segmentsGrob(
#       x0 = x.start, y0 = y.start,
#       x1 = x.end, y1 = y.end, arrow = arrow,
#       default.units = default.units,
#       gp = gpar(lineend = "butt"),
#       name = "lines")
#   }
#   else {
#     grob.lines <- NULL
#   }
#   if (!only.lines) {
#     grob.points <- pointsGrob(
#       x, y, size = size, default.units = default.units,
#       pch = pch, gp = gpar(lty = 1, linejoin = "mitre"),
#       name = "points")
#   }
#   else {
#     grob.points <- NULL
#   }
#   children <- gList(grob.lines, grob.points)
#   gTree(x = x, y = y, size = size, pch = pch, space = space, id=id,
#         only.lines = only.lines, name = name, default.units = default.units,
#         vp = vp, gp = gp, children = children, cl = "barbed")
# }
#
# compute_just <- function(just, x) {
#   inward <- just == "inward"
#   just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
#   outward <- just == "outward"
#   just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]
#
#   unname(c(left = 0, center = 0.5, right = 1,
#            bottom = 0, middle = 0.5, top = 1)[just])
# }
#
# just_dir <- function(x, tol = 0.001) {
#   out <- rep(2L, length(x))
#   out[x < 0.5 - tol] <- 1L
#   out[x > 0.5 + tol] <- 3L
#   out
# }
#
#
#
#
#
#
#
#
# labelGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
#                       just = "center", padding = unit(0.25, "lines"), r = unit(0.1, "snpc"),
#                       default.units = "npc", name = NULL,
#                       text.gp = gpar(), rect.gp = gpar(fill = "white"), vp = NULL) {
#
#   stopifnot(length(label) == 1)
#
#   if (!is.unit(x))
#     x <- unit(x, default.units)
#   if (!is.unit(y))
#     y <- unit(y, default.units)
#
#   gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
#         name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob")
# }

