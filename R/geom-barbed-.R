#' geom_barbed
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
#' #rm(list = ls())
#' # geom_line() is suitable for time series
#' ggplot(economics, aes(date, unemploy)) + geom_line()
#' ggplot(economics, aes(date, unemploy)) + geom_barbed()
#' 
#' # Control line join parameters
#' df <- data.frame(x = 1:3, y = c(4, 1, 9))
#' base <- ggplot(df, aes(x, y))
#' base + geom_barbed(size = 10)
#' base + geom_barbed(size = 10, linewidth=8, linejoin = "mitre", lineend = "butt")
#' 
#' # Use qplot instead
#' qplot(date, unemploy, data=economics, geom="barbed")
#' 
#' # Using economic data:
#' # How is unemployment and personal savings rate related?
#' qplot(unemploy/pop, psavert, data=economics, geom="barbed")
#' qplot(unemploy/pop, psavert, data=economics, geom="barbed", size=as.numeric(date))
#' 
#' # How is rate of unemployment and length of unemployment?
#' qplot(unemploy/pop, uempmed, data=economics, geom="barbed")
#' 
#' # geom_barbed removes missing values on the ends of a line.
#' # use na.rm = T to suppress the warning message
#' df <- data.frame(
#' x = 1:5,
#' y1 = c(1, 2, 3, 4, NA),
#' y2 = c(NA, 2, 3, 4, 5),
#' y3 = c(1, 2, NA, 4, 5),
#' y4 = c(1, 2, 3, 4, 5))
#' qplot(x, y1, data = df, geom = "barbed", arrow=arrow())
#' qplot(x, y2, data = df, geom = "barbed", arrow=arrow())
#' qplot(x, y3, data = df, geom = "barbed", arrow=arrow(), na.rm=T)
#' qplot(x, y4, data = df, geom = "barbed", arrow=arrow())
#' 
#' # Setting line type vs colour/size
#' # Line type needs to be applied to a line as a whole, so it can
#' # not be used with colour or size that vary across a line
#' x <- seq(0.01, .99, length=100)
#' df <- data.frame(x = rep(x, 2), y = c(qlogis(x), 2 * qlogis(x)), group = rep(c("a","b"), each=100))
#' p <- ggplot(df, aes(x=x, y=y, group=group))
#' 
#' # Should work
#' p + geom_path(aes(colour = x))
#' p + geom_barbed(aes(colour = x))
#' 
#' # Should fail
#' # should_stop(p + geom_line(aes(colour = x), linetype=2))
#' 
#' # Set aesthetics to fixed value
#' p + geom_barbed(colour = "red", size = 1)
#' 
#' # Using a time series
#' qplot(date, pop, data=economics, geom="barbed")
#' 
#' qplot(date, pop, data=economics, geom="barbed", log="y")
#' qplot(date, pop, data=subset(economics, date > as.Date("2006-1-1")), geom="barbed")
#' qplot(date, pop, data=economics, size=unemploy/pop, geom="barbed")
#' 
#' # A simple pcp example
#' y2005 <- runif(300, 20, 120)
#' y2010 <- y2005 * runif(300, -1.05, 1.5)
#' group <- rep(LETTERS[1:3], each = 100)
#' 
#' df <- data.table(id = seq_along(group), group, y2005, y2010)
#' library(reshape2) # for melt
#' dfm <- melt(df, id.var = c("id", "group"))
#' ggplot(dfm, aes(variable, value, group = id, colour = group)) +
#' geom_barbed(aes(size=(id)),alpha = 0.5, space=2)
#' }
geom_barbed <- function (mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm=FALSE, lineend = "butt", 
                         ...,
                         show.legend=NA, inherit.aes=TRUE,
                         arrow = NULL) {
  ggplot2::layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomBarbed,
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
GeomBarbed <- ggplot2::ggproto(
  "GeomBarbed", 
  
  ggplot2:::Geom, 
  
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    shape=1, fill=NA, colour="black", alpha = NA, 
    linewidth=.5, linetype=1,
    space=1, size=3),
  
  draw_group = function(data, panel_scales, coord, arrow = NULL, 
                        lineend="butt", linejoin="round", linemitre=1,
                        na.rm=FALSE) {
    if (!anyDuplicated(data$group)) {
      message_wrap("geom_barbed: Each group consist of only one observation. Do you need to adjust the group aesthetic?")
    }
    
    keep <- function(x) {
      # from first non-missing to last non-missing
      first <- match(FALSE, x, nomatch = 1) - 1
      last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
      c(
        rep(FALSE, first),
        rep(TRUE, last - first),
        rep(FALSE, length(x) - last))
    }
    
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    missing <- !stats::complete.cases(data[c("x", "y", 
                                             "size", "shape", 
                                             "linetype","linewidth",
                                             "colour","fill")])
    kept <- stats::ave(missing, data$group, FUN=keep)
    data <- data[kept, ]
    # must be sorted on group
    data <- plyr::arrange(data, group)
    
    if (!all(kept) && !na.rm) {
      warning("Removed ", sum(!kept), " rows containing missing values",
              " (geom_barbed).", call. = FALSE)
    }
    
    munched <- coord_munch(coord, data, panel_scales)
    
    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())
    
    # Work out whether we should use lines or segments
    attr <- plyr::ddply(munched, "group", function(df) {
      data.frame(
        solid = identical(unique(df$linetype), 1),
        constant = nrow(unique(df[, c("alpha","colour","size","linetype")])) == 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stop("geom_barbed: If you are using dotted or dashed lines",
           ", colour, size and linetype must be constant over the line",
           call.=FALSE)
    }
    
    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)
    
    if (!constant) {
      with(munched,
           barbedGrob(
             x, y, size=unit(size, "mm"), pch=shape, id=group,
             arrow = arrow, space = space, default.units="npc",
             gp=gpar(col=alpha(colour, alpha), 
                     fill = alpha(fill, alpha), 
                     lwd=linewidth * .pt,
                     lty=linetype, 
                     lineend = lineend,
                     linejoin = linejoin,
                     fontsize = size * .pt))
      )
    } else {
      id <- match(munched$group, unique(munched$group))
      barbedGrob(
        munched$x, munched$y, size=unit(munched$size, "mm"), 
        pch=munched$shape, id=id,
        arrow = arrow, space = munched$space, default.units="npc",
        gp=gpar(col=alpha(munched$colour, munched$alpha)[start], 
                lwd=munched$linewidth[start] * .pt,
                lty=munched$linetype[start], 
                fill = alpha(munched$fill, munched$alpha)[start], 
                fontsize = munched$size * .pt,
                lineend = lineend,
                linejoin = linejoin,
                linemitre = linemitre)
      )
    }
  },
  #default_stat <- function(.) StatIdentity#StatBarbed
  #default_aes <- function(.) aes(size=1, shape=1, fill=NA, colour="black", alpha = NA, linetype=1, space=1)
  #guide_geom <- function(.) "barbed"
  draw_key = function(data, ...) {
    #print(data)
    data$arrow <- NULL
    #data <- aesdefaults(data, .$default_aes(), list(...))
    with(data,grobTree(
      barbedGrob(
        c(.2,.8), c(.5,.5), size=unit(size, "mm"), pch=shape,
        arrow = NULL, space = space, default.units="npc",
        gp=gpar(col=alpha(colour, alpha), 
                lwd=linewidth * .pt,
                lty=linetype, 
                fill = alpha(fill, alpha), 
                fontsize = size * .pt)))
    )
  }
)


barbedGrob <- function (
  x = stats::runif(10), y = stats::runif(10), 
  size = unit(sample(1:4, 10, replace = TRUE), "char"), 
  pch = 21, arrow = NULL, space = 1, only.lines = FALSE, 
  gp = gpar(), name = NULL, default.units = "npc", 
  vp = NULL, id=NULL) 
{
  n <- length(x)
  if (n > 1) {
    if (length(size) < n) 
      size <- rep(size, length = n)
    dx <- diff(x)
    dy <- diff(y)
    new.x <- rep(x, each = 2)[-c(1, 2 * length(x))]
    new.y <- rep(y, each = 2)[-c(1, 2 * length(y))]
    sizex <- convertUnit(size, default.units, "x", "dimension", 
                         valueOnly = TRUE)
    sizey <- convertUnit(size, default.units, "y", "dimension", 
                         valueOnly = TRUE)
    new.sizex <- rep(sizex, each = 2, length = 2 * n)[-c(1, 
                                                         2 * n)]
    new.sizey <- rep(sizey, each = 2, length = 2 * n)[-c(1, 
                                                         2 * n)]
    length <- sqrt(dx^2 + dy^2)
    exclusionx <- 0.5 * rep(space, length(new.sizex)) * new.sizex
    exclusiony <- 0.5 * rep(space, length(new.sizey)) * new.sizey
    length.exc <- sqrt(exclusionx^2 + exclusiony^2)
    scaling <- length.exc/rep(length, each = 2)
    start <- seq(1, by = 2, length(new.x))
    end <- seq(2, by = 2, length(new.x))
    x.start <- scaling[start] * dx[(start + 1)/2] + new.x[start]
    y.start <- scaling[start] * dy[(start + 1)/2] + new.y[start]
    x.end <- new.x[end] - scaling[end] * dx[end/2]
    y.end <- new.y[end] - scaling[end] * dy[end/2]
    grob.lines <- segmentsGrob(
      x0 = x.start, y0 = y.start, 
      x1 = x.end, y1 = y.end, arrow = arrow, 
      default.units = default.units, 
      gp = gpar(lineend = "butt"), 
      name = "lines")
  }
  else {
    grob.lines <- NULL
  }
  if (!only.lines) {
    grob.points <- pointsGrob(
      x, y, size = size, default.units = default.units, 
      pch = pch, gp = gpar(lty = 1, linejoin = "mitre"), 
      name = "points")
  }
  else {
    grob.points <- NULL
  }
  children <- gList(grob.lines, grob.points)
  gTree(x = x, y = y, size = size, pch = pch, space = space, id=id,
        only.lines = only.lines, name = name, default.units = default.units, 
        vp = vp, gp = gp, children = children, cl = "barbed")
}
