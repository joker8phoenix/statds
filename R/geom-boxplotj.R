#' Box and whiskers plot.
#'
#' The lower and upper "hinges" correspond to the first and third quartiles
#' (the 25th and 75th percentiles). This differs slightly from the method used
#' by the \code{boxplot} function, and may be apparent with small samples.
#' See \code{\link{boxplot.stats}} for for more information on how hinge
#' positions are calculated for \code{boxplot}.
#'
#' The upper whisker extends from the hinge to the highest value that is within
#' 1.5 * IQR of the hinge, where IQR is the inter-quartile range, or distance
#' between the first and third quartiles. The lower whisker extends from the
#' hinge to the lowest value within 1.5 * IQR of the hinge. Data beyond the
#' end of the whiskers are outliers and plotted as points (as specified by Tukey).
#'
#' In a notched box plot, the notches extend \code{1.58 * IQR / sqrt(n)}.
#' This gives a roughly 95% confidence interval for comparing medians.
#' See McGill et al. (1978) for more details.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "boxplot")}
#'
#' @seealso \code{\link{stat_quantile}} to view quantiles conditioned on a
#'   continuous variable, \code{\link{geom_jitter}} for another way to look
#'   at conditional distributions.
#' @inheritParams geom_point
#' @param geom,stat Use to override the default connection between
#'   \code{geom_boxplot} and \code{stat_boxplot}.
#' @param outlier.colour Override aesthetics used for the outliers. Defaults
#'   come from \code{geom_point()}.
#' @param outlier.shape Override aesthetics used for the outliers. Defaults
#'   come from \code{geom_point()}.
#' @param outlier.size Override aesthetics used for the outliers. Defaults
#'   come from \code{geom_point()}.
#' @param outlier.stroke Override aesthetics used for the outliers. Defaults
#'   come from \code{geom_point()}.
#' @param notch if \code{FALSE} (default) make a standard box plot. If
#'   \code{TRUE}, make a notched box plot. Notches are used to compare groups;
#'   if the notches of two boxes do not overlap, this suggests that the medians
#'   are significantly different.
#' @param notchwidth for a notched box plot, width of the notch relative to
#'   the body (default 0.5)
#' @param varwidth if \code{FALSE} (default) make a standard box plot. If
#'   \code{TRUE}, boxes are drawn with widths proportional to the
#'   square-roots of the number of observations in the groups (possibly
#'   weighted, using the \code{weight} aesthetic).
#' @export
#' @references McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of
#'     box plots. The American Statistician 32, 12-16.
#' @examples
#' p <- ggplot(mpg, aes(class, hwy))
#' p + geom_boxplot() + ylim(c(10,50))
#' p + geom_boxplotj() + ylim(c(10,50))
#' p + geom_boxplotj(fence = T) + ylim(c(10,50))
#' p + geom_boxplotj(fence = T, triangle = T) + ylim(c(10,50))
#' p + geom_boxplot() + geom_jitter(width = 0.2)
#' p + geom_boxplotj() + geom_jitterj(width = 0.2)
#' p + geom_boxplot() + coord_flip()
#' p + geom_boxplotj() + coord_flip()
#' p + geom_boxplotj(fence = T, triangle = T) + coord_flip()
#'
#' p + geom_boxplot(notch = TRUE)
#' p + geom_boxplotj(notch = TRUE)
#' p + geom_boxplotj(notch = TRUE, triangle = T)
#' p + geom_boxplot(varwidth = TRUE)
#' p + geom_boxplotj(varwidth = TRUE)
#' p + geom_boxplotj(varwidth = TRUE, triangle = T)
#' p + geom_boxplot(fill = "white", colour = "#3366FF")
#' p + geom_boxplotj(fill = "white", colour = "#3366FF", outlier.colour= "#3366FF")
#' p + geom_boxplotj(fill = "white", colour = "#3366FF", outlier.colour= "#3366FF", outlierEx.colour= "#3366FF")
#' p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
#' p + geom_boxplotj(outlier.colour = "red", outlier.shape = 1)
#'
#' # Boxplots are automatically dodged when any aesthetic is a factor
#' p + geom_boxplot(aes(fill = drv))
#' p + geom_boxplotj(aes(fill = drv))
#' p + geom_boxplotj(aes(fill = drv), fence = T)
#' p + geom_boxplotj(aes(fill = drv), fence = T, triangle = T)
#'
#' # You can also use boxplots with continuous x, as long as you supply
#' # a grouping variable. cut_width is particularly useful
#' ggplot(diamonds, aes(carat, price)) + geom_boxplot()
#' ggplot(diamonds, aes(carat, price)) + geom_boxplotj()
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplot(aes(group = cut_width(carat, 0.25)))
#' ggplot(diamonds, aes(carat, price)) +
#'   geom_boxplotj(aes(group = cut_width(carat, 0.25)))
#'
#' \donttest{
#' # It's possible to draw a boxplot with your own computations if you
#' # use stat = "identity":
#' y <- rnorm(100)
#' df <- data.frame(
#'   x = 1,
#'   y0 = min(y),
#'   y25 = quantile(y, 0.25),
#'   y50 = median(y),
#'   y75 = quantile(y, 0.75),
#'   y100 = max(y)
#' )
#' ggplot(df, aes(x)) +
#'   geom_boxplot(
#'    aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
#'    stat = "identity"
#'  )
#' ggplot(df, aes(x)) +
#'   geom_boxplotj(
#'    aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
#'    stat = "identity"
#'  )
#' }
geom_boxplotj <- 
  function(mapping = NULL, data = NULL, stat = "boxplotJ",
           triangle = FALSE, fence = FALSE,
           outlier.colour = "black", outlier.shape = 1,
           outlier.size = 2, outlier.stroke = 1, 
           outlierEx.colour = "black", outlierEx.shape = 18,
           outlierEx.size = 2, outlierEx.stroke = 1, 
           notch = FALSE, notchwidth = 0.5,
           position = "dodge", 
           ...,
           varwidth = FALSE, show.legend = NA, inherit.aes = TRUE, na.rm = TRUE)
  {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomBoxplotJ,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        outlier.colour = outlier.colour,
        outlier.shape = outlier.shape,
        outlier.size = outlier.size,
        outlier.stroke = outlier.stroke,
        outlierEx.colour = outlierEx.colour,
        outlierEx.shape = outlierEx.shape,
        outlierEx.size = outlierEx.size,
        outlierEx.stroke = outlierEx.stroke,
        notch = notch,
        notchwidth = notchwidth,
        varwidth = varwidth,
        na.rm = na.rm,
        triangle = triangle,
        fence = fence,
        ...
      )
    )
  }

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBoxplotJ <- ggplot2::ggproto(
  "GeomBoxplotJ", ggplot2::Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    
    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })
      
      data$ymin_final <- pmin(out_min, data$ymin)
      data$ymax_final <- pmax(out_max, data$ymax)
    }
    
    # if `varwidth` not requested or not available, don't use it
    if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
    } else {
      # make `relvarwidth` relative to the size of the largest group
      data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
      data$xmin <- data$x - data$relvarwidth * data$width / 2
      data$xmax <- data$x + data$relvarwidth * data$width / 2
    }
    data$width <- NULL
    if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL
    
    data
  },
  
  draw_group = function(data, panel_scales, coord, fatten = 2,
                        outlier.colour = "black", outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlierEx.colour = "black", outlierEx.shape = 19,
                        outlierEx.size = 1.5, outlierEx.stroke = 0.5,
                        notch = FALSE, notchwidth = 0.5, varwidth = FALSE,
                        triangle = FALSE, fence = FALSE) {
    # print(data)
    
    common <- data.frame(
      colour = data$colour,
      size = data$size,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )
    # print(common)
    whiskers <- data.frame(
      x = data$x,
      xend = data$x,
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      alpha = NA,
      linetype = 2,
      common,
      stringsAsFactors = FALSE
    )
    #print(head(data))
    sub.whiskers <- data.frame(
      x = data$xmin + (data$xmax-data$xmin)/4,
      xend = data$xmax - (data$xmax-data$xmin)/4,
      y = c(data$ymax,data$ymin),
      yend = c(data$ymax,data$ymin),
      alpha = NA,
      linetype = data$linetype,
      common,
      stringsAsFactors = FALSE
    )
    #print(sub.whiskers)

    if(fence){
      df.fence <- data.frame(
        x = data$xmin + (data$xmax-data$xmin)/6,
        xend = data$xmax - (data$xmax-data$xmin)/6,
        y = c(data$far.lower.fence,data$lower.fence, data$upper.fence, data$far.upper.fence),
        yend = c(data$far.lower.fence,data$lower.fence,data$upper.fence,data$far.upper.fence),
        alpha = NA,
        linetype = c(4,3,3,4),
        common,
        stringsAsFactors = FALSE
      )
      fence_grob = GeomSegment$draw_panel(df.fence, panel_scales, coord)
    }else{
      fence_grob = NULL
    }
    
    box <- data.frame(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      linetype = data$linetype,
      common,
      stringsAsFactors = FALSE
    )
    
    # print(data$outliers)
    # print(data$colour)
    # print(outlier.colour)
    # print(sub.whiskers)
    # print(data$outliers)
    # print(outlier.colour)
    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- data.frame(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = NA,
        stringsAsFactors = FALSE
      )
      if(!is.null(data$outliersEx) && length(data$outliersEx[[1]]>=1)){
        # print("outliersEx")
        outliersEx <- data.frame(
          y = data$outliersEx[[1]],
          x = data$x[1],
          colour = outlierEx.colour %||% data$colour[1],
          shape = outlierEx.shape %||% data$shape[1],
          size = outlierEx.size %||% data$size[1],
          stroke = outlierEx.stroke %||% data$stroke[1],
          fill = NA,
          alpha = NA,
          stringsAsFactors = FALSE
        )
        outliers <- outliersEx %>% dplyr::union(outliers)
      }
      # print(outliers)
      outliers_grob <- GeomPoint$draw_panel(outliers, panel_scales, coord)
    } else {
      outliers_grob <- NULL
    }
    
    
    if (triangle){#(!is.null(data$triangle) && length(data$triangle[[1]] >= 1)) {
      tr=data$triangle[[1]]
      # print(c(tr[1:3],tr[2],tr[1]))
      # print(c(data$x[1],data$xmin + (data$xmax-data$xmin)/4,data$x[1],data$xmax - (data$xmax-data$xmin)/4))
      df.triangle <- data.frame(
        y = c(tr[1:3],tr[2],tr[1]),
        x = c(data$x[1],data$xmin + (data$xmax-data$xmin)/4,data$x[1],data$xmax - (data$xmax-data$xmin)/4, data$x[1]),
        fill = NA,
        alpha = NA,
        linetype = data$linetype,
        common,
        stringsAsFactors = FALSE
      )
      # print(triangle)
      triangle_grob <- GeomPath$draw_panel(df.triangle, panel_scales, coord)
      
      df.mean <- data.frame(
        y = data$triangle[[1]][2],
        x = data$x[1],
        colour = data$colour[1],
        shape = 1,
        size = 2,#data$size[1],
        stroke = 1,
        fill = NA, 
        alpha = NA,
        stringsAsFactors = FALSE
      )
      mean_grob = GeomPointJ$draw_panel(df.mean, panel_scales, coord)
    } else {
      triangle_grob <- NULL
      mean_grob = NULL
    }
    
    ggname("geom_boxplotJ", grobTree(
      outliers_grob,
      fence_grob,
      GeomSegment$draw_panel(whiskers, panel_scales, coord),
      GeomSegment$draw_panel(sub.whiskers, panel_scales, coord),
      GeomCrossbar$draw_panel(box, fatten = fatten, panel_scales, coord),
      triangle_grob,
      mean_grob
    ))
  },
  
  draw_key = draw_key_boxplot,
  
  default_aes = ggplot2::aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                    alpha = NA, shape = 1, linetype = "solid", 
                    outlier.colour = "black", outlier.shape = 1, outlier.size = 1.5, outlier.stroke = 0.5,
                    outlierEx.colour = "black", outlierEx.shape = 2, outlierEx.size = 1.5, outlierEx.stroke = 0.5),
  
  required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)
