stat_boxplotj <- stat_boxplotJ <- function(mapping = NULL, data = NULL, geom = "boxplotJ",
  position = "dodge", na.rm = FALSE, coef = 1.5, show.legend = NA,
  ...,
  inherit.aes = TRUE)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      coef = coef,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBoxplotJ <- ggplot2::ggproto("StatBoxplotJ", ggplot2::Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    params$width <- params$width %||% resolution(data$x) * 0.75
    params
  },

  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5) {
    qs <- c(0, 0.25, 0.5, 0.75, 1)

    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
      stats <- as.numeric(stats::coef(mod))
    } else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])

    statsEx <- c(stats[2] - 2 * coef * iqr, stats[2] - coef * iqr, stats[4] + coef * iqr, stats[4] + 2 * coef * iqr)
    names(statsEx) <- c("far lower fence", "lower fence", "upper fence", "far upper fence")

    # outliers <- (data$y < statsEx[2] && data$y > statsEX[1]) | (data$y > statsEx[3] && data$y < statsEx[4])
    outliers <- (data$y < statsEx[2]) | (data$y > statsEx[3])
    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
    }

    outliersEx <- data$y < statsEx[1] | data$y > statsEx[4]

    len <- ifelse(na.rm, length(data$y), length(!is.na(data$y)))
    mu <- mean(data$y,na.rm=na.rm)
    s <- sd(data$y,na.rm = na.rm)/sqrt(len)
    triangle <- c(mu-qnorm(0.975)*s,mu,mu+qnorm(0.975)*s)
    names(triangle) <- c("lower.mean", "mean", "upper.mean")

    if (length(unique(data$x)) > 1)
      width <- diff(range(data$x)) * 0.9

    df <- as.data.frame(as.list(stats))
    df <- cbind(df,as.data.frame(as.list(statsEx)))
    df$outliers <- list(data$y[outliers])
    df$outliersEx <- list(data$y[outliersEx])
    df$triangle <- list(triangle)

    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    } else {
      # Sum up weights for non-NA positions of y and weight
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }

    df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
    df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

    df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df
  }
)
