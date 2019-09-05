#'
#' ggplot2 themes in statds
#'
mytheme_common_text <- function(base_size = 20,
                                base_family = "",
                                #"mono",#serif or sans
                                number_family = "",
                                #"mono",#serif or sans
                                legend = c(0.95, 0.05),
                                #"right",
                                margin.top = 1,
                                margin.side = 1,
                                angle.text.x = 0,
                                angle.text.y = 0,
                                angle.title.x = 0,
                                angle.title.y = 90) {
  half_line <- base_size / 2
  axis.x.hjust <- NULL
  axis.y.hjust <- NULL
  if (angle.text.x != 0) {
    axis.x.hjust = 0
  }
  if (angle.text.x == 90) {
    axis.x.hjust = 0.95
  }
  if (angle.text.y != 0) {
    axis.y.hjust = 1
  }
  if (angle.text.y == 90) {
    axis.y.hjust = 0.5
  }

  theme(
    text = element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    axis.text = element_text(
      family = number_family,
      size = rel(0.8),
      colour = "black"
    ),
    axis.text.x = element_text(
      margin = margin(t = 0.8 * half_line / 2),
      angle = angle.text.x,
      hjust = axis.x.hjust,
      vjust = 0.2
    ),
    axis.text.y = element_text(
      margin = margin(r = 0.8 * half_line / 2),
      hjust = axis.y.hjust,
      angle = angle.text.y
    ),
    axis.title = element_text(family = base_family, size = base_size),
    axis.title.x = element_text(
      family = base_family,
      margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2),
      angle = angle.title.x,
      vjust = -1.0,
      hjust = 0.5
    ),
    axis.title.y = element_text(
      family = base_family,
      vjust = 1,
      hjust = 0.5,
      angle = angle.title.y,
      margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
    ),


    legend.background = element_rect(colour = NA),
    legend.spacing = unit(0.4, "cm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(6, 6, 6, 6, "pt"),
    # legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(1.2, "lines"),
    legend.text = element_text(family = base_family, size = rel(0.8)),
    legend.title = element_blank(),
    legend.position = legend,
    legend.justification = round(legend),
    legend.box.just = "right",
    legend.box.background = element_rect(colour = "grey50"),
    #
    strip.text = element_text(
      family = base_family,
      colour = "black",
      size = rel(0.8)
    ),
    strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
    strip.text.y = element_text(
      angle = -90,
      margin = margin(l = half_line, r = half_line)
    ),
    plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 1.2)),
    plot.margin = margin(
      half_line * margin.top,
      half_line * margin.top,
      half_line * margin.side,
      half_line * margin.side
    ),
    complete = TRUE
  )
}

mytheme_grey <- function (base_size = 20,
                          base_family = "",
                          #"mono",#serif or sans
                          number_family = "",
                          #"mono",#serif or sans
                          legend = c(0.95, 0.05),
                          margin.top = 1,
                          margin.side = 1,
                          angle.text.x = 0,
                          angle.text.y = 0,
                          angle.title.x = 0,
                          angle.title.y = 90,
                          panel.border.colour = NA,
                          #"grey50",
                          panel.grid.major.colour = NA,
                          #"grey50",
                          panel.grid.minor.colour = NA) {
  #"grey90") {
  half_line <- base_size / 2
  mytheme_common_text(
    base_size,
    base_family,
    number_family,
    legend,
    margin.top,
    margin.side,
    angle.text.x,
    angle.text.y,
    angle.title.x,
    angle.title.y
  ) %+replace%
    theme(
      line = element_line(
        colour = "black",
        size = 0.5,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = "black",
        size = 0.5,
        linetype = 1
      ),
      #
      axis.line = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks = element_line(colour = "grey20"),
      axis.ticks.length = unit(half_line / 2, "pt"),
      #
      panel.background =   element_rect(fill = "grey92", colour = NA),
      panel.border =       element_blank(),
      panel.grid.major =   element_line(colour = "white"),
      panel.grid.minor =   element_line(colour = "white", size = 0.25),
      # panel.margin =       unit(half_line, "pt"),
      # panel.margin.x =     NULL,
      # panel.margin.y =     NULL,
      panel.spacing = unit(half_line, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop    =     FALSE,
      #
      strip.background = element_rect(fill = "grey85", colour = NA),
      strip.switch.pad.grid = unit(0.1, "cm"),
      strip.switch.pad.wrap = unit(0.1, "cm"),
      plot.background = element_rect(colour = "white"),
      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
mytheme_gray <- mytheme_grey

#' @export
#' @rdname ggtheme
mytheme_bw <- function (base_size = 20, ...)
{
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      axis.text = element_text(size = rel(0.8)),
      axis.ticks = element_line(colour = "black"),
      legend.key = element_rect(colour = "grey80"),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "grey50"),
      panel.grid.major = element_line(colour = "grey90", size = 0.2),
      panel.grid.minor = element_line(colour = "grey98", size = 0.5),
      strip.background = element_rect(fill = "grey85", colour = "grey20"),
      legend.key = element_rect(fill = "white", colour = NA),
      complete = TRUE
    )
}

#' @export
#' @rdname ggtheme
mytheme_linedraw <- function (base_size = 20, ...) {
  half_line <- base_size / 2
  # Starts with theme_grey and then modify some parts
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      axis.text         = element_text(colour = "black", size = rel(0.8)),
      axis.ticks        = element_line(colour = "black", size = 0.25),
      legend.key        = element_rect(colour = "black", size = 0.25),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(
        fill = NA,
        colour = "black",
        size = 0.5
      ),
      panel.grid.major  = element_line(colour = "black", size = 0.05),
      panel.grid.minor  = element_line(colour = "black", size = 0.01),
      strip.background  = element_rect(fill = "black", colour = NA),
      strip.text.x      = element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = element_text(
        colour = "white",
        angle = 90,
        margin = margin(l = half_line, r = half_line)
      )
    )
}

#' @export
#' @rdname ggtheme
mytheme_light <- function (base_size = 20, ...) {
  half_line <- base_size / 2
  # Starts with theme_grey and then modify some parts
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      axis.ticks        = element_line(colour = "grey70", size = 0.25),
      legend.key        = element_rect(
        fill = "white",
        colour = "grey50",
        size = 0.25
      ),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(
        fill = NA,
        colour = "grey70",
        size = 0.5
      ),
      panel.grid.major  = element_line(colour = "grey85", size = 0.25),
      panel.grid.minor  = element_line(colour = "grey93", size = 0.125),
      strip.background  = element_rect(fill = "grey70", colour = NA),
      strip.text.x      = element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = element_text(
        colour = "white",
        angle = 90,
        margin = margin(l = half_line, r = half_line)
      )
    )

}

#' @export
#' @rdname ggtheme
mytheme_minimal <- function (base_size = 20, ...) {
  # Starts with theme_bw and then modify some parts
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "lines")
    )
}

#' @export
#' @rdname ggtheme
mytheme_classic <- function (base_size = 20, ...) {
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      panel.border     = element_blank(),
      axis.line        = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(colour = "black", size = 0.5),
      legend.key       = element_blank()
    )
}

#' @export
#' @rdname ggtheme
mytheme_dark <- function (base_size = 20, ...) {
  half_line <- base_size / 2
  # Starts with theme_grey and then modify some parts
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      axis.ticks        = element_line(colour = "grey40", size = 0.25),
      legend.key        = element_rect(
        fill = "grey50",
        colour = "grey40",
        size = 0.25
      ),
      panel.background  = element_rect(fill = "grey50", colour = NA),
      panel.grid.major  = element_line(colour = "grey40", size = 0.25),
      panel.grid.minor  = element_line(colour = "grey45", size = 0.125),
      strip.background  = element_rect(fill = "grey20", colour = NA),
      strip.text.x      = element_text(
        colour = "white",
        margin = margin(t = half_line, b = half_line)
      ),
      strip.text.y      = element_text(
        colour = "white",
        angle = 90,
        margin = margin(l = half_line, r = half_line)
      )
    )
}

#' @export
#' @rdname ggtheme
mytheme_simple <- function (base_size = 20, ...) {
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      panel.border     = element_blank(),
      axis.line        = element_line(colour = "black"),
      axis.line.x      = element_blank(),
      axis.line.y      = element_blank(),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(colour = "black", size = 0.5),
      legend.key       = element_blank(),
      panel.background  = element_blank(),
      plot.background = element_blank()
    )
}

mytheme_simple_x <- function (base_size = 20, ...) {
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      panel.border     = element_blank(),
      axis.line        = element_line(colour = "black"),
      axis.line.x      = element_line(colour = "black"),
      axis.line.y      = element_blank(),
      #panel.grid.major = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      axis.text = element_blank(),
      strip.background = element_rect(colour = "black", size = 0.5),
      panel.background  = element_blank(),
      plot.background = element_blank(),
      legend.key       = element_blank()
    )
}

mytheme_simple_y <- function (base_size = 20, ...) {
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      panel.border     = element_blank(),
      axis.line        = element_line(colour = "black"),
      axis.line.x      = element_blank(),
      axis.line.y      = element_line(colour = "black"),
      #panel.grid.major = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks        = element_blank(),
      #axis.ticks.length = unit(0, "lines"),
      axis.text = element_blank(),
      strip.background = element_rect(colour = "black", size = 0.5),
      panel.background  = element_blank(),
      plot.background = element_blank(),
      legend.key       = element_blank()
    )
}

mytheme_simple_xy <-
  function (base_size = 20,
            colour = "grey70",
            size = 0.5,
            linetype = 1,
            ticks = F,
            ...) {
    mytheme_grey(base_size = base_size, ...) %+replace%
      theme(
        panel.border     = element_blank(),
        axis.line        = element_line(
          colour = colour,
          size = size,
          linetype = linetype
        ),
        axis.line.x      = element_line(colour = colour),
        axis.line.y      = element_line(colour = colour),
        #panel.grid.major = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = if (!ticks)
          element_blank()
        else
          element_line(colour = "grey40", size = 0.25),
        axis.ticks.length = if (!ticks)
          unit(0, "lines")
        else
          unit(0.5, "lines"),
        axis.text = if (!ticks)
          element_blank()
        else
          element_text(colour = "black", size = rel(0.8)),
        strip.background = element_rect(colour = colour, size = 0.5),
        panel.background  = element_blank(),
        legend.key       = element_blank(),
        plot.background = element_blank()
      )
  }

mytheme_seaborn <- function (base_size = 20,
                             base_family = "",
                             #"mono",#serif or sans
                             number_family = "",
                             #"mono",#serif or sans
                             legend = c(0.95, 0.05),
                             margin.top = 1,
                             margin.side = 1,
                             angle.text.x = 0,
                             angle.text.y = 0,
                             angle.title.x = 0,
                             angle.title.y = 90,
                             panel.border.colour = NA,
                             #"grey50",
                             panel.grid.major.colour = NA,
                             #"grey50",
                             panel.grid.minor.colour = NA,
                             #"grey90"
                             ...) {
  half_line <- base_size / 2
  mytheme_grey(
    base_size = base_size,
    base_family = base_family,
    number_family = number_family,
    legend = legend,
    margin.top = margin.top,
    margin.side = margin.side,
    angle.text.x = angle.text.x,
    angle.text.y = angle.text.y,
    angle.title.x = angle.title.x,
    angle.title.y = angle.title.y,
    panel.border.colour = panel.border.colour,
    panel.grid.major.colour = panel.grid.major.colour,
    panel.grid.minor.colour = panel.grid.minor.colour,
    ...
  ) %+replace%
    theme(
      # Elements in this first block aren't used directly, but are inherited
      # by others
      line = element_line(
        colour = "black",
        size = 0.5,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = "black",
        size = 0.5,
        linetype = 1
      ),
      text = element_text(
        family = base_family,
        face = "plain",
        colour = "black",
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
      #title=element_text(),
      #aspect.ratio=1,
      axis.line = element_line(colour = "black", size = 0.75),
      axis.line.x = element_line(colour = "black", size = 0.75),
      axis.line.y = element_line(colour = "black", size = 0.75),
      # axis.text = element_text(
      #   family=number_family,
      #   size = rel(0.8),
      #   colour = "black"),
      # # axis.text.x = element_text(
      #   margin = margin(t = 0.8 * half_line / 2),
      #   angle = angle.text.x,
      #   hjust=0.95,vjust=0.2),
      # axis.text.y = element_text(
      #   margin = margin(r = 0.8 * half_line / 2),
      #   hjust = 1,
      #   angle=angle.text.y),
      axis.ticks = element_line(colour = "black", size = 0.5),
      axis.ticks.length = unit(half_line / 1, "pt"),
      # axis.ticks.x = element_line(),
      # axis.ticks.y = element_line(),
      # axis.line =          element_blank(),
      # axis.line.x =          element_line(),
      # axis.line.y =          element_line(),
      # axis.title = element_text(family=base_family, size=base_size, colour="black"),#"grey30"),
      axis.title.x = element_text(
        family = base_family,
        margin = margin(t = 0.8 * half_line, b = 0.8 * half_line / 2),
        angle = angle.title.x,
        vjust = -1.0,
        hjust = 0.5
      ),
      axis.title.y = element_text(
        family = base_family,
        vjust = 1,
        hjust = 0.5,
        angle = angle.title.y,
        margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
      ),
      # legend.background = element_rect(colour = NA),
      # legend.margin = unit(0.2, "cm"),
      # legend.key = element_rect(fill = "grey95", colour = "white"),
      # legend.key.size = unit(1.2, "lines"),
      # legend.key.height = NULL,
      # legend.key.width =   NULL,
      legend.text =        element_text(family = base_family, size = rel(0.8)),
      # legend.text.align =  NULL,
      # legend.title =       element_text(family=base_family, hjust=0),
      # legend.title.align = NULL,
      # legend.position =    legend,
      # legend.direction =   NULL,
      # legend.justification = "center",
      # legend.box =         NULL,
      #legend.box.just = "top",
      panel.background =   element_blank(),
      panel.border =       element_blank(),
      panel.grid.major =   element_line(colour = "grey80", size = 0.25),
      #panel.grid.major.x =   element_line(),
      #panel.grid.major.y =   element_line(),
      panel.grid.minor =   element_line(colour = "grey90", size = 0.1),
      #panel.grid.minor.x =   element_line(size = 0.25),
      #panel.grid.minor.y =   element_line(size = 0.25),
      # panel.margin =       unit(half_line, "pt"),
      # panel.margin.x =     NULL,
      # panel.margin.y =     NULL,
      # panel.grid =   element_line(colour = "white"),
      panel.ontop    =     FALSE,
      strip.background = element_blank(),
      #element_rect(fill = "grey85", colour=NA),
      strip.text = element_text(colour = "black", size = rel(0.8)),
      strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
      strip.text.y = element_text(
        angle = -90,
        margin = margin(l = half_line, r = half_line)
      ),
      strip.switch.pad.grid = unit(0.1, "cm"),
      strip.switch.pad.wrap = unit(0.1, "cm"),
      plot.background = element_rect(colour = "white"),
      plot.title = element_text(size = rel(1.2),
                                margin = margin(b = half_line * 1.2)),
      plot.margin = margin(
        half_line * margin.top,
        half_line * margin.top,
        half_line * margin.side,
        half_line * margin.side
      ),
      complete = TRUE
    )
}

mytheme_simplest <-
  function (base_size = 20,
            col.panel.border = NA,
            col.axis.line = NA,
            col.axis.title = NA,
            col.strip = NA,
            ...) {
    element_panel.border = element_blank()
    if (!is.na(col.panel.border))
      element_panel.border = element_rect(fill = NA,
                                          colour = col.panel.border,
                                          size = 0.5)
    element_axis.line = element_line(colour = col.axis.line)
    element_axis.title = element_text(size = base_size, colour = col.axis.title)
    element_strip = element_blank()
    if (!is.na(col.strip))
      element_strip = element_text(colour = col.strip, size = rel(0.8))
    mytheme_grey(base_size = base_size, ...) %+replace%
      theme(
        panel.border = element_panel.border,
        axis.title.x = element_axis.title,
        axis.title.y = element_axis.title,
        axis.line = element_axis.line,
        axis.line.x      = element_axis.line,
        axis.line.y      = element_axis.line,
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        axis.ticks        = element_blank(),
        axis.ticks.length = unit(0, "lines"),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_strip,
        legend.key       = element_blank(),
        panel.background  = element_blank(),
        plot.background = element_blank()
      )
  }

mytheme_JPR <- function(base_size = 12,
                        base_family = "") {
  half_line <- base_size / 2
  mytheme_grey(base_size = base_size, ...) %+replace%
    theme(
      line = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid",
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      text = element_text(
        family = base_family,
        face = "plain",
        colour = "black",
        size = base_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = margin(),
        debug = FALSE
      ),
      axis.title.x = element_text(vjust = 0),
      axis.title.y = element_text(vjust = 0, angle = 90),
      axis.text = element_text(size = rel(1)),
      axis.text.x = element_text(margin = margin(t = 0.8 * half_line / 2), vjust = 1),
      axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line /
                                                       2), vjust = 0),
      axis.text.y = element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line /
                                                         2), hjust = 0),
      axis.ticks = element_line(size = 1),
      axis.ticks.length = unit(-0.2, "cm"),
      # axis.ticks.margin = unit(0.4, "cm"),
      axis.line = element_line(
        colour = "#000000",
        size = 1,
        linetype = "solid",
        lineend = "round"
      ),
      legend.background = element_rect(colour = NA),
      # legend.margin = unit(0.2, "cm"),
      legend.key = element_rect(fill = NA, colour = NA),
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = rel(0.8)),
      legend.title = element_blank(),
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_blank(),
      # panel.margin = unit(0.25, "lines"),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.background = element_rect(colour = "white"),
      plot.title = element_text(size = rel(1.2)),
      # plot.margin = unit(c(1, 1, 1, 1), "lines"),
      plot.margin = margin(half_line, half_line, half_line, half_line),
      strip.background = element_rect(
        fill = NA,
        colour = "black",
        size = 1
      ),
      strip.text = element_text(size = rel(0.8)),
      strip.text.y = element_text(angle = -90),
      complete = TRUE
    )
}
