#' @export
#' @rdname geom_tileh
geom_recth =
  function (mapping = NULL,
            data = NULL,
            stat = "Hatch",
            position = "identity",
            show.legend = NA,
            na.rm = FALSE,
            ...,
            inherit.aes = TRUE) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomRectH,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(# hatch = hatch,
        na.rm = na.rm,
        ...)
    )
  }

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRectH <- ggplot2::ggproto(
  "GeomRectH",
  ggplot2:::Geom,

  draw_group = function(self, data, panel_scales, coord) {
    # cat("coord$is_linear= ", coord$is_linear())
    if (!coord$is_linear()) {
      aesthetics <- setdiff(names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax"))

      polys <- plyr::alply(data, 1, function(row) {
        poly <- with(row, rect_to_poly(xmin, xmax, ymin, ymax))
        aes <- as.data.frame(row[aesthetics],
                             stringsAsFactors = FALSE)[rep(1, 5),]

        GeomPolygonH$draw_panel(cbind(poly, aes), panel_scales, coord)
      })
      ggname("barH", do.call("grobTree", polys))
    } else {
      coords = coord$transform(data, panel_scales)
      # cat(coords$xmin,coords$ymax,coords$xmax - coords$xmin,coords$ymax - coords$ymin,"\n")
      # cat(coords$colour,coords$fill, coords$alpha, coords$size, coords$linetype, "\n")
      grect = grid::rectGrob(
        coords$xmin,
        coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "native",
        just = c("left", "top"),
        gp = grid::gpar(
          col = coords$colour,
          # col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          lwd = coords$size * .pt,
          lty = coords$linetype,
          lineend = "butt"
        )
      )
      # print(panel_scales)
      # print(coord)
      # print(coords)
      # print(colnames(data))
      # print((rec2pol(data)))
      # print(colnames(rec2pol(data)))
      data2 <- hatch2(rec2pol(data), panel_scales, F)
      # print(data2)
      ggname("geom_rectH",
             grid::grobTree(
               #gTree(children = gList(
               grect,
               ggplot2:::GeomSegment$draw_panel(data2, panel_scales, coord)
             ))
    }
  },

  # default_aes = ggplot2::aes(hatch = NULL, colour="black", fill=NA, size=0.5, linetype=1, alpha = NA),
  default_aes = ggplot2::aes(
    colour = "black",
    fill = NA,
    size = 0.5,
    linetype = 1,
    alpha = NA,
    hatch = 3
  ),
  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  draw_key = draw_key_hatch
)



# @keyword internal
rec2pol <- function(data) {
  n <-
    range(length(data$xmin),
          length(data$xmax),
          length(data$ymin),
          length(data$ymax))
  if (n[1L] == 0)
    stop("invalid rectangle specification")
  n <- n[2L]
  x <-
    rbind(rep.int(NA, n), data$xmin, data$xmax, data$xmax, data$xmin)[-1L]
  y <-
    rbind(rep.int(NA, n), data$ymin, data$ymin, data$ymax, data$ymax)[-1L]
  df1 = data.frame(x, y)
  df2 = data.frame(data[, setdiff(names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax"))], row.names =
                     NULL)
  df2 = df2[gl(n, 5, (5 * n - 1)), ]
  df = data.frame(df1, df2, row.names = NULL)
  #print(df)
  df
}
