#' Surround with Hatch
#'
#' @param hatch as bins of hatch
#' @param angle as angle of hatch
#' @param fill as colour of hatch
#' @param border as linetype of borderline
#' @param colour as colour of borderline
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "rect")}
#'
#' @inheritParams geom_point
#' @export
#'
#' @examples
#' \donttest{
#' f1=function(x){-x+3.5}
#' f2=function(x){-2*x+4}
#' f3=function(x){-3*x+5}
#' f4=function(x){-0.5*x+4.5}
#' f5=function(x){1*x+3}
#' f6=function(x){3*x+1}
#'
#' x=seq(0,1,len=10)
#' y1=f1(x)
#' y2=f2(x)
#' y3=f3(x)
#' y4=f4(x)
#' y5=f5(x)
#' y6=f6(x)
#' dt1=data.table(x=x,y1=y1,y2=y2,group=1)
#' dt2=data.table(x=x,y1=y3,y2=y4,group=2)
#' dt3=data.table(x=x,y1=y5,y2=y6,group=3)
#' dt4=rbindlist(l=list(dt1,dt2,dt3))
#' dt4$angle=dt4$group*10
#' dt4 %>%
#'   ggplot(aes(x))+
#'   geom_hatch(aes(y=y1,yend=y2,group=group,
#'                  angle=angle,fill=factor(group),
#'                  linetype=factor(group)),
#'              border=3,hatch=3,colour="red")+
#'   mytheme()
#' }
geom_surroundh <- geom_surroundH <- function(mapping=NULL, data=NULL, stat="hatch",
                       position="identity",
                       ...,
                       show.legend=NA, na.rm=FALSE,
                       inherit.aes=TRUE){
  ggplot2::layer(
    data=data,
    mapping = mapping,
    stat = stat,
    geom=GeomSurroundH,
    position=position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomSurroundH <- ggplot2::ggproto(
  "GeomSurroundH",ggplot2:::Geom,
    required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
      colour="black", fill="grey", size=0.5, linetype=1, border=1, alpha = NA),

  draw_group = function(data, panel_scales, coord, na.rm = FALSE) {
    if (na.rm) data <- data[complete.cases(data[required_aes]), ]
    data <- data[order(data$group, data$x), ]
    data <- area_to_data(data)
    data2 <- hatch2(data,panel_scales)
    data$fill=NA
    # print(data)
    #data$linetype=1
    #data$linetype=data$border
    data2$colour=data2$fill
    if (mode(data$group) != "numeric")
      data$group <- factor(data$group)
    ggname("geom_surround", grobTree(
      ggplot2:::GeomSegment$draw_panel(data2, panel_scales, coord),
      ggplot2:::GeomPolygon$draw_panel(data, panel_scales, coord)
    ))
  },

  #default_stat <- function(.) StatHatch
  #required_aes <- c("x", "y")
  #default_aes <- function(.) aes(colour="black", fill="grey", size=0.5, linetype=1, alpha = NA)
  #guide_geom <- function(.) "hatch"

  draw_key = draw_key_hatch
  # draw_key = function(data, ...)  {
  #   #data <- aesdefaults(data, .$default_aes(), list(...))
  #   with(data, grobTree(
  #     linesGrob(x=c(0.125,0.125), y=c(0.875,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.125,0.250), y=c(0.750,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.125,0.375), y=c(0.625,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.125,0.500), y=c(0.500,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.125,0.625), y=c(0.375,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.125,0.750), y=c(0.250,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.125,0.875), y=c(0.125,0.875), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.250,0.875), y=c(0.125,0.750), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.375,0.875), y=c(0.125,0.625), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.500,0.875), y=c(0.125,0.500), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.625,0.875), y=c(0.125,0.375), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.750,0.875), y=c(0.125,0.250), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     linesGrob(x=c(0.875,0.875), y=c(0.125,0.125), gp = gpar(col = alpha(fill,alpha), lwd = size * .pt, lty = linetype)),
  #     rectGrob(height=0.750, width=0.750, gp = gpar(col = alpha(colour, alpha), fill = NA, lty = linetype))
  #   ))
  # }
)

area_to_data <- function(data){
  if(is.null(data$yend)){
    data$yend=0
  }
  df1=data.frame(x=c(data$x,rev(data$x)),y=c(data$yend,rev(data$y)))
  df2=data[data$x, setdiff(names(data), c("x","y","yend"))]
  data.frame(df1, df2, row.names=NULL)
}

