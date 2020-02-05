#' geom_polygonH
geom_polygonh <- 
  function (mapping = NULL, data = NULL, stat = "hatch", 
            position = "identity", show.legend = NA, na.rm=FALSE, 
            inherit.aes = TRUE, ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomPolygonH,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    )
  }

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPolygonH <- ggplot2::ggproto(
  "GeomPolygonH", ggplot2::Geom,
  
  #draw_groups <- function(., ...) .$draw(...)
  draw_group = function(data, panel_scales, coord) {
    n <- nrow(data)
    if (n == 1) return(zeroGrob())
    
    # print(pol2pols(data))
    # print(str(pol2pols(data)))
    data2 <- hatch2(pol2pols(data),panel_scales)
    
    #     if(!is.numeric(data$border)){
    #       data$linetype=as.numeric(data$border)
    #     }
    # else{data$linetype=data$border}
    # data$fill=NA
    
    # Check if group is numeric, to make polygonGrob happy (factors are numeric,
    # but is.numeric() will report FALSE because it actually checks something else)
    if (mode(data$group) != "numeric")
      data$group <- factor(data$group)
    
    
    munched <- coord_munch(coord, data, panel_scales)
    # Sort by group to make sure that colors, fill, etc. come in same order
    munched <- munched[order(munched$group), ]
    
    # For gpar(), there is one entry per polygon (not one entry per point).
    # We'll pull the first value from each group, and assume all these values
    # are the same within each group.
    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]
    
    #     print(data)
    #     print(data2)
    #     print(first_rows)
    
    ggname("geom_polygonh",gTree(children = gList(
      polygonGrob(munched$x, munched$y, default.units = "native", id = munched$group,
                  gp = gpar(
                    col = first_rows$border,
                    fill = first_rows$fill,
                    lwd = first_rows$size * .pt,
                    lty = first_rows$linetype
                  )),
      ggplot2:::GeomSegment$draw_panel(data2, panel_scales, coord)
    )))
  },
  
  default_aes = ggplot2::aes(colour = "black", fill = NA, size = 0.5, linetype = 1, alpha = NA),
  required_aes = c("x", "y"),
  
  draw_key = draw_key_hatch
)

pol2pols <- function(data){
  groups <- unique(data$group)
  df=data.frame()
  for(i in groups){
    tmp=data[data$group==i,]
    df=rbind(df,tmp,rep.int(NA,dim(tmp)[1]))
  }
  df1=data.frame(x=df$x,y=df$y)
  df2=data.frame(data[, setdiff(names(data), c("x","y"))],row.names=NULL)[1L,]
  data.frame(df1,df2,row.names=NULL)
}

