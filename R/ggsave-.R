myggsave <- function(filename=NULL, plot=last_plot(), device = NULL, path = NULL,
                     scale = 1, width=129.4427, height=80, units = "mm", 
                     dpi = 300, limitsize = TRUE, bg = "transparent", ...){
  
  width = width * scale
  height = height * scale
  
  if(!is.null(plot$grob)){
    ggsave(filename,plot,device,path,scale,width,height,units,dpi,limitsize,...)
  }else{
    g = ggplotGrob(plot)
    
    panels <- grep("panel", g$layout$name)
    panel_index_w <- unique(g$layout$l[panels])
    panel_index_h <- unique(g$layout$t[panels])
    nw <- length(panel_index_w)
    nh <- length(panel_index_h)
    
    width = unit(width/nw, units[1])
    height = unit(height/nh, units[1])
    margin = unit(1, "mm")
    
    if(getRversion() < "3.3.0"){
      # the following conversion is necessary
      # because there is no `[<-`.unit method
      # so promoting to unit.list allows standard list indexing
      g$widths <- grid:::unit.list(g$widths)
      g$heights <- grid:::unit.list(g$heights)
      
      g$widths[panel_index_w] <-  rep(list(width),  nw)
      g$heights[panel_index_h] <- rep(list(height), nh)
    } else {
      g$widths[panel_index_w] <-  rep(width,  nw)
      g$heights[panel_index_h] <- rep(height, nh)
    }
    
    if(!is.null(filename))
      ggsave(filename, g, #device = device, path = path, scale = scale, 
             width = convertWidth(sum(g$widths) + margin, unitTo = "in", valueOnly = TRUE),
             height = convertHeight(sum(g$heights) + margin, unitTo = "in", valueOnly = TRUE), 
             bg = bg, ...)#,
    #units = units, dpi = dpi, limitsize = limitsize, ...)
    invisible(g)
  }
  
}

print.fixed <- function(x) grid.draw(x)