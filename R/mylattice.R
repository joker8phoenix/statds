#' my lattice Library
#
# ltheme <- canonical.theme(color = FALSE) ## in-built B&W theme
# ltheme$strip.background$col <- "transparent" ## change strip bg
# ltheme$fontsize$text <- 18
# ltheme$fontsize$points <- 12
# lattice.options(default.theme = ltheme) ## set as default

"DMlatticeOptions" <- function(superpose.polygon.col=NULL,
                               superpose.line.col=NULL) {
  require(lattice)
  require(RColorBrewer)
  ltheme = canonical.theme(color=TRUE)
  
  if (!is.null(superpose.polygon.col))
    ltheme$superpose.line$col =  superpose.line.col else
      ltheme$superpose.line$col =
    c('black',"red","blue","#e31111","darkgreen", "gray")
  #  ltheme$superpose.line$col = rev(brewer.pal(8,"Set1"))
  ltheme$superpose.fill$col = ltheme$superpose.line$col
  if (!is.null(superpose.polygon.col))
    ltheme$superpose.polygon$col =  superpose.polygon.col
  ltheme$strip.shingle$col = ltheme$superpose.polygon$col
  
  ltheme$superpose.symbol$pch = c(16,17,18,1,2,3,4,8)
  ltheme$superpose.symbol$col = ltheme$superpose.line$col 
  ltheme$superpose.symbol$cex = 0.4
  ltheme$strip.background$col = c("gray90", "gray80")
  ltheme$background$col = "transparent"
  ltheme$par.main.text$cex = 0.9 # default is 1.2
  ltheme$par.ylab.text$cex =0.8
  ltheme$par.ylab.text$cex =0.8
  ltheme$add.text$cex = 1
  ltheme$axis.text$cex = 0.6
  ltheme$box.rectangle$col = "black"
  ltheme$box.umbrella$col = "black"
  ltheme$dot.symbol$col = "black"
  
  ltheme$plot.symbol$col = "black"
  ltheme$plot.line$col = "black"
  ltheme$plot.symbol$cex = 0.3
  ltheme$plot.symbol$pch = c(16)
  
  ltheme$plot.polygon$col = "#A6D96A"
  ltheme$par.sub.text$cex=0.7
  ltheme$par.sub.text$font=1
  
  lattice.options(default.theme=ltheme)
}



'plot.box.and.whiskers' <-
  function(
    points.x = x,
    points.y = y,
    minimum.required = 6,
    black.and.white = black.and.white.in.panel,
    box.ratio = 1,
    box.width = box.ratio/(1+box.ratio),
    outlier.cex = 1
  ){
    cat("\nPlotting Box and Whiskers\n")
    ### Box Plot Box Color (Default color is blue)
    box.rectangle <- trellis.par.get("box.rectangle")
    box.rectangle$col <- "black"
    trellis.par.set("box.rectangle", box.rectangle)
    
    ### Box Plot Outliers
    outlier.color <- "black"
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.symbol$col <- outlier.color
    plot.symbol$cex <- outlier.cex
    trellis.par.set("plot.symbol", plot.symbol)
    
    ### Box Plot Whiskers
    whisker.color <- "black"
    box.umbrella <- trellis.par.get("box.umbrella")
    box.umbrella$col <- whisker.color
    trellis.par.set("box.umbrella", box.umbrella)
    
    
    if(black.and.white){
      boxfill.color <- "grey 90"
    }else{
      boxfill.color <- "light yellow"
    }
    
    table.points.x <- table(points.x)
    locations.x <- table.points.x[table.points.x >= minimum.required]
    x.names <- as.numeric(names(locations.x))
    
    if(length(x.names) > 0){
      x.box.points <- points.x[points.x %in% x.names]
      y.box.points <- points.y[points.x %in% x.names]
      ### Select only locations that contained the minimum requirement
      
      #cat("Table of X.Points\n");print(table.points.x)
      #cat("X Points that Passed");print(x.names)
      #cat("x.box.points");print(x.box.points)
      #cat("y.box.points");print(y.box.points)
      
      panel.bwplot(
        x = x.box.points,
        y = y.box.points,
        horizontal = FALSE,
        color = "black",  ### Color of "dot at median"
        pch="|",  ### Creates a horizontal line at the median, instead of a solid dot.  Omit if you like the 'dot' better.
        fill = boxfill.color,
        box.ratio = box.ratio,
        box.width = box.width
      )
    }else{
      cat("\nWarning!\n
          \tPlease lower the minimum required for the box and whisker plot to allow boxes to be plotted.\n
          \t(Only if desired)\n")
    }
  }


'lines.by.id' <-
  function(
    points.x = x,
    points.y = y,
    ids = groups,
    rows.being.plotted = subscripts,
    black.and.white = black.and.white.in.panel
  ){
    cat("\nPlotting Lines Connected by ID\n")
    
    unique.ids <- unique(ids)
    
    if(black.and.white){ 
      colors.unique <- c(rep(1,length(unique.ids) ))
      line.style <- "dashed"
    }else{
      colors.unique <- rainbow( length(unique.ids) ,start = .56,end = .13)
      #Colors blue to purple to red to orange, no yellow and green
      line.style <- "solid"
    }
    
    panel.superpose(
      x = points.x,
      y = points.y,
      subscripts = rows.being.plotted,
      groups = ids,
      panel.groups = "panel.lines", # Panel used to plot the data
      col = colors.unique,
      pch = NA,
      lty = line.style,
      type = "l",
      alpha = 1  # The opacity of the lines
    )
  }
