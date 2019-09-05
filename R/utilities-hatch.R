#' hatch
hatch2 <- function(data, panel_scales, verbose=FALSE,...){
  if(verbose) {
    cat("##############\n")
    cat("### hatch2 ###\n")
    cat("##############\n")
    print(data)
  }
  # print(data)
  x=data$x
  y=data$y
  xy <- xy.coords(x, y)
  angle=data$angle
  # cat("angle: ", angle,"\n")
  if(verbose) cat("angle: ", angle,"\n")
  if(is.null(angle)){
    angle = 45
    if(verbose) {
      cat("The angle is null.")
      cat("angle: ", angle,"\n")
    }
  }
  # cat("angle: ", angle,"\n")
  if(any(is.factor(angle)) | any(!is.numeric(angle))){
    angle=(as.numeric(angle)-1)*60+45
    if(verbose) {
      cat("The angle is factor or not numeric.")
      cat("angle: ", angle,"\n")
    }
  }
  # cat("angle: ", angle,"\n")
  hatch = data$hatch
  # print(hatch)
  start <- 1
  ends <- c(seq_along(xy$x)[is.na(xy$x) | is.na(xy$y)], length(xy$x) + 1)
  num.polygons <- length(ends)
  angle <- rep_len(angle, num.polygons)
  if(verbose) {
    cat("ends: ",ends,"\n")
    cat("num.polygon: ",num.polygons,"\n")
    cat("angle: ", angle,"\n")
  }
  i <- 1L
  df=data.frame()
  for (end in ends) {
    if(verbose){
      cat(sprintf("start= %d, end= %d\n",start, end))
    }
    if (end > start) {
      if(all(xy$y[start:(end-1)]==0)) {
        if(verbose){
          cat(xy$y[start:(end-1)])
          cat("all(xy$x[start:(end-1)]==0 --> next")
        }
        start <- end + 1
        next
      }
      if(verbose){
        cat("hatch= ",hatch,"\n")
        cat("x= ", xy$x[start:(end-1)], "\n")
        cat("y= ", xy$y[start:(end - 1)], "\n")
        cat("hatch[start]= ", hatch[i], "\n")
        cat("angle= ", angle[i],"\n")
      }
      # df1=polygon.fullhatch(xy$x[start:(end - 1)], xy$y[start:(end - 1)], hatch = hatch[start], angle = angle[start], panel_scales, verbose, ...)
      df1=polygon.fullhatch(xy$x[start:(end - 1)], xy$y[start:(end - 1)], hatch = hatch[i], angle = angle[i], panel_scales, verbose, ...)
      df2=data.frame(data[start:(end-1), setdiff(names(data), c("x","y"))],row.names=NULL)
      df2=df2[1L,]
      # print(df1)
      if(verbose) print(df1)
      if(length(df1$x)==0){
        start <- end + 1
        next
      }
      df1=data.frame(df1,df2)
      #df1$colour=df1$fill
      i <- i + 1
      df=rbind(df,df1)
      if(verbose) print(i)
    }
    #print(df)
    start <- end + 1
  }
  if(verbose)
    cat("hatching: angle= ", angle, ", hatch= ",hatch,"\n")
  df
}

polygon.fullhatch <- function(x, y, hatch, angle, panel_scales, verbose = FALSE, ...) {
  if(verbose) {
    cat("   ######### polygon.fullhatch #########\n")
    # cat("   ### (x, y)= ", x, ", y= ", y, ", hatch= ", hatch, ", angle= ", angle, "\n")
  }
  x <- c(x, x[1L])
  y <- c(y, y[1L])
  angle <- angle%%180
  if(verbose){
    print(t(data.frame(x,y)))
    cat("\thatch= ", hatch, "\n")
    cat(sprintf("\tangle\t%f\n",angle))
  }
  if (par("xlog") || par("ylog")) {
    warning("cannot hatch with logarithmic scale active")
    return()
  }
  usr <- par("usr")
  pin <- par("pin")
  upi <- c(usr[2L] - usr[1L], usr[4L] - usr[3L])/pin
  #print(upi)
  xdiff=diff(panel_scales$x.range)*0.06
  ydiff=diff(panel_scales$y.range)*0.04
  upi <- c(xdiff, ydiff)
  if(verbose){
    cat(sprintf("\txdiff\t%f\n\tydiff\t%f\n",xdiff,ydiff))
  }
  if (upi[1L] < 0)
    angle <- 180 - angle
  if (upi[2L] < 0)
    angle <- 180 - angle
  upi <- abs(upi)
  xd <- cos(angle/180 * pi) * upi[1L]
  yd <- sin(angle/180 * pi) * upi[2L]
  if(verbose){
    cat(sprintf("\txd\t%f\n\tyd\t%f\n\n",xd,yd))
  }

  rangex=panel_scales$x.range
  rangey=panel_scales$y.range
  if(!is.numeric(panel_scales$x.range)){
    rangex=range(as.numeric(factor(panel_scales$x.range)))
  }
  if(!is.numeric(panel_scales$y.range)){
    rangey=range(as.numeric(factor(panel_scales$y.range)))
  }
  if(verbose){
    # print(rangex)
    # print(diff(rangex))
    cat(sprintf("\tdiff(rangex)\t%f\n\tdiff(rangey)\t%f\n",diff(rangex)[1],diff(rangey)[1]))
  }

  if(is.null(hatch)){
    # print(panel_scales$x.range)
    theta=angle[1L]/180*pi
    if(verbose) {
      cat(sprintf("\tangle\t%f\n\txd\t%f\n\tyd\t%f\n\ttheta\t%f\n\n",angle, xd, yd, theta))
      # cat(sprintf("\thatch\t%f\n",5*yd/xd/diff(rangex)/abs(sin(theta))))
    }
    hatch= if(angle[1L]<45 | angle[1L]>135){
      5*xd/yd/diff(rangey)/abs(cos(theta))
    } else {5*yd/xd/diff(rangex)/abs(sin(theta))}
    if(verbose){
      cat(sprintf("\thatch\t%f\n\n",hatch))
    }
    # cat(sprintf("\thatch\t%f\n\n",hatch))
  }

  if (angle < 45 || angle > 135) {
    if (angle < 45) {
      first.x <- max(x)
      last.x <- min(x)
    } else {
      first.x <- min(x)
      last.x <- max(x)
    }
    y.shift <- upi[2L]/hatch/abs(cos(angle/180 * pi))
    if(y.shift<1/diff(rangey)) y.shift=1/diff(rangey)
    x0 <- 0
    y0 <- floor((min(y) - first.x * yd/xd)/y.shift) * y.shift
    y.end <- max(y) - last.x * yd/xd
    ret=data.frame()
    while (y0 < y.end) {
      ret1=polygon.onehatch(x, y, x0, y0, xd, yd, ...)
      ret=rbind(ret,ret1)
      y0 <- y0 + y.shift
    }
    return(ret)
  } else {
    if (angle < 90) {
      first.y <- max(y)
      last.y <- min(y)
    } else {
      first.y <- min(y)
      last.y <- max(y)
    }
    if(verbose) {
      # cat("   upi: ", upi[1], ", hatch: ", hatch, "\n")
      cat(sprintf("\tupi\t%f\n\thatch\t%f\n\n", upi[1], hatch))
    }
    x.shift <- upi[1L]/hatch/abs(sin(angle/180 * pi))
    if(x.shift<1/diff(rangex)) x.shift=1/diff(rangex)
    if(verbose) {
      cat("\tx\t",x,"\n")
      cat("\tfirst.y\t",first.y,"\n")
      cat("\txd\t",xd,"\n")
      cat("\tyd\t",yd,"\n")
      cat("\tx.shift\t",x.shift,"\n\n")
    }
    x0 <- floor((min(x) - first.y * xd/yd)/x.shift) * x.shift
    if(verbose){
      cat("\tfloor((min(x) - first.y * xd/yd)/x.shift)\t",floor((min(x) - first.y * xd/yd)/x.shift),"\n")
    }
    y0 <- 0
    x.end <- max(x) - last.y * xd/yd
    ret=data.frame()
    if(verbose) cat("\tx0\t",x0,"\n\tx.end\t",x.end,"\n")
    while (x0 < x.end) {
      ret1=polygon.onehatch(x, y, x0, y0, xd, yd, F,...)
      ret=rbind(ret,ret1)
      x0 <- x0 + x.shift
    }
    # if(verbose){
    #   cat("   ret: \n")
    #   print(ret)
    # }
    # cat("df1: ")
    # print(ret)
    if(verbose) {
      cat("   ######### polygon.fullhatch end #########\n")
    }
    return(ret)
  }
}

polygon.onehatch <- function(x, y, x0, y0, xd, yd, verbose=FALSE,...) {
  if(verbose) cat("      ######### polygon.onehatch #########\n")
  halfplane <- as.integer(xd * (y - y0) - yd * (x - x0) <= 0)
  cross <- halfplane[-1L] - halfplane[-length(halfplane)]
  does.cross <- cross != 0
  if(verbose){
    cat("      x: ",x,"\n")
    cat("      y: ",y,"\n")
    cat("      x0: ",x0,", y0: ",y0,", xd: ",xd,", yd: ",yd,"\n")
    cat("      xd * (y - y0): ",xd * (y - y0),"\n")
    cat("      yd * (x - x0): ",yd * (x - x0),"\n")
    cat("      halfplane: ",halfplane,"\n")
    cat("      does.cross: ",does.cross,"\n")
  }
  if (!any(does.cross))
    return()
  x1 <- x[-length(x)][does.cross]
  y1 <- y[-length(y)][does.cross]
  x2 <- x[-1L][does.cross]
  y2 <- y[-1L][does.cross]
  t <- (((x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 - x1))/(xd * (y2 - y1) - yd * (x2 - x1)))
  o <- order(t)
  tsort <- t[o]
  crossings <- cumsum(cross[does.cross][o])
  crossings <- crossings%%2
  drawline <- crossings != 0
  lx <- x0 + xd * tsort
  ly <- y0 + yd * tsort
  lx1 <- lx[-length(lx)][drawline]
  ly1 <- ly[-length(ly)][drawline]
  lx2 <- lx[-1L][drawline]
  ly2 <- ly[-1L][drawline]
  if(verbose) cat("      ######### polygon.onehatch end #########\n\n")
  data.frame(x=lx1,y=ly1,xend=lx2,yend=ly2)
}
