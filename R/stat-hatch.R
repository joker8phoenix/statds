#' stat_hatch
stat_hatch <- function (
  mapping = NULL, data = NULL, geom = "hatch",
  position = "identity", width = NULL, height = NULL,
  hatch = NULL, angle = NULL, border = NULL,
  ...,
  show.legend=NA, inherit.aes=TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatHatch,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      height = height,
      hatch = hatch,
      angle = angle,
      border = border,
      ...
    )
  )
}

StatHatch <- ggplot2::ggproto("StatHatch", ggplot2:::Stat,
  #default_geom <- function(.) GeomHatch

  compute_group = function(
    self, data, scales, hatch=NULL, angle=NULL, border=NULL, ...) {
    # print(data)
    # print(self$aesthetics)

    if (is.null(angle)) {
      if (is.null(data$angle)){
        # din=graphics::par("din")
        # angle=atan(din[2]/din[1])/pi*180
        # print(angle)
        # print(data)
        # print(self)
        # print(scales)
        #prettyNum(from_inches(dim), digits = 3)
        # print(hatch)
        data$angle = (45 * runif(1)) %% 180
      }
    }
    else data$angle <- angle
    # print(unique(data$angle))
    if (is.null(hatch)) {
      if (is.null(data$hatch)) {
        rangex=scales$x$range$range
        rangey=scales$y$range$range
        if(!is.numeric(scales$x$range$range)){
          rangex=range(as.numeric(factor(scales$x$range$range)))
        }
        if(!is.numeric(scales$y$range$range)){
          rangey=range(as.numeric(factor(scales$y$range$range)))
        }
        # cat("range_x=",rangex,"\n")
        # cat("range_y",rangey,"\n")
        # print(data)
        #data$hatch <- 10/diff(range(scales$x$range$range,scales$y$range$range))}
        if(any(is.factor(data$angle)) | any(!is.numeric(data$angle))){
          data$angle=as.numeric(data$angle)*113
        }
        #print(data$angle)
        data$angle=data$angle%%180
        theta=data$angle[1L]/180*pi
        usr <- par("usr")
        pin <- par("pin")
        # print(pin)
        upi <- c(usr[2L] - usr[1L], usr[4L] - usr[3L])/pin
        if(upi[1L] <0){
          angle <- 180 -angle
        }
        if(upi[2L] <0){
          angle <- 180 - angle
        }
        upi <- abs(upi)
        xd <- cos(data$angle[1L]/180 * pi) * upi[1L]
        yd <- sin(data$angle[1L]/180 * pi) * upi[2L]
        hatch = if(data$angle[1L]<45 | data$angle[1L]>135){
          5*yd/xd/diff(rangex)/abs(sin(theta))
        } else {
          5*xd/yd/diff(rangey)/abs(cos(theta))
        }
        data$hatch = hatch
        # cat("upi= ",upi,"\n")
        # cat("xd= ",xd,"\n")
        # cat("yd= ",yd,"\n")
        # cat("hatch= ",hatch,"\n")
      } else if(any(data$hatch<0) | any(!is.numeric(data$hatch))){
        data$hatch=as.numeric(data$hatch)
        dh=range(data$hatch)
        data$hatch=4*(data$hatch-dh[1])/diff(dh)+1
        # print(data$hatch)
        # print(dh)
        # print(range(data$hatch))
      }
    }
    else data$hatch <- hatch
    if (is.null(border)) {
      if (is.null(data$border))
        data$border <- "black"
    }else {
      data$border <- border
    }


    # angles = plyr::ddply(data, "group", function(df) {
    #   angle <- unique(df$angle)+runif(1,0,360)
    #   angle = angle %% 180
    #   data.frame(angle)
    # })
    # print(angles)
    # print("stat_hatch")
    # print(data)
    unique(data)
  }
  #desc_outputs <- list()
)
