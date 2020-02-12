
#'
#'
ggsave.pdf.g <-
  function (filename, plot = last_plot(), device = NULL, path = NULL,
            scale = 1, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE, ...)
  {
    height=sqrt((sqrt(10)-sqrt(2))/2)*4.5*1.5
    width=height*(1+sqrt(5))/2
    ggplot2::ggsave(filename=filename,plot=plot,device=device,path=path,scale=scale,
                    width=width,height=height,units=units,dpi=dpi,limitsize=limitsize,...)
  }

#'
#'
ggsave.pdf.s <-
  function (filename, plot = last_plot(), device = NULL, path = NULL,
            scale = 1, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE, ...)
  {
    height = 4.5*1.5
    width = height*sqrt(2)
    ggplot2::ggsave(filename=filename,plot=plot,device=device,path=path,scale=scale,
           width=width,height=height,units=units,dpi=dpi,limitsize=limitsize,...)
  }

#' getLevel
#' @export
getLevel <- function(x,y,prob=0.95) {
  kk <- MASS::kde2d(x,y)
  dx <- diff(kk$x[1:2])
  dy <- diff(kk$y[1:2])
  sz <- sort(kk$z)
  c1 <- cumsum(sz) * dx * dy
  approx(c1, sz, xout = 1 - prob)$y
}



#' fonts
#'
#serif, sans, mono, AvantGarde, Bookman, Courier, Helvetica, Helvetica-Narrow,
#NewCentrySchoolbook, Palatino, Times, URWGothic, URWBookman, NimbusMon, NimbusSan,
#URWHelvetica, NimbusSanCond, CenturySch, URWPalladio, NimbusRom, URWTimes,
#Japan1, Japan1HeiMin, Japan1GothicBBB, Japan1Ryumin,
#CNS1, GB1
# quartzFonts(HiraMaru=quartzFont(rep(c("HiraMaruProN-W4","HiraMaruProN-W8"), 2)))
# quartzFonts(HiraKaku=quartzFont(rep(c("HiraKakuProN-W3","HiraKakuProN-W6"), 2)))
# quartzFonts(HiraMin=quartzFont(rep(c("HiraMinProN-W3","HiraMinProN-W6"), 2)))
# quartzFonts(MigMix=quartzFont(rep(c("MigMix 1P regular","MigMix 1P bold"), 2)))

angled.boxes <-
  list("far.from.others.borders","calc.boxes","enlarge.box","draw.rects")


##############
### ggsave ###
##############
`gggsave` <- function (filename = default_name(plot), plot = last_plot(),
                       device = default_device(filename), path = NULL, scale = 1,
                       width = par("din")[1], height = par("din")[2], dpi = 300,
                       keep = plot$options$keep, drop = plot$options$drop, ...)
{
  # original
  # if (!inherits(plot, "ggplot"))
  #     stop("plot should be a ggplot2 plot")
  if (!inherits(plot, "recordedplot"))
    stop("plot should be a recordedplot")
  eps <- ps <- function(..., width, height) grDevices::postscript(..., width = width, height = height, onefile = FALSE, horizontal = FALSE, paper = "special")
  tex <- function(..., width, height) grDevices::pictex(..., width = width, height = height)
  pdf <- function(..., version = "1.4") grDevices::pdf(..., version = version)
  svg <- function(...) grDevices::svg(...)
  wmf <- function(..., width, height) grDevices::win.metafile(..., width = width, height = height)
  png <- function(..., width, height) grDevices::png(..., width = width, height = height, res = dpi, units = "in")
  jpg <- jpeg <- function(..., width, height) grDevices::jpeg(..., width = width, height = height, res = dpi, units = "in")
  bmp <- function(..., width, height) grDevices::bmp(..., width = width, height = height, res = dpi, units = "in")
  tiff <- function(..., width, height) grDevices::tiff(..., width = width, height = height, res = dpi, units = "in")
  default_name <- function(plot) {
    paste(digest.ggplot(plot), ".pdf", sep = "")
  }
  default_device <- function(filename) {
    pieces <- strsplit(filename, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
    match.fun(ext)
  }
  if (missing(width) || missing(height)) {
    message("Saving ", prettyNum(width * scale, digits = 3), "x ", prettyNum(height * scale, digits = 3), "image")
  }
  width <- width * scale
  height <- height * scale
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  device(file = filename, width = width, height = height, ...)
  on.exit(capture.output(dev.off()))
  print(plot, keep = keep, drop = drop)
  invisible()
}
