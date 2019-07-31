#' mysave
mysave <- function(filename,w=pdf.sw,h=pdf.sh){
  p<-recordPlot();
  if (regexpr('\\.pdf$', filename)  > 0) { # ファイル名の最後が '.pdf'か？
    pdf(filename,width=w,height=h); 
  } else if (regexpr('\\.png$', filename)  > 0) { # ファイル名の最後が '.png'か？
    if(w==pdf.sw){
      w<-png.sw
    }
    if(h==pdf.sh){
      h<-png.sh
    }
    png(filename,width=w,height=h); 
  }else{
    next
  }
  replayPlot(p); 
  dev.off()
}

############
### size ###
############
sh <- 8.27
sw <- sh*sqrt(2)
gh <- sqrt((sqrt(10)-sqrt(2))/2)*sh
gw <- gh*(1+sqrt(5))/2
png.sh <- 320
png.sw <- png.sh*sqrt(2)
png.gh <- sqrt((sqrt(10)-sqrt(2))/2)*png.sh
png.gw <- png.gh*(1+sqrt(5))/2
pdf.sh <- 4.5*0.8
pdf.sw <- pdf.sh*sqrt(2)
pdf.gh <- sqrt((sqrt(10)-sqrt(2))/2)*pdf.sh
pdf.gw <- pdf.gh*(1+sqrt(5))/2

silver.h <- 8.27
silver.w <- silver.h*sqrt(2)
gold.h <- sqrt((sqrt(10)-sqrt(2))/2)*silver.h
gold.w <- gold.h*(1+sqrt(5))/2

##############
### colour ###
##############
cCyan <- "#00a0e9"
cyan <- "#00a0e9"
cMagenta <- "#e4007f"
magenta <- "#e4007f"
cGreen <- "#009944"
green <- "#009944"
cOrange <- "#f39800"
orange <- "#f39800"
cLightBlue <- "#0068b7"
lightblue <- "#0068b7"
qcolours <- c(cCyan,cMagenta,cGreen,cLightBlue,cOrange)
#scolours = colorRampPalette(c(cCyan,"grey60",cOrange))
#jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
jet.colours <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")

############
### grid ###
############
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, by.row=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




