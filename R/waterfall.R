
waterfall <- function(x,y,z=NULL,slices=50,lboost=1,gboost=1,
                      xinc=0,yinc=0.01, bcol="white", fcol="black",
                      lcol="white",lwidth=.2, cutprop=FALSE,
                      alpha=.5, tmax = .8, tmin = .05,
                      heightprop=FALSE, xlab=FALSE){
  
  ycut<-min(y)+((0:(slices))*(max(y)-min(y))/slices)
  
  height<-gboost*((slices*yinc)+max(density(x)$y))
  
  # plot( c(min(x)-((max(x)-min(x))/10),max(x)+((max(x)-min(x))/10)),
  # c(0,height),
  # xaxt="n",yaxt="n",ylab="",xlab="")
  # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=bcol)
  # dfpar=data.frame(x=c(par("usr")[1],par("usr")[2],par("usr")[2],par("usr")[1]),y=c(par("usr")[3],par("usr")[3],par("usr")[4],par("usr")[4]))
  # p=p+geom_rect(data=NULL,aes(xmin=par("usr")[1],xmax=par("usr")[2],ymin=par("usr")[3],ymax=par("usr")[4]),fill=bcol);p
  if (length(z)==length(y)) {
    zmin <- min(z)
    zmax <- max(z)
    zrange <- max(zmax-zmin)
  }
  
  ifcol <- fcol; ilcol <- lcol;  zdeg <- 0
  
  if (length(z)==length(y)){
    meanz <- NULL
    for(i in slices:1)
      meanz[i]<- mean(z[y>=min(ycut[i]) & y<max(ycut[i+1])])
    zdegree<-(meanz-min(meanz, na.rm=TRUE))/
      (max(meanz, na.rm=TRUE)-min(meanz, na.rm=TRUE))
  }
  
  color.mix <- function(cols, degree=0) {
    if (is.null(nrow(cols))) {
      if (class(cols)=="numeric") 
        return(rgb(cols[1],cols[2],cols[3],cols[4]))
      return(cols)
    }
    # Define a function to find elementwise minimum 
    (deg <- degree*(nrow(cols)-1)+1)
    emin <- function(x, y=0) apply(cbind(x, y), 1, min)
    (r <- 1-emin(abs(1:nrow(cols)-deg),1))
    (comb <- apply(cols*r,2,sum))
    mm <- function(x) max(min(x,1),0)
    rgb(mm(comb[1]),
        mm(comb[2]),
        mm(comb[3]),
        mm(comb[4]))
  }
  
  # i=50
  df=data.frame()
  for(i in slices:1) {
    # for(i in slices:40) {
    # print(i)
    miny<-ycut[i]
    maxy<-ycut[i+1]
    
    
    gx<-(i-1)*(max(x)-min(x))*xinc
    
    if (cutprop) {
      slLength <- slices*sum(y>=miny & y<maxy)/length(y)
      if (i==slices) gy <- (i-1)*(height)*yinc
      if (i<slices)  gy <- gyLast-(height)*yinc*slLength
      gyLast <- gy
    }else gy<-(i-1)*(height)*yinc
    # print(gy)
    if (transprop) {
      trange <- tmax-tmin
      if (is.null(nrow(ifcol))) 
        ifcol[4] <- min(trange*slices*sum(y>=miny & y<maxy)/length(y)+tmin,tmax)
      if (!is.null(nrow(ifcol))) 
        ifcol[,4] <- min(trange*slices*sum(y>=miny & y<maxy)/length(y)+tmin,tmax)
    }
    
    
    
    # If z is an input vector then grab the color degree from it
    if (length(z)==length(y)) zdeg<-zdegree[i]
    
    # Added the try because distributions without defined upper
    # and lower bounds can give the density function trouble.
    try({
      # Use the color.mixer function to select a color
      fcol<-color.mix(ifcol, zdeg);
      lcol<-color.mix(ilcol, zdeg);
      # Calculte density curves and plot them
      # print(fcol)
      dd<-density(x[y>=miny & y<maxy]);
      if (heightprop) vscale <- lboost*slices*sum(y>=miny & y<maxy)/length(y)
      if (!heightprop) vscale <- lboost
      # polygon(dd$x+gx,vscale*dd$y+gy,col=fcol, border=fcol);
      # lines(dd$x+gx,vscale*dd$y+gy,col=lcol,lwd=lwidth);
      df=rbindlist(l=list(df,data.frame(idx=i,x=dd$x+gx,y=vscale*dd$y+gy,fcol=fcol,lcol=lcol)))
    },silent = T)
  }
  # df$idx
  # unique(df$idx)
  df$idx=factor(df$idx,levels=unique(df$idx))
  # print(df)
  p= ggplot(data=NULL)+xlim(c(min(x)-((max(x)-min(x))/10),max(x)+((max(x)-min(x))/10)))+ylim(c(0,height))+xlab("")+ylab("");
  p=p+geom_polygon(data=df,aes(x,y,fill=fcol,colour=lcol,group=idx),alpha=alpha)
  # p=p+geom_line(data=df,aes(x,y,colour=lcol,group=idx),alpha=.7,size=lwidth)
  # p=p+geom_text(data=df %>% group_by(idx) %>% summarise(y=min(y),fcol=unique(fcol)),aes(x=-10,y,label=fcol),size=5)
  p=p+scale_colour_manual(values=unique(df$lcol))
  # p=p+scale_fill_manual(values=unique(df$fcol));p
  # p=p+viridis::scale_color_viridis(discrete = TRUE)
  p=p+viridis::scale_fill_viridis(discrete = TRUE)
  p=p+mytheme_bw(legend="none")
  
  return(p)
}




# 
# fcol <- rgb(0,.5,.5,.5)
# n <- 500000; y<-rnorm(n); x<-rnorm(n)
# n <- 500000; y<-rnorm(n); x<-3*rnorm(n)+y^2
# 
# z <- -(abs(x)+abs(y))+rnorm(n)*3
# fcol <- rbind(c(0,.1,.5,.5), c(.3,.8,.8,.5), c(1,1,0,.5))
# lcol <- rbind(c(0,.3,.3,.8), c(.1,.1,.2,.7), c(0,0,1,.65))
# waterfall(x,y,z,fcol=fcol,lcol=lcol,alpha=.5)
# 
