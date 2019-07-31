#' My break points
#' @import NULL
#' @export
#' @param x A numeric vector
#' @param num.breaks The number of break points.
#' @return A numeric vector
#' @examples 
#' p = ggplot(mtcars, aes(mpg, wt)) + geom_point() + mytheme_grey()
#' p + scale_x_continuous()
#' p + scale_x_continuous(breaks=myBreaks(mtcars$mpg, 3))
#' p + scale_x_continuous(breaks=myBreaks(mtcars$mpg, 10))
#' p + scale_x_continuous(breaks = myBreaks(mtcars$mpg, 10, verbose =T))
#' p + scale_x_continuous(breaks = myBreaks(mtcars$mpg, 10, method="q", verbose =T))
#' p + scale_x_continuous(breaks = myBreaks(mtcars$mpg, 20, method="e", verbose =T))
#' @rdname myBreaks
myBreaks <- function(x,num.breaks=3,breaks=NULL,method="equal",verbose=FALSE){
  y <- as.numeric(as.character(levels(factor(x))))
  if(is.null(breaks)){
    if(grepl("e",substring(method,1,1))){
      breaks <- seq(min(y), max(y), len = (num.breaks-2))
    }else if(grepl("q",substring(method,1,1))){
      breaks <- quantile(y,0:(num.breaks-1)/(num.breaks-1))
    }
  }
  if(verbose){
    cat("num.breaks=",num.breaks, ", method=", method,"\n")
    print(breaks)
  } 
  
  names(breaks) <- attr(breaks,"labels")
  breaks
}

make.bks=function(x,base=10,interval=10,num=NA){
  minx=min(x)
  maxx=max(x)
  if(minx==0) minx=min(df$x[-1])/base
  lim0=base^floor(log(minx,base))
  lim1=base^ceiling(log(maxx,base))
  bks=numeric()
  minor.bks=numeric()
  lbls=character()
  minor_lbls=character()
  lower=lim0
  upper=lower*base
  while(upper<=lim1){
    bks=c(bks,lower)
    if(interval!=1){
      minor.bks=c(minor.bks,seq(lower,upper,len=interval)[-interval])
    }
    # if(interval>1){
    #   lbls=c(lbls,c(lower,rep("",interval-2)))
    # }else{
    lbls=c(lbls,lower)
    # }
    lower=upper
    upper=lower*base
  }
  bks=c(bks,lim1)
  lbls=c(lbls,lim1)
  if(!is.na(num)){
    idx=which(lbls!="")
    s=ceiling(length(idx)/num)
    idxs=idx[1]
    while(max(idxs)<=length(idx)){
      idxs=c(idxs,idxs[length(idxs)]+s)
    }
    idxs=idxs[-length(idxs)]
    lbls[idx[!idx%in%idx[idxs]]]=""
  }
  minor_lbls=as.character(minor.bks)
  # cat(bks)
  # if(length(minor.bks)!=0){
  #   return(list(breaks=bks,labels=lbls,minor_breaks=minor.bks))
  # }else{
    return(list(breaks=bks,labels=lbls,minor_breaks=minor.bks,minor_labels=minor_lbls))
  # }
}
