################
### hist.bin ###
################
fixed <- function(x,num){
  binwidth <- sum(abs(range(x)/num))
  print(binwidth)
  return(binwidth)
}

data.frame.sturges <- function(x){
  binwidth <- 1 + log(dim(x)[1],2)
  print(binwidth)
  return(binwidth)
}

sturges <- function(x){
  #binwidth <- ceiling(log2(length(x))+1)
  #binwidth <- fixed(x,ceiling(log2(length(x))+1))
  binwidth <- (diff(range(x))/ceiling(log2(length(x))+1))
  print(binwidth)
  return(binwidth)
}

data.frame.scott <- function(x){
  binwidth <- 3.5 * sapply(x, sd) * dim(x)[1]^{-1/3}
  print(binwidth)
  return(binwidth)
}

scott <- function(x,na.rm=T){
  binwidth <- 3.5 * sd(x,na.rm) * length(x)^{-1/3}
  # print(binwidth)
  return(binwidth)
}

freadman <- function(x){
  iqr <- stats::IQR(x)
  if (iqr == 0) 
    return(stats::mad(x, constant = 2))
  if (iqr > 0) 
    return(2 * iqr * length(x)^(-1/3))
  #ceiling(diff(range(x))/binwidth)
  else return(1L)
}

breaks.scott=function(x,na.rm=T){
  bw=scott(x,na.rm)
  if(na.rm) x=na.omit(x)
  range(x)/bw
}

bins.sturges=function(x,na.rm=T){
  bw=sturges(x)
  if(na.rm) x=na.omit(x)
  (diff(range(x))/bw)[1]
}

bins.scott=function(x,na.rm=T){
  bw=scott(x,na.rm)
  if(na.rm) x=na.omit(x)
  (diff(range(x))/bw)[1]
}

bins.freadman=function(x,na.rm=T){
  bw=freadman(x)
  if(na.rm) x=na.omit(x)
  (diff(range(x))/bw)[1]
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2006 Author Hideaki Shimazaki
# Department of Physics, Kyoto University
# shimazaki at ton.scphys.kyoto-u.ac.jp
# Please feel free to use/modify/distribute this program.
shimazaki <- function(x){
  N <- 2: 100
  C <- numeric(length(N))
  D <- C
  for (i in 1:length(N)) {
    D[i] <- diff(range(x))/N[i]
    edges = seq(min(x),max(x),length=N[i])
    hp <- hist(x, breaks = edges, plot=FALSE )
    ki <- hp$counts
    k <- mean(ki)
    v <- sum((ki-k)^2)/N[i]
    C[i] <- (2*k-v)/D[i]^2	#Cost Function
  }
  idx <- which.min(C)
  optD <- D[idx]
  #edges <- seq(min(x),max(x),length=N[idx])
  #h = hist(x, breaks = edges )
  #return(edges[2]-edges[1])
  #rug(x)
  print(optD)
  return(optD)
}

