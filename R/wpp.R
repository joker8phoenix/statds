wpp_trans <- function(base=10){#exp(1)){
  trans <- function(y) log(-log(1-y, base),base)
  inv = function(y) 1-base^(-base^y)
  trans_new(name='wpp', transform=trans, inverse=inv, breaks=wpp_breaks(base=base),
            domain = c(1e-100,Inf))
}
wpp_breaks=function (n = 3, base = 10) {
  function(y) {
    rng <- sort(log(-log(1-range(y[y!=0], na.rm = TRUE), base = base)))
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min) 
      return(1-base^(-base^min))
    by <- floor((max - min)/n) + 1
    1-base^(-base^seq(min, max, by = by))
  }
}
coord_wpp=function(ylim=c(0.0001,1)){
  # coord_trans(x = "log10", y="wpp", limy = ylim)
}
axis_wpp=function(){
  breaks=c(seq(.00001,.0001-.00001,by=.00001),seq(.0001,.001-.0001,by=.0001),seq(0.001,0.01-0.001,by=0.001),seq(0.01,0.1-0.01,by=0.01),seq(0.1,1-0.1,by=0.1),.95,0.99,.999,1)
  labels=as.character(breaks*100)
  minor_breaks=c(seq(.00001,.0001-.0000025,by=.0000025),seq(.0001,.001-.000025,by=.000025),seq(0.001,0.01-0.00025,by=0.00025),seq(0.01,0.1-0.0025,by=0.0025),seq(0.1,1-0.025,by=0.025),seq(.91,.99,.01),seq(.991,.999,.001),1)
  list(breaks=breaks,labels=labels,minor_breaks=minor_breaks)
}
scale_x_wpp=function(x,...){
  i=floor(log(min(x),10))
  j=ceiling(log(max(x),10))
  k=numeric()
  for(ii in i:(j-1)){
    k=c(k,seq(10^ii,10^ii*10-10^ii,by=10^ii))
  }
  l=numeric()
  for(ii in i:(j-1)){
    l=c(l,seq(10^ii,10^ii*10-10^ii/4,by=10^ii/4))
  }
  scale_x_continuous(...,breaks=k,labels=as.character(k),minor_breaks=l,trans=log10_trans())
}
scale_y_wpp=function(...){
  aw=axis_wpp()
  scale_y_continuous(...,breaks=aw$breaks,labels=aw$labels,minor_breaks=aw$minor_breaks,trans=wpp_trans())
}

# n=100
# m=1
# eta=1
# x1=sort(rweibull(n,m,eta))
# y1=(1:n)/(n+1)
# d=data.frame(x1,y1) 
# mbsx=make.bks(x1,base=10)
# mbsy=make.bks(y1,base=10)
# p=d %>% ggplot(aes(x1,y1))+geom_pointj()+mytheme_seaborn()
# p
# p+scale_x_wpp(d$x1)
# p+scale_y_wpp()
# p+scale_y_wpp()+scale_x_wpp(d$x1)
# p+scale_y_wpp()+scale_x_wpp(d$x1)+annotation_logticks()
# 
# 
# p+scale_y_wpp()+coord_trans(x="log10")
# 
# p+scale_x_continuous(breaks=mbsx$breaks,labels=mbsx$labels,minor_breaks=mbsx$minor_breaks)
# p+scale_y_continuous(breaks=mbsy$breaks,labels=mbsy$labels,minor_breaks=mbsy$minor_breaks)
# p+coord_wpp(ylim = c(0.001,0.999))
# p+scale_y_wpp(limits=c(.00001,.9999),breaks=mbsy$breaks,labels=mbsy$labels,minor_breaks=mbsy$minor_breaks)
# 
# # p=data.frame(t,hat.f) %>% ggplot(aes(t,hat.f))+geom_pointj()+coord_lognormal()+ggtitle("Lognormal")+mytheme_gray()
# # p=data.frame(t,hat.f) %>% ggplot(aes(t,hat.f))+geom_pointj()+coord_weibull()+ggtitle("Weibull")+mytheme_gray()
# # p=data.frame(t,hat.f) %>% ggplot(aes(t,hat.f))+geom_pointj()+coord_loglogistic()+ggtitle("Loglogistic")+mytheme_gray()
# 
# p+scale_y_wpp()+scale_x_wpp(d$x1)
# 
# 
# 
