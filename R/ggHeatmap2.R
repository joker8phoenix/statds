ggHeatmap2=function(df,x=NULL,y=NULL,z=NULL,...,base_size=14,rescale=NULL,rev=TRUE,low="white",high="steelblue",legend="none")
{
  if(any(class(df)=="data.table")) df=as.data.frame(df)
  df=df[,1:3]
  start=1
  end=dim(df)[1]
  if(is.null(x)){
    x=df[,1]
  }
  if(is.null(y)){
    y=df[,2]
  }
  if(is.null(z)){
    z=df[,3]
  }
  df=df[start:end,]
  cnames=colnames(df)
  setnames(df,cnames[1:3],c("x","y","z"))
  dfm=df
  dfm$rescale=df$z
  if(!is.null(rescale)){
    rs="x"
    if(grepl("x",rescale)){
      dfm=dfm %>%
        dplyr::group_by(x) %>%
        dplyr::mutate(rescale=as.numeric(scale(z)))
    }
    if(grepl("y",rescale)){
      dfm=dfm %>%
        dplyr::group_by(y) %>%
        dplyr::mutate(rescale=as.numeric(scale(z)))
    }
  }
  flg.x=FALSE
  flgs=suppressWarnings(!is.na(as.numeric(as.character(dfm$x))))
  if(all(flgs)){
    dfm$x=as.numeric(as.character(dfm$x))
    flg.x=TRUE
  }
  p=dfm %>% 
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = rescale),colour = "white") + 
    scale_fill_gradient(low = low, high = high) + 
    mytheme_grey(base_size = base_size) + 
    labs(x = cnames[1], y = cnames[2]) +
    theme(legend.position = legend, 
          axis.ticks = element_blank(), 
          axis.text.x = element_text(size = base_size *0.8, angle = 330, hjust = 0, colour = "grey50"))
  if(!flg.x) {
    p=p+scale_x_discrete(expand = c(0, 0))
  } else {
    p=p+scale_x_continuous(expand = c(0, 0))
  }
  
#   if(rev){
#     p=p+scale_y_reverse(expand = c(0, 0))
#   }else{
#     p=p+scale_y_discrete(expand = c(0, 0))
#   }
  print(p)
  return(list(plot=p,data=df))
}


