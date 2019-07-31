label_parseall <- function(variable, value) {
  plyr::llply(value, function(x) parse(text = paste(variable, x, sep = "==")))
}
label_wrapper<- function(variable, value, ...) {
  plyr::llply(as.character(value), function (x) paste(strwrap(x, ...), collapse = "\n"))
}
label_wrapper30 <- function(variable, value) {
  label_wrapper(variable, value, 30)
}

myfacet_grid=function(facets, margins = FALSE, scales = "fixed", space = "fixed", 
             shrink = TRUE, labeller = label_parseall, as.table = TRUE, 
             switch = NULL, drop = TRUE){
  facet_grid(facets, margins = margins, scales =scales, space = space, 
             shrink=shrink, labeller=labeller, as.table=as.table, 
             switch=switch, drop=drop)
}

myfacet_wrap=function(facets, nrow=NULL, ncol=NULL, scales = "fixed", 
                      shrink =TRUE, labeller = label_parsed, as.table = TRUE,
                      switch = NULL, drop = TRUE, dir = "h"){
  facet_wrap(facets,nrow,ncol,scales,shrink,labeller,as.table,switch,drop,dir)
}
