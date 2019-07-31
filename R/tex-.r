tabletex=function(d2,align,filename,dirname="table/"){
  align=paste0("c",align)
  if(!dir.exists(dirname)) dir.create(dirname)
  filename=paste0(dirname,filename)
  print(xtable(d2,align=align),booktabs=T,floating=FALSE,include.rownames=F,file=filename)
}

