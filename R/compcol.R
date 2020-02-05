comp2col = function(x,y,d=4){
  c1 = ifelse(x<y,paste0("\\bf{",format(x,digits=d),"}"),format(x,digits=d))
  c2 = ifelse(x>y,paste0("\\bf{",format(y,digits=d),"}"),format(y,digits=d))
  data.frame(c1,c2)
}

comp3col = function(x,y,z,d=4){
  c1 = ifelse(x<y & x<z,paste0("\\bf{",format(x,digits=d),"}"),format(x,digits=d))
  c2 = ifelse(y<x & y<z,paste0("\\bf{",format(y,digits=d),"}"),format(y,digits=d))
  c3 = ifelse(z<x & z<y,paste0("\\bf{",format(z,digits=d),"}"),format(z,digits=d))
  data.frame(c1,c2,c3)
}

comp4col = function(w,x,y,z,d=4){
  c1 = ifelse(w<x & w<y & w<z, paste0("\\bf{",format(w,digits=d),"}"),format(w,digits=d))
  c2 = ifelse(x<w & x<y & x<z, paste0("\\bf{",format(x,digits=d),"}"),format(x,digits=d))
  c3 = ifelse(y<w & y<x & y<z, paste0("\\bf{",format(y,digits=d),"}"),format(y,digits=d))
  c4 = ifelse(z<w & z<x & z<y, paste0("\\bf{",format(z,digits=d),"}"),format(z,digits=d))
  data.frame(c1,c2,c3,c4)
}

comp5col = function(v,w,x,y,z,d=4){
  c0 = ifelse(v<w & v<x & v<y & v<z, paste0("\\bf{",format(v,digits=d),"}"),format(v,digits=d))
  c1 = ifelse(w<v & w<x & w<y & w<z, paste0("\\bf{",format(w,digits=d),"}"),format(w,digits=d))
  c2 = ifelse(x<v & x<w & x<y & x<z, paste0("\\bf{",format(x,digits=d),"}"),format(x,digits=d))
  c3 = ifelse(y<v & y<w & y<x & y<z, paste0("\\bf{",format(y,digits=d),"}"),format(y,digits=d))
  c4 = ifelse(z<v & z<w & z<x & z<y, paste0("\\bf{",format(z,digits=d),"}"),format(z,digits=d))
  data.frame(c0,c1,c2,c3,c4)
}
