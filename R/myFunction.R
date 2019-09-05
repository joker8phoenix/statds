#' Function
#' 
#' 
NULL

proxy=function(flg=1){
  if(flg==1){
    Sys.setenv("http_proxy"="http://proxy.cc.chuo-u.ac.jp:8080")
    cat("プロキシを設定しました。\n")
  }else{
    Sys.setenv("http_proxy"="")
    cat("プロキシを解除しました。\n")
  }
}


rbind.addname=function(...){
  key=as.list(substitute(list(...)))[-1L]
  var=list(...)
  df=data.frame()
  for(i in 1:length(key)){
    df=rbindlist(l=list(df,cbind(data.frame(data.frame.name=paste0(key[[i]])),var[[i]])))
  }
  return(df)
}

substrLeftRight <- function(x, left, right){
  substr(x, nchar(x)-left+1, nchar(x)-left+right)
}

substrLeft <- function(x, n){
  substr(x, 1, nchar(x)-n)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

constAssign=function(x, value, envir=as.environment(-1)) {
  e=envir
  assign(x, value, envir=e)
  lockBinding(x, e)
}
pb <- function(N, FUN = c) {
  pbar <- txtProgressBar(min = 2, max = N, style = 3)
  count <- 0L
  if(is.character(FUN)) {
    FUN <- get(FUN)
  }
  function(...) {
    count <<- count + length(list(...)) - 1L
    setTxtProgressBar(pbar, count)
    if(count == (N-1)) {
      cat("\n")
    }
    FUN(...)
  }
}
# 多倍長計算を行う
# http://aoki2.si.gunma-u.ac.jp/R/multibyte.html
"%add%" <- function(ans, b)                          # 足し算の演算子　ans %add% b を行い結果を返す
{                                                       # ans, b は "multibyte" クラスの多倍長整数
  ans <- ans+b                                 # 各桁の足し算を行う
  for (i in length(ans):1) {                      # 各桁について下の桁から，
    if (ans[i] >= 10000000000) {            # 繰り上がり処理を行う
      ans[i] <- ans[i]-10000000000
      ans[i-1] <- ans[i-1]+1
    }
  }
  return(ans)                                     # 結果を返す
}
#
"%sub%" <- function(ans, b)                          # 引き算の演算子　ans %sub% b を行い結果を返す
{                                                       # ans, b は "multibyte" クラスの多倍長整数
  ans <- ans-b                                 # 各桁の引き算を行う
  for (i in length(ans):1) {                      # 各桁について下の桁から，
    if (ans[i] < 0) {                    # 繰り下がり処理を行う
      ans[i] <- ans[i]+10000000000
      ans[i-1] <- ans[i-1]-1
    }
  }
  return(ans)                                     # 結果を返す
}
#
"%div%" <- function(ans, n)                          # 割り算の演算子　ans %div% n を行い結果を返す
{                                                       # 注：n は "multibyte" クラスではなく普通の整数値
  r <- 0                                               # 剰余
  for (i in 1:length(ans)) {                      # 各桁について上の桁から，
    x <- ans[i]+r*10000000000            # より上の位での剰余を考慮した，被除算数
    ans[i] <- x%/%n                              # 割り算を行い結果を格納
    r <- x-n*ans[i]                              # 今回の剰余
  }
  return(ans)                                     # 結果を返す
}

require2 <- function(packages){
  list.of.packages <- paste0(substitute(packages))
  # list.of.packages <- c("ggplot2", "Rcpp")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  require(list.of.packages,character.only = T)
}

print.xtable.booktabs <- function(x){
  print(
    xtable(x),floating=F,hline.after=NULL,
    add.to.row=list(
      pos=list(-1,0, nrow(x)),command=c(
        '\\toprule\n',
        '\\midrule\n',
        '\\bottomrule\n')))
  
}

# https://stackoverflow.com/questions/3245862/format-numbers-to-significant-figures-nicely-in-r
format.sig = function(x,digits=3){
  formatC(signif(x,digits=digits), digits=digits,format="fg", flag="#")
}
