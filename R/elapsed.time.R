###### statistic #######3
txtprogress = function(i, iter, rt=NULL){
  percentDone = percentdone(i, iter);
  msg = sprintf('%3.1f percent has been achived.', percentDone);
  if(!is.null(rt)){
    msg = paste0(msg, sprintf(" Time remaining is %s.", rt$rt(i)))
  }
  cat(sprintf("\r%s", msg))
}
percentdone = function(i, iter){
  100 * (i) / iter
}
###### statistic #######3
setRemainingTime = function(rt, i){
  msg = sprintf("Time remaining is %s", rt$rt(i))
  cat(sprintf("\r%s.", msg))
}

remainingTime = function(iter){
  mean.dt = 0
  start = Sys.time()
  .val = 0
  dt = numeric(iter)
  rt = function(i){
    cur1 = Sys.time()
    dt[i] = as.numeric(cur1 - start)
    mean.dt = mean(dt[1:i])
    # mean.dt = (mean.dt*(i-1) + dt)/i
    remain = mean.dt*(iter - i)
    t = .POSIXct(remain, tz="GMT")
    format(t, "%H h: %M m: %OS1 s")
    # elapse = elapse + dt
    # rate = elapse/i*(iter-i)
    # format(.POSIXct((elapse/rate), tz="GMT"), "%H h: %M m: %S s")
  }
  getVal <- function() .val
  structure(list(getVal = getVal, rt=rt), class = "timeRemaining")
}###### statistic #######3
