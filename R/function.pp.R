alt.plot.failure.data = function(d, xlabel=quote(italic(t)), ylabel="Specimens"){
  t = d$t
  nj = rev(seq(nrow(d)))
  dj = d$cens
  rj = 1-dj
  d = cbind(d, data.frame(nj, dj, rj))
  d %>% ggplot(aes(0, nj))+
    geom_segment(aes(xend=t, yend=nj))+
    geom_text(aes(x=t, label=ifelse(dj==1, "X", "")))+
    xlab(xlabel)+ylab(ylabel)
}
plot.failure.data = alt.plot.failure.data

alt.create.pp.data = function(d, empirical = 0){
  t = d$t
  nj = rev(seq(nrow(d)))
  dj = d$cens
  rj = 1-dj
  d = cbind(d, data.frame(nj, dj, rj))

  # d = data.frame(t, nj)
  d = d[which(d$cens==1), ]
  d

  alpha = 0.05
  d = d %>%
    mutate(p = 1/(nj+ empirical)) %>%
    mutate(q = 1-p) %>%
    mutate(s = cumprod(q)) %>%
    mutate(
      f = 1-s,
      se = sqrt(s^2*cumsum(p/(1-p)/nj))) %>%
    mutate(w = exp(qnorm(1-alpha/2)*se/(f*(1-f)))) %>%
    mutate(
      lower.f = f/(f+(1-f)*w),
      upper.f = f/(f+(1-f)/w)
    )
  d

  # d = rbind(data.frame(t=0, p=0, lower.p=0, upper.p=0), d)
  d$xend <- c(d$t[2:nrow(d)], d$t[nrow(d)]+(d$t[nrow(d)]-d$t[nrow(d)-1])*1.5)
  d$fend <- d$f
  d$lower.fend <- d$lower.f
  d$upper.fend <- d$upper.f

  return(d)
}
create.pp.data= alt.create.pp.data

alt.plot.pp.data = function(d, empirical = 0, xlabel=""){
  d = create.pp.data(d, empirical)
  p = d %>% ggplot(aes(t, f))+
    geom_point()+
    geom_segment(aes(xend=xend, yend=fend))+
    geom_segment(aes(t, lower.f, xend=xend, yend=lower.fend), linetype=2)+
    geom_segment(aes(t, upper.f, xend=xend, yend=upper.fend), linetype=2)+
    xlim(c(min(d$t), max(d$xend)))+
    ylim(c(min(d$lower.f), max(d$upper.fend)))+
    xlab(xlabel)+ylab(quote(italic(p)))
  print(p)
  return(p)
}
plot.pp.data = alt.plot.pp.data

alt.pp.comparison = function(df, empirical = 0){
  d = create.pp.data(df, empirical)
  p = plot.pp.data(df, empirical)
  range.y = c(min(d$lower.f), max(d$upper.fend))
  p.normal = p + scale_y_normal(limits=range.y)+ggtitle("Normal Plot")
  p.lognormal = p + scale_y_normal(limits=range.y)+scale_x_log10()+ggtitle("Lognormal Plot")
  p.exp = p+scale_y_exp(limits=range.y)+theme(legend.position = "none")+ggtitle("Exponential Plot")
  p.sev = p+scale_y_sev(limits=range.y)+ggtitle("SEV Plot")
  p.weibull = p + scale_y_sev(limits=range.y)+scale_x_log10()+ggtitle("Weibull Plot")
  p.lev = p + scale_y_lev(limits=range.y)+ggtitle("LEV Plot")
  p.loglev = p + scale_y_lev(limits=range.y)+scale_x_log10()+ggtitle("Log LEV Plot")
  p.logis = p + scale_y_logis(limits=range.y)+ggtitle("Logistic Plot")
  p.loglogis = p + scale_y_logis(limits=range.y)+scale_x_log10() + ggtitle("Loglogistic Plot")
  p.all = gridExtra::grid.arrange(
    p.normal, p.lognormal,
    p.sev, p.weibull, p.exp,
    p.lev, p.loglev,
    p.logis, p.loglogis,
    ncol=3)
  return(p.all)
}
pp.comparison = alt.pp.comparison
