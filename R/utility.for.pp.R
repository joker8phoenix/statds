func.breaks = function(){
  structure(.Data = c(
    .00001, .00003, .00005,
    .0001, .0003, .0005,
    .001, .003, .005,
    .01, .03, .05,
    .1, .3, .5,
    .9, 0.99, 0.999))
}
func.minor_breaks = function(){
  structure(.Data = c(
    seq(1e-05, 1e-04 - 2.5e-06, by = 2.5e-06),
    seq(1e-04, 0.001 - 2.5e-05, by = 2.5e-05),
    seq(0.001, 0.01 - 0.00025, by = 0.00025),
    seq(0.01, 0.1 - 0.0025, by = 0.0025),
    seq(0.1, 1 - 0.025, by = 0.025),
    seq(0.90, 0.98, 0.01)[-1],
    seq(0.98, 0.99, 0.005)[-1],
    seq(0.99, 0.999, 0.001)[-1]
  ))
}

exp_trans = function() {
  trans = function(p)
    - log(1 - p)
  inv = function(x)
    1 - exp(-x)
  trans_new(
    name = "exponential.pp",
    transform = trans,
    inverse = inv,
    domain = c(1e-100, Inf)
  )
}

scale_y_exp = function(
  ...,
  limits = c(0, 0.99),
  expand = c(0, 0),
  breaks = func.breaks(),
  minor_breaks = func.minor_breaks()) {
  labels = formatC(breaks, format = "g")
  scale_y_continuous(
    limits = limits,
    expand = expand,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    trans = exp_trans(),
    ...
  )
}

normal_trans = function() {
  trans = function(p)
    qnorm(p)
  inv = function(x)
    pnorm(x)
  trans_new(
    name = "normal.pp",
    transform = trans,
    inverse = inv,
    domain = c(1e-100, Inf)
  )
}

scale_y_normal = function (
  ...,
  limits =c(0.001, 0.99),
  expand = c(0, 0),
  breaks = func.breaks(),
  minor_breaks = func.minor_breaks()) {
  labels = formatC(breaks, format = "g")
  scale_y_continuous(
    limits = limits,
    expand = expand,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    trans = normal_trans(),
    ...
  )
}

sev_trans = function() {
  trans = function(p)
    log(-log(1-p))
  # sign(p)*log(-log(1-abs(p)))
  inv = function(x)
    1-exp(-exp(x))
  trans_new(
    name = "sev.pp",
    transform = trans,
    inverse = inv,
    domain = c(1e-100, Inf)
  )
}

scale_y_sev = function (
  ...,
  limits= c(0.01, 0.999),
  expand=c(0, 0),
  breaks = func.breaks(),
  minor_breaks = func.minor_breaks()) {
  labels = formatC(breaks, format = "g")
  scale_y_continuous(
    limits = limits,
    expand = expand,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    trans = sev_trans(),
    ...
  )
}
lev_trans = function() {
  trans = function(p)
    -log(-log(p))
  inv = function(x)
    exp(-exp(-x))
  trans_new(
    name = "lev.pp",
    transform = trans,
    inverse = inv,
    domain = c(1e-100, Inf)
  )
}

scale_y_lev = function (...,
                        limits=c(0.01, 0.999),
                        expand=c(0, 0),
                        breaks = func.breaks(),
                        minor_breaks = func.minor_breaks()) {
  labels = formatC(breaks, format = "g")
  scale_y_continuous(
    limits = limits,
    expand = expand,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    trans = lev_trans(),
    ...
  )
}

logis_trans = function() {
  trans = function(p)
    -log(p^(-1)-1)
  inv = function(x)
    1/(1+exp(-x))
  trans_new(
    name = "logis.pp",
    transform = trans,
    inverse = inv,
    domain = c(1e-100, Inf)
  )
}
scale_y_logis = function (...,
                        limits=c(0.01, 0.999),
                        expand=c(0, 0),
                        breaks = func.breaks(),
                        minor_breaks = func.minor_breaks()) {
  labels = formatC(breaks, format = "g")
  scale_y_continuous(
    limits = limits,
    expand = expand,
    breaks = breaks,
    labels = labels,
    minor_breaks = minor_breaks,
    trans = logis_trans(),
    ...
  )
}

arrhenius_trans = function() {
  trans = function(x) 11605/(x+273.15)
  inv = function(x) (11605-273.15*x)/x
  trans_new(
    name = "arrhenius.scale",
    transform = trans,
    inverse = inv,
    breaks = extended_breaks(),
    minor_breaks = regular_minor_breaks(),
    format = format_format(),
    domain = c(-Inf, Inf)
  )
}
scale_x_arrhenius = function (...) {
  scale_x_continuous(
    ...,
    trans = arrhenius_trans()
  )
}

