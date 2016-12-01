ll.gamma <-
function(gamma.par, t) {
gamma.par <- mpfr(gamma.par, 99)
y=log(prod((gamma.par^(gamma.par)*t^(gamma.par-1)*exp(-(gamma.par*t)))/(gamma(gamma.par))))
return(asNumeric(y))
}
