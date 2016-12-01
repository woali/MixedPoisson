Gamma.density <-
function(theta, gamma.par) {
theta <- mpfr(theta, 99)
y=(gamma.par^(gamma.par)*theta^(gamma.par-1)*exp(-(gamma.par*theta)))/(gamma(gamma.par))
return(asNumeric(y))
}
