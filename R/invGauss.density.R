invGauss.density <-
function(theta, delta) {
theta <- mpfr(theta, 99)
y=(delta*exp(delta^2)*theta^(-3/2)*exp(-((delta^2)/2)*(1/theta + theta)))/(sqrt(2*pi))
return(asNumeric(y))
}
