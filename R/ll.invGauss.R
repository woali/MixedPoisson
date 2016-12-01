ll.invGauss <-
function(delta,t) {
delta <- mpfr(delta, 99)
y=log(prod((delta*exp(delta^2)*t^(-3/2)*exp(-((delta^2)/2)*(1/t + t)))/(sqrt(2*pi))))
return(asNumeric(y))
}
