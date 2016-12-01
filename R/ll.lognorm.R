ll.lognorm <-
function(nu,t) {
nu <- mpfr(nu, 99)
y=log(prod(exp(-((log(t)+(nu^2)/2)^2)/(2*nu^2))/(sqrt(2*pi)*nu*t)))
return(asNumeric(y))
}
