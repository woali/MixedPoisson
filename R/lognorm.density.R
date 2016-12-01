lognorm.density <-
function(theta, nu) {
theta <- mpfr(theta, 99)
y=exp(-((log(theta)+(nu^2)/2)^2)/(2*nu^2))/(sqrt(2*pi)*nu*theta)
return(asNumeric(y))
}
