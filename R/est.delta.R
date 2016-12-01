est.delta <-
function(t){
ll.invGauss = function(delta) {
delta <- mpfr(delta, 99)
y=log(prod((delta*exp(delta^2)*t^(-3/2)*exp(-((delta^2)/2)*(1/t + t)))/(sqrt(2*pi))))
return(asNumeric(y))
}
ll.delta.opt = NULL
ll.delta.opt = optimize(f = ll.invGauss, interval = c(0, 3), maximum=TRUE)
outlist = list(nu=ll.delta.opt$maximum, ll.delta.max=ll.delta.opt$objective)
class(outlist) = "est.delta"
return(outlist)
}
