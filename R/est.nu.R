est.nu <-
function(t){
ll.lognorm = function(nu) {
nu <- mpfr(nu, 99)
y=log(prod(exp(-((log(t)+(nu^2)/2)^2)/(2*nu^2))/(sqrt(2*pi)*nu*t)))
return(asNumeric(y))
}
ll.nu.opt = NULL
ll.nu.opt = optimize(f = ll.lognorm, interval = c(0, 10), maximum=TRUE)
outlist = list(nu=ll.nu.opt$maximum, ll.nu.max=ll.nu.opt$objective)
class(outlist) = "est.nu"
return(outlist)
}
