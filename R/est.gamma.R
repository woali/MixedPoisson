est.gamma <-
function(t){
ll.gamma <-function(gamma.par) {
gamma.par <- mpfr(gamma.par, 99)
y=log(prod((gamma.par^(gamma.par)*t^(gamma.par-1)*exp(-(gamma.par*t)))/(gamma(gamma.par))))
return(asNumeric(y))
}
ll.gamma.opt = NULL
ll.gamma.opt = optimize(f = ll.gamma, interval = c(0.1, 4), maximum=TRUE)
outlist = list(gamma.par=ll.gamma.opt$maximum, ll.gamma.max=ll.gamma.opt$objective)
class(outlist) = "est.gamma"
return(outlist)
}
