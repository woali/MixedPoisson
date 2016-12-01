pseudo_values <-
function(variable, mixing=c("Gamma", "lognorm", "invGauss"), lambda=1, gamma.par=0.5, nu=1, delta=1, n=100){
nominator = c(); denominator = c()

rules = laguerre.quadrature.rules(n)
rule = rules[[n]]

if (mixing=="Gamma") {
for (i in 1:length(variable)) {
t.r.nominator = function(theta) {
theta <- mpfr(theta, 99)
y=Gamma.density(theta, gamma.par)*exp(-lambda*theta)*(theta)^(variable[i]+1)
return(asNumeric(y))
}

nominator[i] = (lambda)*laguerre.quadrature(t.r.nominator, rule, lower = 0, upper = Inf, weighted = FALSE)

if (is.nan(nominator[i])) 
stop("Function laguerre.quadrature failed to compute the integrals (returned 'NaN') in the E-step for the given data. Further computations interrupted.", call. = FALSE)

t.r.denominator = function(theta) {
theta <- mpfr(theta, 99)
y=Gamma.density(theta, gamma.par)*exp(-lambda*theta)*(theta)^(variable[i])
return(asNumeric(y))
}

denominator[i] = laguerre.quadrature(t.r.denominator, rule, lower = 0, upper = Inf, weighted = FALSE)
if (is.nan(denominator[i])) 
stop("Function laguerre.quadrature failed to compute the integrals (returned 'NaN') in the E-step for the given data. Further computations interrupted.", call. = FALSE)
}
}

if (mixing=="lognorm") {
for (i in 1:length(variable)) {
t.r.nominator = function(theta) {
theta <- mpfr(theta, 99)
y=(lambda*theta)^(variable[i]+1)*exp(-lambda*theta)*lognorm.density(theta, nu)
return(asNumeric(y))
}

nominator[i] = laguerre.quadrature(t.r.nominator, rule, lower = 0, upper = Inf, weighted = FALSE)

if (is.nan(nominator[i])) 
stop("Function laguerre.quadrature failed to compute the integrals (returned 'NaN') in the E-step for the given data. Further computations interrupted.", call. = FALSE)

t.r.denominator = function(theta) {
theta <- mpfr(theta, 99)
y=(lambda*theta)^(variable[i])*exp(-lambda*theta)*lognorm.density(theta, nu)
return(asNumeric(y))
}

denominator[i] = laguerre.quadrature(t.r.denominator, rule, lower = 0, upper = Inf, weighted = FALSE)
if (is.nan(denominator[i])) 
stop("Function laguerre.quadrature failed to compute the integrals (returned 'NaN') in the E-step for the given data. Further computations interrupted.", call. = FALSE)
}
}

if (mixing=="invGauss") {
for (i in 1:length(variable)) {
t.r.nominator = function(theta) {
theta <- mpfr(theta, 99)
y=(lambda*theta)^(variable[i]+1)*exp(-lambda*theta)*invGauss.density(theta, delta)
return(asNumeric(y))
}

nominator[i] = laguerre.quadrature(t.r.nominator, rule, lower = 0, upper = Inf, weighted = FALSE)

if (is.nan(nominator[i])) 
stop("Function laguerre.quadrature failed to compute the integrals (returned 'NaN') in the E-step for the given data. Further computations interrupted.", call. = FALSE)

t.r.denominator = function(theta) {
theta <- mpfr(theta, 99)
y=(lambda*theta)^(variable[i])*exp(-lambda*theta)*invGauss.density(theta, delta)
return(asNumeric(y))
}

denominator[i] = laguerre.quadrature(t.r.denominator, rule, lower = 0, upper = Inf, weighted = FALSE)
if (is.nan(denominator[i])) 
stop("Function laguerre.quadrature failed to compute the integrals (returned 'NaN') in the E-step for the given data. Further computations interrupted.", call. = FALSE)
}
}


#t = round(nominator/denominator)
t = nominator/denominator

outlist = list(pseudo_values=t, nominator=nominator, denominator=denominator)
class(outlist) = "pseudo_values"
return(outlist)
}
