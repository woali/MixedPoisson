lambda_start <-function(variable, X){
g=glm(variable~-1+X, family=poisson(log))

lambda.start=exp(X%*%g$coefficients) 
outlist = list(lambda=exp(X%*%g$coefficients), beta=g$coefficients, glm=g)
    class(outlist) = "lambda_start"
    return(outlist)
}
