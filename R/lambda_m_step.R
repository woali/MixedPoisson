lambda_m_step <-function(variable, X, offset){
g=glm(variable~-1+X, offset=offset,  family=poisson(log))
lambda.new=exp(X%*%g$coefficients) 

outlist = list(lambda=lambda.new, beta=g$coefficients, glm=g)
    class(outlist) = "lambda_m_step"
    return(outlist)
}
