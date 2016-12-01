\name{lambda_m_step}
\alias{lambda_m_step}

\title{Estimation of Lambda in M-step -- Expectation-Maximization (EM) algorithm} 

\description{
The function fits the GLM Poisson with given offset.}

\usage{
lambda_m_step(variable, X, offset)
}

\arguments{
\item{variable}{the vector of numbers}
\item{X}{model matrix of the form \eqn{X=model.matrix(~regressor)}. In the model without regressor the \eqn{X} sould be defined as \eqn{X=as.matrix(rep(1, length(variable)))}} 
\item{offset}{offset in GLM Poisson}
}

\details{It fits the GLM Poisson, where \eqn{variable \sim 1} and the ofsset is given as the vector of the variable's length. 
The results are used in M-step of EM algorithm, cf. [Karlis, 2012] pp. 6850.
}

\value{
\item{lambda}{\eqn{\hat \lambda=\hat \beta X}}
\item{beta}{regressor parameters}
\item{glm}{output of \eqn{\texttt{glm}}}
}

\author{Alicja Wolny--Dominiak, Michal Trzesiok}
\examples{
set.seed(1234)
variable=rpois(50,4)
X=as.matrix(rep(1, length(variable)))
t=pseudo_values(variable, mixing=c("invGauss"), lambda=4, delta=1, n=100)
lambda_m_step(variable, X, offset=t$pseudo_values)
}
