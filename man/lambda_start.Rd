\name{lambda_start}
\alias{lambda_start}

\title{Estimation of starting lambda in Expectation-Maximization (EM) algorithm} 

\description{
The function fits the GLM Poisson without regressors.}

\usage{
lambda_start(variable, X)
}

\arguments{
\item{variable}{the vector of numbers}
\item{X}{model matrix of the form \eqn{X=model.matrix(~regressor)}. In the model without regressor the \eqn{X} sould be defined as \eqn{X=as.matrix(rep(1, length(variable)))}} 
}

\details{It fits the GLM Poisson, where \eqn{variable \sim 1}. 
The results are taken as the starting value of EM algorithm.
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