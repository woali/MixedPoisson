\name{est.delta}
\alias{est.delta}

\title{Estimation of delta parameter of inverse-Gaussian distribution} 

\description{
The function estimates the value of the parameter delta using \eqn{\texttt{optimize}}.}

\usage{
est.delta(t)
}

\arguments{
  \item{t}{the vector of values}
}

\details{
The form of the distribution is as in the function \eqn{\texttt{ll.invGauss}}}

\value{
\item{nu}{the estimates of \eqn{\nu}}
\item{ll.delta.max}{the value of loglikehood}
}

\author{Michal Trzesiok}
\examples{est.delta(t=c(3,8))}
