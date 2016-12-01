\name{ll.invGauss}
\alias{ll.invGauss}
\title{Inverse-Gaussian Log-likelihood} 

\description{
The function returns the value of log-likelihood function for of inverse-Gaussian distribution with one parameter \eqn{\delta}.
}
\usage{
ll.invGauss(delta, t)
}

\arguments{
\item{delta}{\eqn{\delta} parameter}
  \item{t}{the vector of values}
}

\details{
The pdf of inverse-Gaussian is of the form 
\eqn{f_\theta(\theta)=\frac{\delta}{2\pi}\exp(\delta^2)\theta^{-\frac{3}{2}} \exp(-\frac{\delta^2}{2}(\frac{1}{\theta}+\theta))}  }

\value{
\item{ll.invGauss}{the value}
}

\author{Michal Trzesiok}
\examples{ll.invGauss(1, c(3,8))}
