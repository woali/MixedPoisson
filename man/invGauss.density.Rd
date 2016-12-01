\name{invGauss.density}
\alias{invGauss.density}
\title{inverse-Gaussian Density} 

\description{
The function returns the vector of values of density  function for of inverse-Gaussian distribution with one parameter \eqn{\delta}.
}
\usage{
invGauss.density(theta, delta)
}

\arguments{
\item{theta}{the vector of values}
 \item{delta}{the parameter of inverse-Gaussian distribution}
}

\details{
The pdf of inverse-Gaussian is of the form 
\eqn{f_\theta(\theta)=\frac{\delta}{2\pi}\exp(\delta^2)\theta^{-\frac{3}{2}} \exp(-\frac{\delta^2}{2}(\frac{1}{\theta}+\theta))}  }

\value{
\item{invGauss.density(theta, delta)}{the density -- the vector of values}
}

\author{Michal Trzesiok}
\examples{invGauss.density(c(2,3,5,4,6,7,6), 5)}
