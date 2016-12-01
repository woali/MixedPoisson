\name{Gamma.density}
\alias{Gamma.density}
\title{Gamma density} 


\description{
The function returns the vector of values of density function for of Gamma distribution with one parameter \eqn{\gamma}.
}
\usage{
Gamma.density(theta, gamma.par)
}

\arguments{
\item{theta}{the vector of values}
 \item{gamma.par}{the parameter of Gamma distribution}
}

\details{
The pdf of Gamma is of the form 
\eqn{f_\theta(\theta)=\frac{\gamma^\gamma}{\Gamma(\gamma)}\theta^{\gamma-1}\exp(-\gamma\theta)} }

\value{
\item{Gamma.density(theta, nu)}{the density -- the vector of values}
}

\author{Michal Trzesiok}
\examples{Gamma.density(c(2,3,5,4,6,7,4), 5)}
