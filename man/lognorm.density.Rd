\name{lognorm.density}
\alias{lognorm.density}
\title{Log-normal Density} 

\description{
The function returns the vector of values of density  function for of log-normal distribution with one parameter \eqn{\nu}.
}
\usage{
lognorm.density(theta, nu)
}

\arguments{
\item{theta}{the vector of values}
 \item{nu}{the parameter of log-normal distribution}
}

\details{
The pdf of log-normal is of the form \eqn{f_\theta(\theta)=\frac{1}{\sqrt{2\pi\nu\theta}}\exp[-\frac{(\log(\theta)+\frac{\nu^2}{2})^2}{2\nu^2}] }}

\value{
\item{lognorm.density(theta, nu)}{the density -- the vector of values}
}

\author{Michal Trzesiok}
\examples{lognorm.density(c(2,3,5,4,6,7,6), 5)}
