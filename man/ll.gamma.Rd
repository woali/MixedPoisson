\name{ll.gamma}
\alias{ll.gamma}

\title{Gamma Log-likelihood} 

\description{
The function returns the value of log-likelihood function for of Gamma distribution with one parameter \eqn{\gamma}.

}
\usage{
ll.gamma(gamma.par, t)
}

\arguments{
\item{gamma.par}{\eqn{\gamma} parameter}
  \item{t}{the vector of values}
}
\details{
The pdf of Gamma is of the form 
\eqn{f_\theta(\theta)=\frac{\gamma^\gamma}{\Gamma(\gamma)}\theta^{\gamma-1}\exp(-\gamma\theta)} }

\value{
\item{ll.gamma}{the value}
}

\author{Michal Trzesiok}
\examples{ll.gamma(1, c(3,8))}
