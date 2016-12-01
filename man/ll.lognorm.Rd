\name{ll.lognorm}
\alias{ll.lognorm}

\title{Log-normal Log-likelihood} 

\description{
The function returns the value of log-likelihood function of log-normal distribution with one parameter \eqn{\nu}.
}

\usage{
ll.lognorm(nu, t)
}

\arguments{
\item{nu}{\eqn{\nu} parameter}
  \item{t}{the vector of values}
}

\details{
The pdf of log-normal is of the form \eqn{f_\theta(\theta)=\frac{1}{\sqrt{2\pi\nu\theta}}\exp[-\frac{(\log(\theta)+\frac{\nu^2}{2})^2}{2\nu^2}] }}

\value{
\item{ll.lognorm}{the value}
}

\author{Michal Trzesiok}
\examples{ll.lognorm(1, c(3,8))}
