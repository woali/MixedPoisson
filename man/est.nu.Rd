\name{est.nu}
\alias{est.nu}

\title{Estimation of nu parameter of log-normal distribution} 

\description{
The function estimates the value of the parameter nu using \eqn{\texttt{optimize}}.}

\usage{
est.nu(t)
}

\arguments{
  \item{t}{the vector of values}
}

\details{
The form of the distribution is as in the function \eqn{\texttt{ll.lognorm}}}

\value{
\item{nu}{the estimates of \eqn{\nu}}
\item{ll.nu.max}{the value of loglikehood}
}

\author{Michal Trzesiok}
\examples{est.nu(t=c(3,8))}
