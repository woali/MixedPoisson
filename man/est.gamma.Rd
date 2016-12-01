\name{est.gamma}
\alias{est.gamma}

\title{Estimation of gamma parameter of Gamma distribution} 

\description{
The function estimates the value of the parameter gamma using \eqn{\texttt{optimize}}.}

\usage{
est.gamma(t)
}

\arguments{
  \item{t}{the vector of values}
}

\details{
The form of the distribution is as in the function \eqn{\texttt{ll.gamma}}}

\value{
\item{gamma}{the estimates of \eqn{\gamma}}
\item{ll.gamma.max}{the value of loglikehood}
}

\author{Michal Trzesiok}
\examples{est.gamma(t=c(3,8))}
