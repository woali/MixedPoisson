\name{pseudo_values}
\alias{pseudo_values}

\title{Pseudo values -- Expectation-Maximization (EM) algorithm} 

\description{
The function returns the pseudo values \eqn{t_i} defined as the conditional expectation \eqn{E[\theta_i|k_1,...,k_n]}, 
where \eqn{k_1,...,k_n} are realizations of the count variable N.}

\usage{
pseudo_values(variable, mixing=c("Gamma", "lognorm", "invGauss"), lambda, gamma.par, nu, delta, n)
}

\arguments{
\item{variable}{the vector of numbers}
\item{mixing}{the name of mixing distribution}
\item{lambda}{\eqn{\lambda} parameter in mixed Poisson model}
\item{gamma.par}{\eqn{\gamma} parameter in Gamma mixing distribution}
\item{nu}{\eqn{\nu} parameter in log-normal mixing distribution}
\item{delta}{\eqn{\delta} parameter in inverse-Gaussian mixing distribution}
\item{n}{The integer value for the Laguerre quadrature. Default to 100}
}

\details{
The function calculates the vector of pseudo values \eqn{t_i=E[\theta_i|k_1,...,k_n]} in E-step of EM algorithm. It applies the numerical integration using \eqn{laguerre.quadrature} 
in the nominator and the denominator of the formula

The proper parameter \eqn{\gamma}, \eqn{\nu}, \eqn{\delta} should be chosen according to the mixing distribution.
}

\value{
\item{pseudo_values}{pseudo values \eqn{t_1,...,t_n}}
\item{nominator}{nominator in the formula}
\item{denominator}{denominator in the formula}
}

\author{Alicja Wolny--Dominiak, Michal Trzesiok}

\examples{
variable=rpois(30,4)
pseudo_values(variable, mixing="Gamma", lambda=4, gamma.par=0.7, n=100)}
