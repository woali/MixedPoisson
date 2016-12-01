\name{pl.dist}
\alias{pl.dist}

\title{
Poisson-Lindley Distribution
}
\description{
The function fits a mixed Poisson distribution, in which the random parameter follows Lindley distribution. As teh method of estimation Expectation-maximization algorithm is used.

}
\usage{
pl.dist(variable, p.start, epsylon)
}

\arguments{
  \item{variable}{
The count variable.
}
  
\item{p.start}{
The starting value of p parameter. Default to 0.1.
}
  \item{epsylon}{
Default to epsylon = 10^(-8)
}
}

\details{This function provides estimated parameters of the model 
\eqn{N|\lambda \sim Poisson(\lambda)} where \eqn{\lambda} parameter is also a random
variable follows Lindley distribution with hiperparameter \eqn{p}. The 
pdf of Lindley is of the form 
\eqn{f_\lambda(\lambda)=\frac{p^2}{p+1}(\lambda+1)\exp(-\lambda p)
 }. 
  }

\value{ \item{p}{the parameter of mixing Lindley distribution} 
\item{n.iter}{the number of steps in EM algorithm} 
}

\references{
Karlis, D. (2005). EM algorithm for mixed Poisson and other discrete distributions. Astin bulletin, 35(01), 3-24.
}

\examples{
library(MASS)
pLindley = pl.dist(variable=quine$Days)
print(pLindley)
}
