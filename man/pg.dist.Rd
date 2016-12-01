\name{pg.dist}
\alias{pg.dist}


\title{
Poisson-Gamma Distribution (Negative-Binomial) 
}
\description{
The function fits a mixed Poisson distribution, in which the random parameter follows Gamma distribution (the negative-binomial distribution). As teh method of estimation Expectation-maximization algorithm is used.
In M-step the analytical formulas taken from [Karlis, 2005] are applied. 
}

\usage{
pg.dist(variable, alpha.start, beta.start, epsylon)
}

\arguments{
  \item{variable}{
The count variable.
}

  \item{alpha.start}{
The starting value of the parameter alpha. Default to 1.}

  \item{beta.start}{
The starting value of the parameter beta. Default to 0.3
}
   \item{epsylon}{
Default to epsylon = 10^(-8)
}
}

\details{This function provides estimated parameters of the model 
\eqn{N|\lambda \sim Poisson(\lambda)} where \eqn{\lambda} parameter is also a random
variable follows Gamma distribution with hiperparameters \eqn{\alpha, \beta}. The 
pdf of Gamma is of the form 
\eqn{f_\lambda(\lambda)=\frac{\lambda^{\alpha-1}\exp(-\beta\lambda)\beta^\lambda}{\Gamma(\alpha)}
 }. 
  }

\value{ \item{alpha}{the parameter of mixing Gamma distribution} 
\item{beta}{the parameter of mixing Gamma distribution} 
\item{theta}{the value 1/beta}
\item{n.iter}{the number of steps in EM algorithm} 
}

\references{
Karlis, D. (2005). EM algorithm for mixed Poisson and other discrete distributions. Astin bulletin, 35(01), 3-24.
}

\examples{
library(MASS)
pGamma1 = pg.dist(variable=quine$Days)
print(pGamma1)
}
