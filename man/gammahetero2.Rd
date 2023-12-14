\name{gammahetero2}
\alias{gammahetero2}
\title{Classic gamma regression. Identity link for the mean}
\description{Performs the Classic Gamma Regression for joint modeling of mean and shape parameters.}
\usage{
gammahetero2(formula1, formula2)
}
\arguments{
  \item{formula1}{object of class formula. It describes yi and xi for the mean equation of the gamma regression.}
  \item{formula2}{object of class formula. It describes zi for the shape equation of the gamma regression.}
}
\details{
The classic gamma regression allow the joint modeling of mean and shape parameters of a gamma distributed variable, 
as is proposed in Cepeda (2001), using the Fisher Scoring algorithm, with log link for the mean  and  log link for the shape.  
}
\value{
  object of class \code{Gammareg} with the following:
  \item{X}{object of class matrix, with the variables for modelling the mean.}
  \item{Z}{object of class matrix, with the variables for modelling the shape.}
  \item{beta}{object of class matrix with the estimated coefficients of beta.}
  \item{gamma}{object of class matrix with the estimated coefficients of gamma.}
  \item{ICB}{object of class matrix with the estimated confidence intervals of beta.}
  \item{ICG}{object of class matrix with the estimated confidence intervals of gamma.}
  \item{CovarianceMatrixbeta}{object of class matrix with the estimated covariances of beta.}
  \item{CovarianceMatrixgamma}{object of class matrix with the estimated covariances of gamma.}
  \item{AIC}{the AIC criteria}
  \item{iteration}{numbers of iterations to convergence}
  \item{convergence}{value of convergence obtained}
}
\references{
1. Cepeda-Cuervo, E. (2001). Modelagem da variabilidade em modelos lineares generalizados. Unpublished Ph.D. tesis. Instituto de Matemáticas. 
Universidade Federal do Río do Janeiro.
//http://www.docentes.unal.edu.co/ecepedac/docs/MODELAGEM20DA20VARIABILIDADE.pdf.
http://www.bdigital.unal.edu.co/9394/.
2. McCullagh, P. and Nelder, N.A. (1989). Generalized Linear Models. Second Edition. Chapman and Hall. 
}
\author{
 Martha Corrales \email{martha.corrales@usa.edu.co}  
 Edilberto Cepeda-Cuervo \email{ecepedac@unal.edu.co},        
}
\examples{

# Simulation Example

X1 <- rep(1,500)
X2 <- runif(500,0,30)
X3 <- runif(500,0,15)
X4 <- runif(500,10,20)
mui <- 15 + 2*X2 + 3*X3
alphai <- exp(0.2 + 0.1*X2 + 0.3*X4)
Y <- rgamma(500,shape=alphai,scale=mui/alphai)
X <- cbind(X1,X2,X3)
Z <- cbind(X1,X2,X4)
formula.mean= Y~X2+X3
formula.shape= ~X2+X4
a=gammahetero2(formula.mean,formula.shape)
a
}
\keyword{Gamma regression}
\keyword{Fisher Scoring}
\keyword{Joint modeling}
\keyword{Mean and shape parameters}

