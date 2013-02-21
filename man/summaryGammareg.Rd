\name{summary.Gammareg}
\alias{summary.Gammareg}
\title{Print the Classic gamma regression}
\description{Summarized the Classic gamma regression for joint modelling of mean and shape parameters.}
\usage{
\method{summary}{Gammareg}(object, \ldots)
}
\arguments{
  \item{object}{an object of class Gammareg}
  \item{\ldots}{not used.}
}
\value{
\item{call}{Call}
\item{coefficients}{Coefficients}
\item{covB}{object of class matrix with the estimated covariances of beta.}
\item{covG}{object of class matrix with the estimated covariances of gamma.}
\item{AIC}{AIC}
\item{iteration}{number of iterations}
\item{convergence}{convergence obtained}
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
 Edilberto Cepeda-Cuervo \email{ecepedac@unal.edu.co}        
}
\keyword{Classic estimation}
\keyword{Fisher Scoring}
\keyword{joint modeling}
\keyword{Gamma regression}
\keyword{Mean and shape parameters}