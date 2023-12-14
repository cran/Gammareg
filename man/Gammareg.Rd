\name{Gammareg}
\alias{Gammareg}
\title{Gammareg}
\description{Function to do Classic Gamma Regression: joint mean and shape modeling}
\usage{
Gammareg(formula1,formula2,meanlink)
}
\arguments{
  \item{formula1}{object of class matrix, with the dependent variable.}
  \item{formula2}{object of class matrix, with the variables for modelling the mean.}
  \item{meanlink}{links for the mean. The default links is the link log. The link identity is also allowed as admisible value.}
 }
\details{
The classic gamma regression allow the joint modelling of mean and shape parameters of a gamma distributed variable, 
as is proposed in Cepeda (2001), using the Fisher Socring algorithm, with two differentes link for the mean: log and identity, and log link for the shape. 
}
\value{
 object of class bayesbetareg with:
 \item{coefficients}{object of class matrix with the estimated coefficients of beta and gamma.}
 \item{desvB}{object of class matrix with the estimated covariances of beta.}
 \item{desvG}{object of class matrix with the estimated covariances of gamma.}
 \item{interv}{object of class matrix with the estimated confidence intervals of beta and gamma.}
 \item{AIC}{the AIC criteria.}
 \item{iteration}{numbers of iterations to convergence.}
 \item{convergence}{value of convergence obtained.}
 \item{call}{Call.} 
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
\examples{

# 

num.killed <- c(7,59,115,149,178,229,5,43,76,4,57,83,6,57,84)
size.sam <- c(1,2,3,3,3,3,rep(1,9))*100
insecticide <- c(4,5,8,10,15,20,2,5,10,2,5,10,2,5,10)
insecticide.2 <- insecticide^2
synergist <- c(rep(0,6),rep(3.9,3),rep(19.5,3),rep(39,3))

par(mfrow=c(2,2))
plot(density(num.killed/size.sam),main="")
boxplot(num.killed/size.sam)
plot(insecticide,num.killed/size.sam)
plot(synergist,num.killed/size.sam)


mean.for  <- (num.killed/size.sam) ~ insecticide  + insecticide.2
dis.for <-  ~ synergist + insecticide

res=Gammareg(mean.for,dis.for,meanlink="ide")

summary(glm((num.killed/size.sam) ~ insecticide  + insecticide.2,family=Gamma("log")))
summary(res)

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
a=Gammareg(formula.mean,formula.shape,meanlink="ide")
summary(a)

}
\keyword{Classic estimation}
\keyword{Fisher Scoring}
\keyword{joint modeling}
\keyword{Gamma regression}
\keyword{Mean and shape parameters}

