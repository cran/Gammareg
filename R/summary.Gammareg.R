summary.Gammareg <-
function(object, ...){
  
  covB = object$desvB
  covG = object$desvG
  desviacion = c(sqrt(diag(covB)),sqrt(diag(covG)))
  L = object$interv[,1]
  U = object$interv[,2]
  n = object$n
  r = object$r
  coef1 = coef(object)[1:nrow(covB)]
  coef2 = coef(object)[(nrow(covB)+1):length(coef(object))]

  t1 = abs(coef1/sqrt(diag(covB)))
  t2 = abs(coef2/sqrt(diag(covG)))
  
  p1 = round(pt(t1,n-r-1, lower.tail = FALSE),4)
  p2 = round(pt(t2,n-r, lower.tail = FALSE), 4)
  p = c(p1,p2)
  
  TAB <- cbind( Coefficient = coef(object),
                Deviation = desviacion,
                pvalue = p,
                L.CredIntv = L,
                U.CredIntv = U
  )
  
  colnames(TAB) <- c("Estimate", "Deviation", "p-value" , "L.Intv",  "U.Intv")
  
  
  res <- list(call=object$call, coefficients=TAB, covB=object$desvB, covG=object$desvG, 
  AIC=object$AIC, iteration=object$iteration, convergence=object$convergence)  
  
  class(res) <- "summary.Gammareg"
  res  
}
