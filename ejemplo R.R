library(dplyr)

mllk <- function(wpar,x){
  zzz <- w2n(wpar)
  -sum(log(outer(x,zzz$lambda,dpois)%*%zzz$delta))
}

n2w <- function(lambda,delta){
  log(c(lambda, delta[-1]/(1-sum(delta[-1]))))
}

w2n <- function(wpar){
  m<- (length(wpar)+1)/2
  lambda<- exp(wpar[1:m])
  delta <- exp(c(0,wpar[(m+1):(2*m-1)]))
  return (list(lambda=lambda,delta=delta/sum(delta)))
}


x=c(13,14,10,9,13,18,24,18,11,19,28,18,14,18,17,17,25,18,17,13,8,12,14,22,17,15,16,17,16,
   16,8,23,12,13,20,20,16,20,22,17,18,20,17,32,23,14,23,14,17,16,24,13,12,15,9,12,10,23,
   9,12,14,11,9,17,12,13,11,10,23,15,21,18,15,10,16,14,17,14,17,14,13,11,10,15,13,13,11,
   14,10,6,19,13,13,12,17,20,14,17,12,18,12,14,13,14,15,11,11,14)
wpar <- n2w(c(1,1,1,1),c(0.5,0.17, 0.22, 1-0.5-0.17-0.22))
oli2<-w2n(nlm(mllk,wpar,x)$estimate)
oli2

