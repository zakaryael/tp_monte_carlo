#comparer deux fonctions

mpois <- function(n, lambda = 2){
  N <- numeric(n)
  s <- numeric(n)
  test <- s <= 1
  while(any(test)){
    i = sum(test)
    u = runif(i)
    s[test] <- s[test] - log(u)/lambda
    N[test] <- N[test] + test[test]
    test = s <= 1
  }
}

simu.pois<-function(n,lambda=2) { 
  u<-runif(n)
  x<- -log(u)/lambda
  i<-0
  l<-which(x<=1)
  ans<-rep(0,sum(x>1))
  while(length(l)>0){
    i<-i+1
    v<-runif(length(1))
    x<-x[l]-log(v)/lambda
    l<-which(x<=1)
    ans<-c(ans,rep(i,sum(x>1)))
  }    
}

library(microbenchmark)
n<-10000
test<-microbenchmark(mpois(n),simu.pois(n))
print(test, unit="ms",signif=2)