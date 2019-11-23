#box-muller

BM.car <- function(n){
  u <- runif(n/2)
  v <- runif(n/2)
  c(sqrt(-log(u))*cos(2*pi*v), sqrt(-log(u))*sin(2*pi*v))
}

x <- BM.car(n)
y <- rnorm(n)

qqplot(x,y, main = "Diagramme Quantile-Quantile")


