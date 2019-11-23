source(exo9.R, local = T)
n = 2000
#question1

p <- 1 - pcauchy(5)
p

#question2
x <- rcauchy(n)
y <- (x>5)
plot(MC.estim.evol(y)$delta, type = "l", col = "red", main = "--")
p <- MC.estim(y)$delta_hat
#question3

x <- runif(n, 0, 1/5)
y <- dcauchy(z) / 5
plot(MC.estim.evol(y)$delta, type = "l", col = "blue")
p <- MC.estim(y)$delta_hat

#comparaison des deux methodes
estim1 <- function(n){
  x <- rcauchy(n)
  y <- (x>5)
  p <- MC.estim(y)$delta_hat
  list(delta = p, sigma2 = MC.estim(y)$sigma2)
}

estim2 <- function(n){
  x <- runif(n, 0, 1/5)
  y <- dcauchy(z) / 5
  p <- MC.estim(y)$delta_hat
  list(delta = p, sigma2 = MC.estim(y)$sigma2)
}

microbenchmark(estim1(2000), estim2(2000))

estim1(2000)$sigma2
estim2(2000)$sigma2