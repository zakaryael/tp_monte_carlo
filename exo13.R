n <- 10000
# question1
k <- 2
p <- 1 - pnorm(k)
p

#quesion2
estim1 <- function(n){
  x <- rnorm(n)
  y <- (x > k)
  p <- MC.estim(y)$delta_hat
  
  sigma2 <-MC.estim(y)$sigma2
  i1 <- MC.estim.evol(y)$IC1
  i2 <- MC.estim.evol(y)$IC2
  delta <- MC.estim.evol(y)$delta
  list(y = y, delta_hat = p, sigma2 = sigma2, i1 = i1, i2 = i2, delta = delta)
}
z <- estim1(n)
plot(z$delta, type = "l")
lines(z$i1, type = 'l', col = 'red')
lines(z$i2, type = 'l', col = 'green')
z$delta_hat
z$sigma2
# question3
m = 2
estim2 <- function(n){
  x <- rnorm(n, mean = m)
  y <- (x > k) * exp((m^2 - 2 * m * x)/2)
  p <- MC.estim(y)$delta_hat
  
  sigma2 <-MC.estim(y)$sigma2
  i1 <- MC.estim.evol(y)$IC1
  i2 <- MC.estim.evol(y)$IC2
  delta <- MC.estim.evol(y)$delta
  list(y = y, delta_hat = p, sigma2 = sigma2, i1 = i1, i2 = i2, delta = delta)
}
z <- estim2(n)
plot(z$delta, type = "l")
lines(z$i1, type = 'l', col = 'red')
lines(z$i2, type = 'l', col = 'green')
z$delta_hat
z$sigma2

# question3
l = 2
estim3 <- function(n){
  x <- rexp(n, rate = l) + k
  y <-  exp(l * (x - k) - x^2 / 2) / ( l * sqrt(2 * pi))
  p <- MC.estim(y)$delta_hat
  
  sigma2 <-MC.estim(y)$sigma2
  i1 <- MC.estim.evol(y)$IC1
  i2 <- MC.estim.evol(y)$IC2
  delta <- MC.estim.evol(y)$delta
  list(y = y, delta_hat = p, sigma2 = sigma2, i1 = i1, i2 = i2, delta = delta)
}
z <- estim3(n)
plot(z$delta, type = "l")
lines(z$i1, type = 'l', col = 'red')
lines(z$i2, type = 'l', col = 'green')
z$delta_hat
z$sigma2


