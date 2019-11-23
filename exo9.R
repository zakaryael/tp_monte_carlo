n = 1000
y <- runif(n)

MC.estim <- function(y, level = 0.05){
  delta <- mean(y)
  q <- qnorm(1 - level/2)
  sigma <-  sqrt(var(y))
  n <- length(y)
  i1 <- delta - q * sigma / sqrt(n)
  i2 <- delta + q * sigma / sqrt(n)
  list(delta_hat = delta, IC_hat = c(i1, i2), sigma2 = sigma * sigma)
}

# MC.estim.evol <- function(y, level = 0.05){
  # n <- length(y)
#   Delta <- numeric(n)
#   IC <- vector(mode = "list", length = n)
#   for (i in 1:n){
#     Delta[i] <- MC.estim(y[1:i], level)$delta_hat
#     i1 <- MC.estim(y[1:i], level)$IC_hat[1]
#     i2 <- MC.estim(y[1:i], level)$IC_hat[2]
#     #append(IC, c(i1, i2))
#     IC[i] <- c(i1, i2)
#   }
#   list(Delta = Delta, IC = IC)
# }

MC.estim.evol <- function(y, level = 0.05){
  n <- length(y)
  delta <- cumsum(y)/1:n
  sigma2 <- (cumsum(y^2) - (delta)^2  * (1:n)) /(0:(n-1))
  sigma2 <- c(0, sigma2[2:n])
  q <- qnorm(1 - level/2)
  i1 <- delta - q * sqrt(sigma2 / 1:n)
  i2 <- delta + q * sqrt(sigma2 / 1:n)
  list(delta = delta, IC1 = i1, IC2 = i2, sigma2 = sigma2)
}

x <- MC.estim.evol(y, 0.1)$delta
t <- 1:n
plot(x)