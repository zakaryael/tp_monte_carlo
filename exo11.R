n <- 100000

sigma <- matrix(c(1,0.5,0.5,1), 2,2)

A <-  chol(sigma)
u <- runif(n)
v <- runif(n)

X <- matrix( numeric(2*n), 2, n)
for(i in 1:n){
  z1 <- sqrt(-2 * log(u[i])) * cos(2 * pi * v[i]) 
  z2 <- sqrt(-2 * log(u[i])) * sin(2 * pi * v[i]) 
  Z <- c(z1, z2)
  X[,i] = A %*% Z
}
x1 <- X[1,]
x2 <- X[2,]
sd <- 1
y <- 1/2 * exp(-sd^2 / 2 + sd * x1) + 1/2 * exp(-sd^2 / 2 + sd * x2) - 1
mean(y * (y > 0))

plot(y)