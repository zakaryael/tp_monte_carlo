n <- 10000

sigma <- matrix(c(4,3,3,9), 2,2)

A <-  chol(sigma)
u <- runif(n)
v <- runif(n)

X <- matrix( numeric(2*n), 2, n)
for(i in 1:n){
  z1 <- sqrt(-2 * log(u[i])) * cos(2 * pi * v[i]) 
  z2 <- sqrt(-2 * log(u[i])) * sin(2 * pi * v[i]) 
  Z <- c(z1, z2)
  X[,i] = A %*% Z + 1:2
  
}

Y = X[1,] + X[2,]

hist(Y, freq = F)

x <- seq(-10, 20, 1/100)
y <- dnorm(x, mean = 3, sd = sqrt(19))

lines(x,y, type = 'l', col = "red")


