#question1
M <- function(b, mean, sd){
  1 / pnorm((mean-b) / sd)
}  

trunc.norm <- function(n, b = 2, mean = 0, sd = 1)
{
  g <- dnorm #densite instrumentale
  C <- M(b, mean, sd)
  # -l'algo du rejet:
  x <- numeric(n)
   for ( i in 1:n){
    y <- rnorm(1)
    u <- runif(1,0, C * g(y))
    while( u > C * dnorm(y, mean, sd) * ( y >= b) ){
    y <- rnorm(1)
    u <- runif(1,0, C * g(y))
    }
    x[i] <- y
  }
  x
}


trunc.norm1.2 <- function(n, b = 2, mean = 0, sd = 1)
{
  # -l'algo du rejet:
  x <- numeric(n)
  C <- M(b, mean, sd)
  for ( i in 1:n){
    y <- rnorm(1)
    u <- runif(1)
    while (u > (y >= b)){
      y <- rnorm(1)
      u <- runif(1)
    }
    x[i] <- y
  }
  x
}

#question2:

trucn.norm2 <- function(n = 10000, b = 2, mean = 0, sd = 1){
  x <- numeric(n)
  alpha <- (b + sqrt(b*b + 4)) / 2
  for(i in 1:n){
    y <- rexp(1, alpha) + b
    u <- unif(1)
    while(u > exp((-(y - alpha)^2)/2)){
      y <- rexp(1, alpha) + b
      u <- unif(1)
    }
  x[i] <- y
  }
  x
}

#question 3
microbenchmark(trunc.norm1.2(100), trunc.norm2(100))
