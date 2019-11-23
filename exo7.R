n = 10000
g <- function(x){
  sqrt(1-x^2)
}
f <- function(x,y){
  y^2+x^2
}
BM_pol <- funtion(n){
  u = runif(n, -1, 1)
  v = numeric(n)
  
  for ( i in 1:n){
    v[i] <- runif(1,-f(u[i]), f(u[i]) )
  }
  
  u * sqrt(-2 * log( u^2 + v^2 )  / ( u^2 + v^2 ))
  
}

t <- BM_pol(n)

hist(t)
#qqnorm(t)
#plot(u,v)


#BM.pol <- funtion(n){
  u = numeric(n)
  v = numeric(n)
  
  for(i in 1:n){
    u[i] <- runif(1, -1, 1)
    v[i] <- runif(1, -1, 1)
    while ( f(u[i],v[i]) > 1 ) {
      u[i] <- runif(1, -1, 1)
      v[i] <- runif(1, -1, 1)      
    }
  }
 t <- u * sqrt(-2 * log( u^2 + v^2 )  / ( u^2 + v^2 ))
 qqnorm(t)
 x <- rnorm(n)
 qqline(t, col='red')
#}

#t <- BM.pol(n)
#hist(t)
 
 #autre methodes?

n = 10000
r <- runif(n)
teta <- runif(n, 0, 2*pi)
u <- r*cos(teta)
v <- r*sin(teta)
t <- u * sqrt(-2 * log( u^2 + v^2 )  / ( u^2 + v^2 ))
qqnorm(t)
x <- rnorm(n)
qqline(t, col='red')

plot(u,v)
