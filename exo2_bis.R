n = 10000
lambda = 2

#question 1: simulation d'une loi exponentielle
u = runif(n)
v <- -log(u)/lambda
hist(v, breaks = 100, freq = F, main = "loi exp de par lamda simulee", col = "tomato3")
x <- seq(0,8, 1/1000)
y <- dexp(x, rate = 2)
lines(x, y, type = "l")

#question 2: simulation d'une loi gamma 10
s = 0
for(i in 1:10){
  u = runif(n)
  s = s - log(u)/lambda
}

hist(s, freq = F, main = "loi de gamma simulee", col = "blue")
z <- dgamma(x,10 , rate = lambda)
lines(x,z, type = "l")
#question 2: methode matricielle?


# question 3: simulation d'une loi de poisson

N = 0
s = 0

N = numeric(n)
s = numeric(n)
for(i in 1:n){
  while(s[i] <= 1){
    u = runif(1)
    s[i] = s[i] - log(u)/lambda
    N[i] = N[i] + 1
  }
}
barplot(table(N)/n, main = "simulation poisson")
t <- rpois(n, lambda)
barplot(table(t)/n, main = "loi de poisson dbsa7")

# poisson methode 2 

N = numeric(n)
s = numeric(n)
test = s <= 1
while(any(test)){
  i = sum(test)
  u = runif(i)
  s[test] = s[test] - log(u)/lambda
  N[test] = N[test] + test[test]
  test = s <= 1
}
par(mfrow = c(1,2))
barplot(table(N)/n, main = "simulation poisson")
t <- rpois(n, lambda)
barplot(table(t)/n, main = "loi de poisson de R")

