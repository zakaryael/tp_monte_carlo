
#simuler suivant l'uniforme sur un carre U[0,1]^2
n = 1000
u = runif(n)
v = runif(n)
delta <- sum( u^2 + v^2 < 1)

MC.estim.evol(y)$delta