#fonction indicatrice de {x^2 + y^2 <= 1}

f <- function(x,y){
  if (x^2 + y^2 <= 1)  1
  else  0
}

#simuler suivant l'uniforme sur un carre U[0,1]^2
n = 1000
u = runif(n)
v = runif(n)

#estimer pi comme la moyenne des f(u_i)
delta = 0
for ( i in 1:n){
  for(j in 1:n){
    delta <- delta + f(u[i], v[j])
  }
}

delta <- delta / n^2
4*delta