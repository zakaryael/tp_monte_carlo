#Methodes 1:
n = 100000 
x <- numeric(n) # les realisations suivant f
for ( i in 1:n ){
  u <- 1; y <- 1
  while ( u > 2 / pi * sqrt(1 - y^2)){    #condition d'accep: u < f(y) = 2 / pi * sqrt( 1 - y^2)
    y <- runif(1, -1, 1) # simuler suivant g
    u <- runif(1, 0 , 2 / pi) # u unif sur [0, M*g(y)]
    #u <- runif(1, 0 , 4 / pi)
    if( u < 2 / pi * sqrt(1 - y^2))  x[i] <- y
  }
}

hist(x, freq = F)
t <- seq(-1, 1, 1/100)
y = 2 / pi * sqrt(1 - t^2)
lines(t,y)

#Methodes2

n = 100000
x <- numeric(n) # les realisations suivant f
for ( i in 1:n ){
  u <- 1; y <- 1
  while ( u > sqrt(1 - y^2) ){    #condition d'accep: u < f(y)/M*g(y)

    y <- runif(1, -1, 1) # simuler suivant g
    u <- runif(1, 0 , 1) # u unif sur [0, 1]

    if( u < sqrt(1 - y^2))  x[i] <- y

  }
}

hist(x, freq = F)
t <- seq(-1, 1, 1/100)
y = 2 / pi * sqrt(1 - t^2)
lines(t,y)

#methode 3
M <- 4 / pi
n = 100000
N = n*M * 1.05
u <- runif(N)
z <- runif(N, -1, 1)
x = z[u < sqrt(1 - z^2)]
length(x)
hist(x)

