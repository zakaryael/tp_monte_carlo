n = 1000
t = seq(0,1, 1/n)
k = length(t)

z <- rnorm(k)
w = numeric(k)
for( i in 1:(k-1)){
  w[i+1] <- w[i] + 1*sqrt(t[i+1] - t[i]) * z[i]
}


plot(t,w, type = 'l' )
