# Distribucion de SUcesiones

# minimo de exponenciales
vals<- seq(0, 5, length.out = 1000)
densidad_mine<- function(n,t){dexp(vals,rate=n)}
densidad1 <-densidad_mine(1,vals) 
densidad2 <-densidad_mine(2,vals) 
densidad4 <-densidad_mine(4,vals) 
densidad20 <-densidad_mine(20,vals) 

plot(vals, densidad1, type = "l", 
     main="Minimo de exponenciales 1", xlab = "X",
     ylab = "Densidad", 
     col = "blue", 
     lwd = 2)

lines(vals, densidad2, type = "l", 
     col = "red", 
     lwd = 2)
lines(vals, densidad4, type = "l", 
     col = "green", 
     lwd = 2)
lines(vals, densidad20, type = "l", 
     col = "black", 
     lwd = 2)
# Agregar leyenda
legend("topright", 
       legend = c("n=1", "n=2", "n=4", "n=20"), 
       col = c("blue", "red", "green", "black"), 
       lwd = 2)



# Distribucion de SUcesiones

# minimo de exponenciales
vals<- seq(-5, 5, length.out = 1000)
densidad_mine<- function(n,t){dexp(vals,rate=n)}
densidad1 <-dnorm(vals,0,1) 
densidad2 <-dnorm(vals,0,1/2)
densidad4 <-dnorm(vals,0,1/4)
densidad20 <-dnorm(vals,0,1/20)

plot(vals, densidad1, type = "l", 
     main="Promedio de Normales 0 ,1",ylim=c(0,2), xlab = "X",
     ylab = "Densidad", 
     col = "blue", 
     lwd = 2)

lines(vals, densidad2, type = "l", 
      col = "red", 
      lwd = 2)
lines(vals, densidad4, type = "l", 
      col = "green", 
      lwd = 2)
lines(vals, densidad20, type = "l", 
      col = "black", 
      lwd = 2)
# Agregar leyenda
legend("topright", 
       legend = c("n=1", "n=2", "n=4", "n=20"), 
       col = c("blue", "red", "green", "black"), 
       lwd = 2)


# Maximo de uniformes
vals<- seq(0, 1, length.out = 1000)
densidad_maxu<- function(n,t){n*t^{n-1}}
densidad1 <-densidad_maxu(1,vals) 
densidad2 <-densidad_maxu(2,vals) 
densidad4 <-densidad_maxu(4,vals) 
densidad20 <-densidad_maxu(20,vals) 

plot(vals, densidad1, type = "l", 
     main="Maximo de uniformes 0 ,1 ", xlab = "X",
     ylim=c(0,2),ylab = "Densidad", 
     col = "blue", 
     lwd = 2)

lines(vals, densidad2, type = "l", 
      col = "red", 
      lwd = 2)
lines(vals, densidad4, type = "l", 
      col = "green", 
      lwd = 2)
lines(vals, densidad20, type = "l", 
      col = "black", 
      lwd = 2)
# Agregar leyenda
legend("topleft", 
       legend = c("n=1", "n=2", "n=4", "n=20"), 
       col = c("blue", "red", "green", "black"), 
       lwd = 2)


