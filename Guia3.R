# Ejer 3
#a)
dbinom(100,110,0.9)

pbinom(100,110,0.9)

sum(dbinom(0:100,110,0.9))

#b)
Emes<-110:200
probEmes<-pbinom(100,Emes,0.9)
plot(Emes,probEmes,type="l",xlab = "venden M pasajes",ylab = "probabilidad que todos viajen")


Emes<-110:130
probEmes<-pbinom(100,Emes,0.9)
plot(Emes,probEmes,type="l",xlab = "venden M pasajes",ylab = "probabilidad que todos viajen")

#f)
rbinom(1,110,0.9)
ganancia<-function(x)
{
  500*110-1000*max(0,x-100)-40000
}

Vganancia<-Vectorize(ganancia)

#ganancia si se presentan menos de 100
500*110-40000
plot(0:110,Vganancia(0:110),type="l")

set.seed(57)
vuelos<-rbinom(100,110,0.9)
Vganancia(vuelos)

hist(Vganancia(vuelos))
sum(Vganancia(vuelos)>=14000)
sum(Vganancia(vuelos)>=13000 &Vganancia(vuelos)<14000 )

hist(Vganancia(vuelos),freq=FALSE)

mean(Vganancia(vuelos)>=14000)

#altura del histograma debe ser tal que el area sea la frecuencia

(mean(Vganancia(vuelos)>=14000))/1000

mean(Vganancia(vuelos)>=13000 &Vganancia(vuelos)<14000 )/1000

hist(Vganancia(vuelos),freq=FALSE)



#g)


vuelos<-rbinom(100,110,0.9)
hist(Vganancia(vuelos),freq = FALSE)


#h)

vuelos<-rbinom(10000,110,0.9)
hist(Vganancia(vuelos),freq = FALSE)


#la puntual verdadera de la ganancia
probganancia<-function(y)
{
  (y<=100)*pbinom(100,110,0.9)+(y>100)*dbinom(y,110,0.9)
}

Vprobganancia<-Vectorize(probganancia)
plot(Vganancia(0:110),Vprobganancia(0:110))

par(mfrow=c(1,2))
plot(Vganancia(0:110),Vprobganancia(0:110))
hist(Vganancia(vuelos),freq = FALSE)


#i) repetir lo anterior con 0.95 


vuelos<-rbinom(10000,110,0.95)
hist(Vganancia(vuelos),freq = FALSE)


#la puntual verdadera de la ganancia
probganancia<-function(y)
{
  (y<=100)*pbinom(100,110,0.95)+(y>100)*dbinom(y,110,0.95)
}

Vprobganancia<-Vectorize(probganancia)
plot(Vganancia(0:110),Vprobganancia(0:110))

par(mfrow=c(1,2))
plot(Vganancia(0:110),Vprobganancia(0:110))
hist(Vganancia(vuelos),freq = FALSE)

#

vuelos<-rbinom(10000,110,0.99)
hist(Vganancia(vuelos),freq = FALSE)


#la puntual verdadera de la ganancia
probganancia<-function(y)
{
  (y<=100)*pbinom(100,110,0.99)+(y>100)*dbinom(y,110,0.99)
}

Vprobganancia<-Vectorize(probganancia)
plot(Vganancia(0:110),Vprobganancia(0:110))

par(mfrow=c(1,2))
plot(Vganancia(0:110),Vprobganancia(0:110))
hist(Vganancia(vuelos),freq = FALSE)


#j)
set.seed(57)

simuloparaM<-function(M)
{
vuelo<-rbinom(10000,M,0.9)
Z<-(vuelo<=100)*0+(vuelo>100)*(vuelo-100) #esto no lo sabe hacer max(0,vuelo-100)
gananciaM<-500*M-1000*Z-40000
mean(gananciaM>0)
}

VsimuloparaM<-Vectorize(simuloparaM)

set.seed(57)
emes<-100:300
plot(emes,VsimuloparaM(emes))
abline(h=0.9)
which(VsimuloparaM(emes)>0.9)
emes[which(VsimuloparaM(emes)>0.9)]

cbind(emes,round(VsimuloparaM(emes),6))

#k)
# Y es binomial 110 0.9

sum(0:110*dbinom(0:110,110,0.9))
110*0.9


# Z tiene esta prob

ganancia<-function(x)
{
  500*110-1000*max(0,x-100)-40000
}
Vganancia<-Vectorize(ganancia)

probganancia<-function(y)
{
  (y<=100)*pbinom(100,110,0.9)+(y>100)*dbinom(y,110,0.9)
}
Vprobganancia<-Vectorize(probganancia)
Vganancia(0:110)
Vprobganancia(0:110) #agrego V para poder calcular sobre vectores

unique(Vganancia(0:110))
unique(Vprobganancia(0:110))
sum(unique(Vganancia(0:110))*unique(Vprobganancia(0:110)))

#l) 

vuelos<-rbinom(10000,110,0.9)
mean(Vganancia(vuelos))


#########################################
#Ejer 6

#b)
(1/2)^12


#c) criterio 0
dbinom(12,12,0.5)
# criterio 1
dbinom(11,12,0.5)+dbinom(12,12,0.5)
# criterio 2
dbinom(10,12,0.5)+dbinom(11,12,0.5)+dbinom(12,12,0.5)
# criterio 3
dbinom(9,12,0.5)+dbinom(10,12,0.5)+dbinom(11,12,0.5)+dbinom(12,12,0.5)


#f) criterio 0
dbinom(12,12,0.8)
# criterio 1
dbinom(11,12,0.8)+dbinom(12,12,0.8)
# criterio 2
dbinom(10,12,0.8)+dbinom(11,12,0.8)+dbinom(12,12,0.8)
# criterio 3
dbinom(9,12,0.8)+dbinom(10,12,0.8)+dbinom(11,12,0.8)+dbinom(12,12,0.8)

dbinom(12,12,0.9)
# criterio 1
dbinom(11,12,0.9)+dbinom(12,12,0.9)
# criterio 2
dbinom(10,12,0.9)+dbinom(11,12,0.9)+dbinom(12,12,0.9)
# criterio 3
dbinom(9,12,0.9)+dbinom(10,12,0.9)+dbinom(11,12,0.9)+dbinom(12,12,0.9)


#f) criterio 0
1-dbinom(12,12,0.8)
# criterio 1
1-(dbinom(11,12,0.8)+dbinom(12,12,0.8))
# criterio 2
1-(dbinom(10,12,0.8)+dbinom(11,12,0.8)+dbinom(12,12,0.8))
# criterio 3
1-(dbinom(9,12,0.8)+dbinom(10,12,0.8)+dbinom(11,12,0.8)+dbinom(12,12,0.8))

   1-(dbinom(12,12,0.9))
# criterio 1
1-(dbinom(11,12,0.9)+dbinom(12,12,0.9))
# criterio 2
1-(dbinom(10,12,0.9)+dbinom(11,12,0.9)+dbinom(12,12,0.9))
# criterio 3
1-(dbinom(9,12,0.9)+dbinom(10,12,0.9)+dbinom(11,12,0.9)+dbinom(12,12,0.9))
