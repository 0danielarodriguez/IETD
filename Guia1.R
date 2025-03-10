#Introducion Basica a R
# R como calculadora
3*2+2

pirulo=3*5
pirulo

8^5

sqrt(4)

exp(2)

sin(0)

#vectores
c(4,2,3)

unoatres=c(1,2,3)

unoatres

dias = c('lu','mar','mier','jue','vie')

dias

dias[4]

dias[2:4]


#de un bolillero con 10 bolillas numeradas de 1 a 10, saco 3 CON reposicion

sample(10,3,TRUE)

#Ahora, del mismo bolillero, saco 3 SIN reposicion

sample(10,3,FALSE)

sample(2,3,FALSE)

#llamo a al vector y le pido que me muestre a

a=sample(10,3,TRUE)
a

#pido que me muestre la posicion dos del vector, o sea, el numero
#de la segunda bolilla extraida

a[2]



#tambien podia directamente escribirlo sin la palabra "FALSE", ya que si uno
#usa el comando sample(,), y no aclara si es TRUE o FALSE, por default es FALSE.

sample(10,3)




#tengo bolillero con n bolillas y saco k. Puede ser k>n? Si es CON reposicion no hay problema, pero si es SIN reposicion 
#salta error, ya que no tengo suficientes bolillas para sacar!!

n = 4
k = 7
sample(n, k,TRUE)

n = 4
k = 7
sample(n, k,FALSE)



# escribo el vector con los numeros naturales
# desde el 5 hasta el 12 (inclusive)

5:12

#multiplico todos esos numeros

prod(5:12)


#otra forma de multiplicar los numeros del 5 al 12 inclusive es hacer
#"12 factorial" dividido "4 factorial". 

factorial(12)/factorial(4)


#ojo que no siempre se puede factorial

prod(451:500)

factorial(500)/factorial(450)






# Ejer 3
#a)
1 -factorial(365):factorial(315) / 365^50
1 - prod(316:365) / 365^50
1 - prod((365 - 50 + 1):365) / 365^50

#b) si en lugar de 50 van 20 invitados?

1 - prod((365 - 20 + 1):365) / 365^20

#otra forma (le decimos que haga la cuenta con "k" 
#alumnos, y decimos que k es igual a 20)

k<-20
1 - prod((365 - k + 1):365) / 365^k

#c) creo una funcion 

probcumple<-function(k)
{
  1 - prod((365 - k + 1):365) / 365^k
}

probcumple(20)

probcumple(50)
#me gustaria aplicarle la funcion a varios valore
probcumple(c(20,50))


# opcion 1
probcumple_vec <- Vectorize(probcumple)
probcumple_vec(c(20,50))

#otra opcion 
sapply(c(20,50),probcumple)

invitados<-2:120
probeninvitados<-probcumple_vec(invitados)

invitados
probeninvitados

plot(invitados,probeninvitados)

plot(invitados, probeninvitados, xlab = "numero de invitados", ylab = "probabilidad", main="Probabilidad de dos o mas coincidencias",type="l")
     
#d)
invitados[probeninvitados>0.6]
abline(v=27)

invitados[probeninvitados>0.5]
abline(v=23)


#e) 

sample(1:365,50,TRUE)   


set.seed(999)
misinvitados<-sample(1:365,50,TRUE)   
duplicated(misinvitados)
cbind(misinvitados,duplicated(misinvitados))

sum(duplicated(misinvitados))

#f)
probcumple(50)

repito10veces<-rep(0,10)
set.seed(57)
for(i in 1:10)
{
  misinvitados<-sample(1:365,50,TRUE)   
  coincidencia<-  1*(sum(duplicated(misinvitados))>0)
  repito10veces[i]<-coincidencia
}

repito10veces
#si repito 10 veces pero con 20 invitados?

repito10veces<-rep(0,10)
set.seed(57)
for(i in 1:10)
{
  misinvitados<-sample(1:365,20,TRUE)   
  coincidencia<-  1*(sum(duplicated(misinvitados))>0)
  repito10veces[i]<-coincidencia
}
repito10veces
mean(repito10veces)



repito10veces<-rep(0,10)
set.seed(57)
for(i in 1:10)
{
  misinvitados<-sample(1:365,50,TRUE)   
  coincidencia<-  1*(sum(duplicated(misinvitados))>0)
  repito10veces[i]<-coincidencia
}



repito1000veces<-rep(0,1000)
set.seed(57)
for(i in 1:1000)
{
  misinvitados<-sample(1:365,50,TRUE)   
  coincidencia<-  1*(sum(duplicated(misinvitados))>0)
  repito1000veces[i]<-coincidencia
}


repito100000veces<-rep(0,100000)
set.seed(57)
for(i in 1:100000)
{
  misinvitados<-sample(1:365,50,TRUE)   
  coincidencia<-  1*(sum(duplicated(misinvitados))>0)
  repito100000veces[i]<-coincidencia
}

repito10veces
repito1000veces
repito100000veces

mean(repito10veces)
mean(repito1000veces)
mean(repito100000veces)
probcumple(50)
