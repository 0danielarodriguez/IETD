# ejer 5
#a)
enfermo<-0.001

sano<-1-enfermo
#b)
PositivoDadoEnfermo<-0.95
NegativoDadoSano<-0.99

NegativoDadoEnfermo<- 1- PositivoDadoSano
PositivoDadoSano<-1- NegativoDadoSano

Positivo<- PositivoDadoSano * sano  + PositivoDadoEnfermo * enfermo
  
Positivo

EnfermoDadoPositivo<-PositivoDadoEnfermo * enfermo/Positivo

EnfermoDadoPositivo

#odd a priori
enfermo/sano
#odd a posteriori
EnfermoDadoPositivo/(1-EnfermoDadoPositivo)

#b)
funcpositivo<-function(p)
{
  enfermo<-p
  
  sano<-1-enfermo
  PositivoDadoEnfermo<-0.95
  NegativoDadoSano<-0.99
  NegativoDadoEnfermo<- 1- PositivoDadoSano
  PositivoDadoSano<-1- NegativoDadoSano
  Positivo<- PositivoDadoSano * sano  + PositivoDadoEnfermo * enfermo
  EnfermoDadoPositivo<-PositivoDadoEnfermo * enfermo/Positivo
  EnfermoDadoPositivo
}

funcpositivo(0.001)

Vfuncpositivo<-Vectorize(funcpositivo)

plot(seq(0,1,length=10000),Vfuncpositivo(seq(0,1,length=10000)))

plot(seq(0,1,length=10000),Vfuncpositivo(seq(0,1,length=10000)),type = "l",xlab="prob. enfermo",ylab = "enfermo dado positivo")

#######################
#ejer 6
#a)
sample(1:28,1,replace = TRUE)
etiqueta<-c(rep("roja marcada",6),rep("roja NO marcada",14),rep("Verde marcada",6),rep("Verde NO marcada",2))
etiqueta
etiqueta[sample(1:28,1,replace = TRUE)]

#forma 1
set.seed(57)
mean(sample(1:28,1000,replace = TRUE)<=20)
#forma 2
set.seed(57)
mean(etiqueta[sample(1:28,1000,replace = TRUE)] %in% c("roja marcada","roja NO marcada"))

fA<-mean(sample(1:28,1000,replace = TRUE)<=20)
#b)

set.seed(57)
sorteo<-sample(1:28,1000,replace = TRUE)

sum(sorteo<=6)/sum(sorteo<=6 | (sorteo>=21&sorteo<=26))

mean(sorteo<=6)/mean(sorteo<=6 | (sorteo>=21&sorteo<=26))
fAdB<-sum(sorteo<=6)/sum(sorteo<=6 | (sorteo>=21&sorteo<=26))

  
pA<-5/7
pAdB<-1/2

abs(pA-fA)
abs(pAdB-fAdB)

#fA aproxima mejor

#c)

guardopA<-c()
guardopAdB<-c()
set.seed(123)
for( i in 1:100)
{
  sorteo<-sample(1:28,1000,replace = TRUE)
  guardopA<-c(guardopA,mean(sorteo<=20))
  guardopAdB<-c(guardopAdB,sum(sorteo<=6)/sum(sorteo<=6 | (sorteo>=21&sorteo<=26)))
}
guardopA
guardopAdB

abs(guardopA-pA)
abs(guardopAdB-pAdB)

sum(abs(guardopA-pA)<abs(guardopAdB-pAdB))
#el 67% de las veces fA aproxima mejor

################
# Ejercicio 2.7