#Ejer 2
0.4*0.6/(200*0.02*0.02)

#Ejer 3
#e)aux2<-sqrt(100)*(0.3-0.23)/sqrt(0.23*0.77)
aux1<-sqrt(100)*(0.16-0.23)/sqrt(0.23*0.77)
pnorm(aux2)-pnorm(aux1)

#f)
aux2<-sqrt(100)*(0.2-0.1)/sqrt(0.1*0.9)
aux1<-sqrt(100)*(0-0.1)/sqrt(0.1*0.9)
pnorm(aux2)-pnorm(aux1)

#g)

36*0.23*0.77/(16*100)

0.1*0.9/100 

(6*0.23/4) - 1/4


(1+(4*0.1))/6
1.4/6

# d)
p<-0.1
n<-100
generoDado<-function(n,p)
{
  dado<-sample(1:6,n,replace = TRUE)
  dado12<-1*(dado %in% c(1,2))
  dado3456<-(1-dado3456)
  respuesta<-rbinom(n,1,0.5)*dado12+rbinom(n,1,p)*dado3456
(6*mean(respuesta)-1)/4
}
genero<-function(n,p)
{
  mean(rbinom(n,1,p))
}

simuDado<-c()
simu<-c()
set.seed(123)
for(i in 1:1000)
{
  simuDado<-c(simuDado,generoDado(100,0.1))
simu<-c(simu,genero(n,0.1))
}
simu
simuDado

par(mfrow=c(1,2))
hist(simuDado,freq = FALSE)
hist(simu,freq = FALSE)
