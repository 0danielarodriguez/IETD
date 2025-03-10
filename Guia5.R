#Ejer 4

round(rbind(dbinom(0:8,8,0.8)*0.1,dbinom(0:8,8,0.3)*0.9),3)



sum(dbinom(0:4,8,0.8))*0.1+sum(dbinom(5:8,8,0.3))*0.9


sum(dbinom(c(1,3,5,7),8,0.8))*0.1+sum(dbinom(c(0,2,4,6,8),8,0.3))*0.9



  
  
errorht<-c((sum(dbinom(0:8,8,0.3))*0.9),(sum(dbinom(0:0,8,0.8))*0.1+sum(dbinom(1:8,8,0.3))*0.9),(sum(dbinom(0:1,8,0.8))*0.1+sum(dbinom(2:8,8,0.3))*0.9),
(sum(dbinom(0:2,8,0.8))*0.1+sum(dbinom(3:8,8,0.3))*0.9),(sum(dbinom(0:3,8,0.8))*0.1+sum(dbinom(4:8,8,0.3))*0.9),(sum(dbinom(0:4,8,0.8))*0.1+sum(dbinom(5:8,8,0.3))*0.9),
(sum(dbinom(0:5,8,0.8))*0.1+sum(dbinom(6:8,8,0.3))*0.9),(sum(dbinom(0:6,8,0.8))*0.1+sum(dbinom(7:8,8,0.3))*0.9),(sum(dbinom(0:7,8,0.8))*0.1+sum(dbinom(8:8,8,0.3))*0.9),
sum(dbinom(0:8,8,0.8))*0.1)

round(errorht,4)
