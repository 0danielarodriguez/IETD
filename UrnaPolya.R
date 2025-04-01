simular_urna_polya <- function(R, B, c, n) {
  # Vectores para almacenar la proporción de bolas rojas
  proporciones <- rep(0,n)
  rojas<-rep(0,n)
  blancas<-rep(0,n)
  rojas[1]<-R
  blancas[1]<-B
  proporciones[1] <- rojas[1]/(rojas[1]+blancas[1])
  for (i in 2:n) {
    # Extraer una bola al azar (1 = roja, 0 = blanca)
    esroja <- rbinom(1, 1, prob = proporciones[1])
    
    # Actualizar la urna según el color extraído
    if (esroja == 1) {
      rojas[i] <- rojas[i-1] + c
      blancas[i] <- blancas[i-1] # Se agregan c bolas rojas
    }
    # Actualizar la urna según el color extraído
    if (esroja == 0) {
      blancas[i] <- blancas[i-1]+ c
      rojas[i] <- rojas[i-1]# Se agregan c bolas blancas
    }
    # Guardamos la proporción de bolas rojas en la iteración actual
    proporciones[i] <- rojas[i]/(rojas[i]+blancas[i])
  }
  
  return(cbind(rojas,blancas,proporciones))
}

set.seed(999)
resultado <- simular_urna_polya(R = 5, B = 10, c = 3,n = 1000)
resultado
# Gráfico de la evolución de la proporción de bolas rojas
plot(resultado[,3], type = "l", col = "red", lwd = 2, 
     xlab = "Extracciones", ylab = "Proporcion de bolas rojas", 
     main = "proporcion de bolas rojas en la urna de Polya")
PR<-5/(5+10)
abline(h=PR)

