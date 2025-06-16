# Función para simular el problema de Buffon
buffon_needle <- function(num_drops, needle_length, line_spacing) {
  crossings <- 0
  
  for (i in 1:num_drops) {
    # Generar aleatoriamente la distancia del centro de la aguja a la línea más cercana
    center_distance <- runif(1, 0, line_spacing )
    # Generar aleatoriamente el ángulo de la aguja
    angle <- runif(1, 0, pi / 2)
    
    # Calcular la mitad de la longitud de la aguja
    half_length <- needle_length / 2
    
    # Verificar si la aguja cruza una línea
    if ((center_distance <= half_length * sin(angle))||((line_spacing- half_length * sin(angle))<=center_distance )) {
      crossings <- crossings + 1
    }
  }
  
  # Calcular la probabilidad
  probability <- crossings / num_drops
  estimated_pi <- if (probability > 0) (2 * needle_length) / (line_spacing * probability) else 0
  
  return(list(estimated_pi = estimated_pi, crossings = crossings))
}


# Parámetros
num_drops <- 1000  # Número de lanzamientos de la aguja
needle_length <- 1   # Longitud de la aguja
line_spacing <- 2   # Distancia entre líneas

# Ejecutar la simulación
result <- buffon_needle(num_drops, needle_length, line_spacing)

# Mostrar resultados
cat("Estimacion de π:", result$estimated_pi, "\n")
cat("Numero de cruces:", result$crossings, "\n")



#Para dibujar
# Cargar librerías necesarias
library(ggplot2)

# Función para simular el problema de Buffon en un rectángulo
buffon_needle_plot_rect <- function(num_drops, needle_length, line_spacing, rect_width, rect_height) {
  crossings <- 0
  needles <- data.frame(x1 = numeric(0), y1 = numeric(0), x2 = numeric(0), y2 = numeric(0), color = character(0))
  
  for (i in 1:num_drops) {
    # Generar aleatoriamente el centro y el ángulo de la aguja
    x_center <- runif(1, 0, rect_width)  # Centro aleatorio dentro del ancho del rectángulo
    y_center <- runif(1, 0, rect_height)  # Centro aleatorio dentro de la altura del rectángulo
    angle <- runif(1, 0, pi / 2)
    
    # Calcular la mitad de la longitud de la aguja
    half_length <- needle_length / 2
    
    # Calcular las coordenadas de los extremos de la aguja
    #x1 <- x_center + half_length * cos(angle) - half_length * sin(angle)
    #y1 <- y_center + half_length * sin(angle) + half_length * cos(angle)
    #x2 <- x_center + half_length * cos(angle) + half_length * sin(angle)
    #y2 <- y_center + half_length * sin(angle) - half_length * cos(angle)
    
    x1 <- x_center + half_length * cos(angle) 
    x2 <- x_center - half_length * cos(angle) 
    y1 <- y_center + half_length * sin(angle) 
    y2 <- y_center - half_length * sin(angle) 
    # Comprobar si cruza alguna línea
    for (line in seq(0, rect_height, by = line_spacing)) {
      if ((y2 < line && y1> line)) {
        crossings <- crossings + 1
        color <- 'red'  # Aguja que cruza
        break
      } else {
        color <- 'blue'  # Aguja que no cruza
      }
    }
    
    # Agregar la aguja al data frame
    needles <- rbind(needles, data.frame(x1, y1, x2, y2, color))
  }
  
  # Calcular la probabilidad
  probability <- crossings / num_drops
  estimated_pi <- if (probability > 0) (2 * needle_length) / (line_spacing * probability) else 0
  
  # Graficar
  ggplot() +
    geom_segment(data = needles, aes(x = x1, y = y1, xend = x2, yend = y2, color = color), linewidth = 1) +
    geom_hline(yintercept = seq(0, rect_height, by = line_spacing), color = "black") +
    xlim(0, rect_width) + ylim(0, rect_height) +
    labs(title = paste("Estimacion de pi:", round(estimated_pi, 4), "\nNumero de cruces:", crossings, "/", num_drops),
         x = "X", y = "Y") +
    theme_minimal() +
    scale_color_identity()  # Usar colores originales
  
}

# Parámetros
num_drops <- 100  # Número de lanzamientos de la aguja
needle_length <- 1  # Longitud de la aguja
line_spacing <- 2   # Distancia entre líneas
rect_width <- 6     # Ancho del rectángulo
rect_height <- 10   # Altura del rectángulo

# Ejecutar la simulación y graficar

buffon_needle_plot_rect(num_drops, needle_length, line_spacing, rect_width, rect_height)     


# SI USO NORMAL MULTIVARIADA

#Para dibujar
# Cargar librerías necesarias
library(ggplot2)
library(MASS)

# Función para simular el problema de Buffon en un rectángulo
Normalbuffon_needle_plot_rect <- function(mu=c(0,0),Sigma=matrix(c(1, 0, 0, 1), nrow = 2) ,num_drops, needle_length, line_spacing, rect_width, rect_height) {
  crossings <- 0
  needles <- data.frame(x1 = numeric(0), y1 = numeric(0), x2 = numeric(0), y2 = numeric(0), color = character(0))
  
  for (i in 1:num_drops) {
    # Generar aleatoriamente el centro y el ángulo de la aguja
    punto<-mvrnorm(1, mu, Sigma)
    x_center <- punto[1]
    y_center <- punto[2]
    angle <- runif(1, 0, pi / 2)
    
    # Calcular la mitad de la longitud de la aguja
    half_length <- needle_length / 2
    
    x1 <- x_center + half_length * cos(angle) 
    x2 <- x_center - half_length * cos(angle) 
    y1 <- y_center + half_length * sin(angle) 
    y2 <- y_center - half_length * sin(angle) 
    # Comprobar si cruza alguna línea
    for (line in seq(mu[2]-rect_height, mu[2]+rect_height, by = line_spacing)) {
      if ((y2 < line && y1> line)) {
        crossings <- crossings + 1
        color <- 'red'  # Aguja que cruza
        break
      } else {
        color <- 'blue'  # Aguja que no cruza
      }
    }
    # Agregar la aguja al data frame
    needles <- rbind(needles, data.frame(x1, y1, x2, y2, color))
  }
  
  # Calcular la probabilidad
  probability <- crossings / num_drops
  estimated_pi <- if (probability > 0) (2 * needle_length) / (line_spacing * probability) else 0
  
  # Graficar
  ggplot() +
    geom_segment(data = needles, aes(x = x1, y = y1, xend = x2, yend = y2, color = color), linewidth = 1) +
    geom_hline(yintercept = seq(mu[2]-rect_height, mu[2]+rect_height, by = line_spacing), color = "black") +
    xlim(mu[1]-rect_width,mu[1]+ rect_width) + ylim(mu[2]-rect_height, mu[2]+rect_height) +
    labs(title = paste("Estimación de π:", round(estimated_pi, 4), "\nNúmero de cruces:", crossings, "/", num_drops),
         x = "X", y = "Y") +
    theme_minimal() +
    scale_color_identity()  # Usar colores originales
  
}

# Parámetros

num_drops <- 10000  # Número de lanzamientos de la aguja
needle_length <- 1  # Longitud de la aguja
line_spacing <- 2   # Distancia entre líneas
rect_width <- 4    # Ancho del rectángulo
rect_height <- 4   # Altura del rectángulo

# Ejecutar la simulación y graficar
# Vector de medias
mu <- c(0, 0)  # Media para dos variables
# Matriz de covarianza
Sigma <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)  # Covarianza
Normalbuffon_needle_plot_rect(mu,Sigma,num_drops, needle_length, line_spacing, rect_width, rect_height)     

