
library(mvtnorm)
library(plotly)
library(ggplot2)

#EJEMPLO 

lambda <- 1

# --- 2. Generar una malla de puntos para la superficie 3D ---
x1_range <- seq(0, 5, length.out = 50)
x2_range <- seq(0,5, length.out = 50)

# Crear una malla (grid) de todos los pares (x1, x2)
grid_data <- expand.grid(X1 = x1_range, X2 = x2_range)

densi <- function(x_vec, lam) {
  x1 <- x_vec[1]
  x2 <- x_vec[2]
  # Ensure the conditions (x2 > x1) and (x1 > 0) are met
  # If conditions are not met, density is 0
  if (x2 > x1 ) {
    return(lam^2 * exp(-lam * x2))
  } else {
    return(0)
  }
}
grid_data$Density <- apply(grid_data, 1, densi, lam = lambda)

# Convert the density column to a matrix for plot_ly
z_matrix <- matrix(grid_data$Density, nrow = length(x1_range), byrow = FALSE)

# --- 3. Graficar la Densidad de la Normal Bivariada en 3D ---
fig_3d <- plot_ly(x = x1_range, y = x2_range, z = z_matrix, type = "surface") %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "Densidad")
    )
  )

fig_3d
#NORMALES MULTIVARIADAS

# --- 1. Definir los parámetros de la Normal Bivariada ---
mu <- c(0, 0) # Vector de medias
sigma <- matrix(c(1, 0, 0, 1), ncol = 2) # Matriz de covarianza (correlación de 0.7)

# --- 2. Generar una malla de puntos para la superficie 3D ---
x1_range <- seq(-3, 3, length.out = 50)
x2_range <- seq(-3, 3, length.out = 50)

# Crear una malla (grid) de todos los pares (x1, x2)
grid_data <- expand.grid(X1 = x1_range, X2 = x2_range)

# Calcular la densidad de probabilidad para cada punto (X1, X2) en la malla
# dmvnorm es la función de densidad de la normal multivariada
grid_data$Density <- dmvnorm(as.matrix(grid_data), mean = mu, sigma = sigma)

# Convertir la columna de densidad a una matriz para plot_ly
z_matrix <- matrix(grid_data$Density, nrow = length(x1_range), byrow = FALSE)

# --- 3. Graficar la Densidad de la Normal Bivariada en 3D ---
fig_3d <- plot_ly(x = x1_range, y = x2_range, z = z_matrix, type = "surface") %>%
  layout(
    title = "Densidad de Probabilidad de una Normal Bivariada",
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "Densidad")
    )
  )

fig_3d

# --- 4. Visualizar las Densidades Marginales (proyecciones en 2D) ---

# Densidad marginal de X1 (Normal Univariada)
# La media marginal es mu[1] y la varianza marginal es sigma[1,1]
p_marginal_X1 <- ggplot(data.frame(x = x1_range), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = mu[1], sd = sqrt(sigma[1,1])),
                color = "steelblue", size = 1.2) +
  labs(title = paste0("Densidad Marginal de X1 ~ N(", mu[1], ", ", sigma[1,1], ")"),
       x = "Valor de X1",
       y = "Densidad") +
  theme_minimal()

# Densidad marginal de X2 (Normal Univariada)
# La media marginal es mu[2] y la varianza marginal es sigma[2,2]
p_marginal_X2 <- ggplot(data.frame(x = x2_range), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = mu[2], sd = sqrt(sigma[2,2])),
                color = "darkred", size = 1.2) +
  labs(title = paste0("Densidad Marginal de X2 ~ N(", mu[2], ", ", sigma[2,2], ")"),
       x = "Valor de X2",
       y = "Densidad") +
  theme_minimal()

# Mostrar marginales
p_marginal_X1
p_marginal_X2

# --- 5. Visualizar una Densidad Condicional (una "rebanada" en 2D) ---

# Para X2 dado X1 = valor_condicional
valor_condicional_X1 <- 1

# Parámetros de la distribución condicional de X2 | X1 = x1
# Media condicional: mu_2_cond = mu[2] + sigma[2,1]/sigma[1,1] * (x1 - mu[1])
# Varianza condicional: sigma_2_cond = sigma[2,2] - sigma[2,1]^2/sigma[1,1]

mean_cond <- mu[2] + sigma[2,1] / sigma[1,1] * (valor_condicional_X1 - mu[1])
var_cond <- sigma[2,2] - sigma[2,1]^2 / sigma[1,1]
sd_cond <- sqrt(var_cond)

p_condicional_X2 <- ggplot(data.frame(x = x2_range), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean = mean_cond, sd = sd_cond),
                color = "darkgreen", size = 1.2) +
  labs(title = paste0("Densidad Condicional de X2 | X1 = ", valor_condicional_X1,
                      "\n~ N(", round(mean_cond, 2), ", ", round(var_cond, 2), ")"),
       x = "Valor de X2",
       y = "Densidad") +
  theme_minimal()

p_condicional_X2

# --- 6. Opcional: Agregar una "rebanada" de la condicional a la gráfica 3D ---
# Para visualizar la condicional en el gráfico 3D, podemos añadir una curva.
# Necesitamos los puntos (X1, X2, Densidad) a lo largo de esa rebanada.

# Seleccionar la fila correspondiente a valor_condicional_X1 en la malla
# Buscamos el índice más cercano al valor_condicional_X1
idx_x1_cond <- which.min(abs(x1_range - valor_condicional_X1))
x_cond_slice <- rep(x1_range[idx_x1_cond], length(x2_range))
y_cond_slice <- x2_range
z_cond_slice <- dmvnorm(cbind(x_cond_slice, y_cond_slice), mean = mu, sigma = sigma)

fig_3d_with_slice <- fig_3d %>%
  add_trace(x = x_cond_slice, y = y_cond_slice, z = z_cond_slice,
            type = "scatter3d", mode = "lines",
            line = list(color = "darkgreen", width = 8),
            name = paste0("X2 | X1 = ", round(valor_condicional_X1, 2))) %>%
  layout(
    scene = list(
      annotations = list(
        list(
          x = x1_range[idx_x1_cond],
          y = max(x2_range),
          z = max(z_matrix) * 0.8, # Ajustar la altura de la etiqueta
          text = paste0("Corte Condicional X1 = ", round(valor_condicional_X1, 2)),
          ax = 50,
          ay = -50,
          arrowhead = 6,
          arrowsize = 1,
          arrowwidth = 2
        )
      )
    )
  )

fig_3d_with_slice
