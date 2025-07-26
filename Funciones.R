

coherencia_wavelet <- function(series1, series2,
                               metodo_tendencia = c("mediana", "promedio"),
                               nrands = 10,
                               titulo = "Wavelet Coherencia",
                               ylab = "Escala",
                               xlab = "min",
                               col_coi = "grey",
                               lwd_coi = 2,
                               plot_phase = TRUE,
                               mostrar_tendencia = TRUE,
                               verbose = TRUE) {
  
  # Validar que las series tengan la misma longitud
  if (length(series1) != length(series2)) {
    stop("Las series deben tener la misma longitud.")
  }
  
  metodo_tendencia <- match.arg(metodo_tendencia)
  
  # Calcular tendencia central antes de la interpolación para usar los datos originales
  if (metodo_tendencia == "mediana") {
    tendencia1 <- median(series1, na.rm = TRUE)
    tendencia2 <- median(series2, na.rm = TRUE)
  } else { # "promedio"
    tendencia1 <- mean(series1, na.rm = TRUE)
    tendencia2 <- mean(series2, na.rm = TRUE)
  }
  
  # Mostrar valor tendencia si se requiere
  if (verbose && mostrar_tendencia) {
    message("Tendencia Series 1 (", metodo_tendencia, "): ", tendencia1)
    message("Tendencia Series 2 (", metodo_tendencia, "): ", tendencia2)
  }
  
  # Interpolación de NAs para mantener el paso de tiempo constante
  series1_interpolated <- na.approx(series1, na.rm = FALSE)
  series2_interpolated <- na.approx(series2, na.rm = FALSE)
  
  # Si la serie empieza o termina con NA, na.approx los deja.
  # Aquí los rellenamos con la tendencia calculada
  if (is.na(series1_interpolated[1])) {
    series1_interpolated[1] <- tendencia1
  }
  if (is.na(series2_interpolated[1])) {
    series2_interpolated[1] <- tendencia2
  }
  if (is.na(tail(series1_interpolated, 1))) {
    series1_interpolated[length(series1_interpolated)] <- tendencia1
  }
  if (is.na(tail(series2_interpolated, 1))) {
    series2_interpolated[length(series2_interpolated)] <- tendencia2
  }
  
  
  # Crear matrices con índices y datos interpolados
  t1 <- cbind(1:length(series1_interpolated), series1_interpolated)
  t2 <- cbind(1:length(series2_interpolated), series2_interpolated)
  
  # Aplicar la transformada wavelet de coherencia
  wtc_result <- wtc(t1, t2, nrands = nrands)
  
  # Graficar
  par(oma = c(0,0,0,1), mar = c(5,4,5,5) + 0.1)
  plot(wtc_result, plot.phase = plot_phase,
       xaxt='n', lty.coi = 1, col.coi = col_coi, lwd.coi = lwd_coi,
       lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12,
       ylab = ylab, xlab = xlab,
       plot.cb = TRUE, main = titulo)
}
#----------------------------------------------------------------------------------------
#Funcion, Calcula la energía en cada escala y tiempo

# Titulo de Funcion: Graficar la energía de la transformada wavelet de dos series de tiempo
# Descripcion: Esta función calcula la transformada wavelet de dos series de tiempo,
# extrae la energía en cada escala y tiempo, y genera un gráfico que
# muestra la distribución de la energía en función del tiempo y la escala.
#'
# @param t1 Un data.frame o matriz con la primera serie de tiempo.
#'           La primera columna debe ser el tiempo y la segunda los valores.
# @param t2 Un data.frame o matriz con la segunda serie de tiempo.
#'           La primera columna debe ser el tiempo y la segunda los valores.
# @param nombre_serie1 Un string con el nombre de la primera serie para el gráfico.
# @param nombre_serie2 Un string con el nombre de la segunda serie para el gráfico.
#'
# @return Un objeto de la clase ggplot que puede ser impreso para mostrar el gráfico.
#'
#'Ejemplo
#' # Ejemplo de uso
#' # t1 <- data.frame(tiempo = 1:100, valor = rnorm(100))
#' # t2 <- data.frame(tiempo = 1:100, valor = sin(seq(1, 10, length.out = 100)) + rnorm(100))
#' # graficar_energia_wavelet(t1, t2, "Serie A", "Serie B")
#'
graficar_energia_wavelet <- function(series1, series2, metodo_tendencia = c("mediana", "promedio"), mostrar_tendencia = TRUE
                                     ) {
  # Validar que las series tengan la misma longitud
  if (length(series1) != length(series2)) {
    stop("Las series deben tener la misma longitud.")
  }
  
  metodo_tendencia <- match.arg(metodo_tendencia)
  
  # Calcular tendencia central antes de la interpolación para usar los datos originales
  if (metodo_tendencia == "mediana") {
    tendencia1 <- median(series1, na.rm = TRUE)
    tendencia2 <- median(series2, na.rm = TRUE)
  } else { # "promedio"
    tendencia1 <- mean(series1, na.rm = TRUE)
    tendencia2 <- mean(series2, na.rm = TRUE)
  }
  
  # Mostrar valor tendencia si se requiere
  if ( mostrar_tendencia) {
    message("Tendencia Series 1 (", metodo_tendencia, "): ", tendencia1)
    message("Tendencia Series 2 (", metodo_tendencia, "): ", tendencia2)
  }
  
  # Interpolación de NAs para mantener el paso de tiempo constante
  series1_interpolated <- na.approx(series1, na.rm = FALSE)
  series2_interpolated <- na.approx(series2, na.rm = FALSE)
  
  # Si la serie empieza o termina con NA, na.approx los deja.
  # Aquí los rellenamos con la tendencia calculada
  if (is.na(series1_interpolated[1])) {
    series1_interpolated[1] <- tendencia1
  }
  if (is.na(series2_interpolated[1])) {
    series2_interpolated[1] <- tendencia2
  }
  if (is.na(tail(series1_interpolated, 1))) {
    series1_interpolated[length(series1_interpolated)] <- tendencia1
  }
  if (is.na(tail(series2_interpolated, 1))) {
    series2_interpolated[length(series2_interpolated)] <- tendencia2
  }
  # Crear matrices con índices y datos interpolados
  t1 <- cbind(1:length(series1_interpolated), series1_interpolated)
  t2 <- cbind(1:length(series2_interpolated), series2_interpolated)
  
  # 1. Realizar la transformada wavelet de cada serie
  wt1 <- wt(t1)
  wt2 <- wt(t2)
  
  
  # 2. Extraer energía y crear matrices de energía
  n_scales <- nrow(wt1$power)
  n_times <- ncol(wt1$power)
  energy1_mat <- abs(wt1$power)
  energy2_mat <- abs(wt2$power)
  
  nombre_serie1 <- deparse(substitute(series1))
  nombre_serie2 <- deparse(substitute(series2))

 
  
  nombre_serie <- series1
  # 3. Convertir a data frames en formato largo
  df1 <- data.frame(
    tiempo = rep(t1[,1], times = n_scales),
    escala = rep(wt1$period, each = n_times),
    energia = as.vector(energy1_mat),
    serie = nombre_serie1
  )
  
  df2 <- data.frame(
    tiempo = rep(t1[,1], times = n_scales),
    escala = rep(wt2$period, each = n_times),
    energia = as.vector(energy2_mat),
    serie = nombre_serie2
  )
  
  # 4. Unir ambos data frames
  df_energy <- rbind(df1, df2)
  
  # 5. Generar y devolver el gráfico
  ggplot(df_energy, aes(x = tiempo, y = escala, fill = energia)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("blue", "yellow", "red")) +
    scale_x_continuous(name = "Minuto") +
    scale_y_log10(name = "Escala (log)") +
    ggtitle("Energía en función del tiempo y la escala") +
    theme_minimal()
}

#--------------------------------------------------------------------------------------------
#' Graficar Serie Original vs. Serie Filtrada por Wavelet
#'
#' Esta función filtra una serie de tiempo utilizando un análisis de wavelet
#' y luego visualiza la serie original junto a la versión filtrada. El filtrado
#' se basa en la selección de un rango específico de periodos (frecuencias)
#' de la transformada de wavelet.
#'
#' @param wt1 Un objeto de clase `dplR::wt` que contiene el resultado del análisis
#'   de wavelet (transformada de wavelet continua).
#' @param t1 Un data frame o matriz de dos columnas, donde la primera columna
#'   es el tiempo y la segunda es el valor de la serie original.
#' @param periodo_min Valor numérico que define el límite inferior del rango de
#'   periodos que se desea aislar para el filtrado.
#' @param periodo_max Valor numérico que define el límite superior del rango de
#'   periodos que se desea aislar para el filtrado.
#'
#' @return Un objeto de la clase `ggplot` que muestra la serie original y la
#'   serie filtrada para el rango de periodos especificado.
#'
#' @detales_de_argumentos
#' La función realiza los siguientes pasos:
#' 1.  **Selección de componentes de wavelet:** Identifica los coeficientes de
#'     wavelet cuyos periodos caen dentro del rango [`periodo_min`, `periodo_max`].
#' 2.  **Reconstrucción de la serie:** Suma los coeficientes de wavelet seleccionados
#'     para cada punto de tiempo. El valor absoluto de esta suma reconstruye
#'     la serie filtrada, aislando las oscilaciones de interés.
#' 3.  **Preparación de datos:** Combina la serie original y la filtrada en un
#'     solo data frame, ideal para la creación de gráficos con `ggplot2`.
#' 4.  **Generación del gráfico:** Crea un gráfico de líneas que compara visualmente
#'     ambas series, con un título dinámico que refleja el rango de periodos
#'     utilizado.
#'
#' @Ejemplo
#' # Para usar esta función, se requiere tener los objetos 'wt1' y 't1'
#' # generados previamente con un análisis de wavelet.
#' # Por ejemplo:
#' #
#' # # Ejemplo (asumiendo que 't1' es una serie de tiempo)
#' # wt1 <- dplR::wt(t1)
#' # graficar_serie_filtrada(wt1, t1, periodo_min = 16, periodo_max = 64)

graficar_serie_filtrada <- function(series1, series2, periodo_min, periodo_max,metodo_tendencia = c("mediana", "promedio"), mostrar_tendencia = TRUE) {
  # Calcular tendencia central antes de la interpolación para usar los datos originales
  if (metodo_tendencia == "mediana") {
    tendencia1 <- median(series1, na.rm = TRUE)
    tendencia2 <- median(series2, na.rm = TRUE)
  } else { # "promedio"
    tendencia1 <- mean(series1, na.rm = TRUE)
    tendencia2 <- mean(series2, na.rm = TRUE)
  }
  
  # Mostrar valor tendencia si se requiere
  if ( mostrar_tendencia) {
    message("Tendencia Series 1 (", metodo_tendencia, "): ", tendencia1)
    message("Tendencia Series 2 (", metodo_tendencia, "): ", tendencia2)
  }
  
  # Interpolación de NAs para mantener el paso de tiempo constante
  series1_interpolated <- na.approx(series1, na.rm = FALSE)
  series2_interpolated <- na.approx(series2, na.rm = FALSE)
  
  # Si la serie empieza o termina con NA, na.approx los deja.
  # Aquí los rellenamos con la tendencia calculada
  if (is.na(series1_interpolated[1])) {
    series1_interpolated[1] <- tendencia1
  }
  if (is.na(series2_interpolated[1])) {
    series2_interpolated[1] <- tendencia2
  }
  if (is.na(tail(series1_interpolated, 1))) {
    series1_interpolated[length(series1_interpolated)] <- tendencia1
  }
  if (is.na(tail(series2_interpolated, 1))) {
    series2_interpolated[length(series2_interpolated)] <- tendencia2
  }
  # Crear matrices con índices y datos interpolados
  t1 <- cbind(1:length(series1_interpolated), series1_interpolated)
  t2 <- cbind(1:length(series2_interpolated), series2_interpolated)
  
  # 1. Realizar la transformada wavelet de cada serie
  wt1 <- wt(t1)
  wt2 <- wt(t2)
  
  # Selecciona las escalas en el rango deseado
  indices <- which(wt1$period >= periodo_min & wt1$period <= periodo_max)
  
  # Extrae las componentes en esas escalas
  wave_subset <- wt1$wave[indices, , drop = FALSE]
  
  # Convierte a magnitud (valor absoluto)
  reconstruccion_mag <- Mod(colSums(wave_subset))
  
  # Crea el dataframe para ggplot
  df_recon <- data.frame(
    tiempo = t1[, 1],
    original = t1[, 2],
    filtrada = reconstruccion_mag
  )
  
  # Crea el título del gráfico de forma dinámica
  titulo_grafico <- paste("Serie original vs. filtrada (", periodo_min, "-", periodo_max, ")", sep = "")
  
  # Genera el gráfico
  ggplot(df_recon, aes(x = tiempo)) +
    geom_line(aes(y = original, color = "Original")) +
    geom_line(aes(y = filtrada, color = "Filtrada")) +
    labs(
      title = titulo_grafico,
      x = "Tiempo (minutos)",
      y = "Valor"
    ) +
    scale_color_manual(values = c("Original" = "blue", "Filtrada" = "red")) +
    theme_minimal()
}

#--------------------------------------------------------------------------------------------------------------


detectar_eventos <- function(base_datos, columna_tiempo = 1, columna_valor = 2, umbral_cuantil = 0.95) {
  #' Detecta picos o eventos en una serie de tiempo usando la derivada y un umbral de cuantil.
  #'
  #' @param datos Un data.frame o matriz que contiene la serie de tiempo.
  #' @param columna_tiempo El índice de la columna que contiene los valores de tiempo.
  #' @param columna_valor El índice de la columna que contiene los valores a analizar.
  #' @param umbral_cuantil El cuantil usado como umbral para detectar picos.
  #' @return Una lista con los picos detectados y el gráfico.
  
  
  # Extraer la serie de valores
  serie_valores <- base_datos[, columna_valor]
  
  # Calcular la derivada (diferencias) para encontrar cambios bruscos
  diff_serie <- diff(serie_valores)
  
  # Detectar los índices de los picos que superan el umbral del cuantil
  eventos <- which(abs(diff_serie) > quantile(abs(diff_serie), umbral_cuantil, na.rm = TRUE))
  
  eje_y <- colnames(PA)[columna_valor]
  # Crear el gráfico
  plot(base_datos[, columna_tiempo], serie_valores, type = 'l', main = 'Serie con eventos detectados',xlab = "Fecha", ylab = eje_y)
  points(base_datos[eventos, columna_tiempo], base_datos[eventos, columna_valor], col = 'red', pch = 19)
  
    # Devolver los índices de los picos
  return(eventos)
}

#-------------------------------------------------------------------------------------------------

































