

# Prueba 1: que la función funciona con datos válidos
test_that("función funciona con datos válidos", {
  set.seed(123)
  serie1 <- sin(seq(0, 2 * pi, length.out = 100))
  serie2 <- cos(seq(0, 2 * pi, length.out = 100))
  
  expect_no_error(
    coherencia_wavelet(serie1, serie2, metodo_tendencia = "mediana")
  )
})

# Prueba 2: que lanza error si las series no tienen la misma longitud
test_that("Error si las series tienen diferente longitud", {
  serie1 <- 1:50
  serie2 <- 1:60
  
  expect_error(
    coherencia_wavelet(serie1, serie2),
    "Las series deben tener la misma longitud."
  )
})

# Prueba 3: que la tendencia central se calcula correctamente
test_that("Calculo correcto de tendencia (mediana y promedio)", {
  # Usamos series largas y controladas para que la mediana y el promedio sean exactos
  serie1 <- c(1:50, NA, 52:100)
  serie2 <- c(1:50, NA, 52:100) * 2
  
  # Mediana: el valor real de la mediana de serie1 con na.rm=TRUE es 50
  expect_message(
    coherencia_wavelet(serie1, serie2, metodo_tendencia = "mediana", verbose = TRUE),
    regexp = "Tendencia Series 1 \\(mediana\\): 50"
  )
  
  # Promedio: el valor real del promedio de serie2 con na.rm=TRUE
  expect_message(
    coherencia_wavelet(serie1, serie2, metodo_tendencia = "promedio", verbose = TRUE),
    regexp = "Tendencia Series 2 \\(promedio\\): 100.989898989899"
  )
})

# Prueba 4: que no haya errores con NA si se calculan medianas o medias
test_that("Manejo de NA en tendencia", {
  # Usamos series más largas y con variabilidad para que wtc funcione
  serie1 <- 1:100
  serie1[50] <- NA
  serie1[60] <- NA
  serie2 <- 101:200
  serie2[50] <- NA
  serie2[60] <- NA
  
  expect_no_error(
    coherencia_wavelet(serie1, serie2, metodo_tendencia = "mediana")
  )
  expect_no_error(
    coherencia_wavelet(serie1, serie2, metodo_tendencia = "promedio")
  )
})