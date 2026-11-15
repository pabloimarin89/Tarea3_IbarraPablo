
# Ejercicio 1: Alertas por ventana movil
# Recibe:
#   df: data frame con la columna fecha
#   ventana: cantidad de dias anteriores
#   k.alerta: multiplicador de desviacion estandar
# Devuelve:
#   data frame con fecha, n.tx, media, desv y alerta

alerta.movil <- function(df,ventana, k.alerta){
   # Paso 1: Contar las transacciones por fecha 
  
  conteo <- table(df$fecha)
  
  # Paso 2: Crear la secuencia completa de fechas
  
  fechas <- seq.Date(
    from = as.Date(min(names(conteo))),
    to = as.Date(max(names(conteo))),
    by = "day"
  )
  
  # Paso 3: Obtener el numero de transacciones diarias
  
  n.tx <- as.integer(conteo[as.character(fechas)])
  n.tx[is.na(n.tx)] <- 0L 
  
  # Paso 4: Inicializar los resultados
  media <- rep(NA_real_, length(fechas))
  desv <- rep(NA_real_, length(fechas))
  alerta <- rep(NA, length(fechas))
  
  # Paso 5: Evaluar cada dia posterior a la ventana
  
    if(length(fechas) > ventana) {
      for (t in (ventana +1): length(fechas)){
        # Seleccionar solo los dias anteriores
        anteriores <- n.tx[(t - ventana):(t - 1)]
        
        # Calcular media y desviacion estandar
        media[t] <- mean(anteriores)
        desv[t] <- sd(anteriores)
        
        # Determinar si existe alerta
        alerta[t] <- n.tx[t] >
          media[t] + k.alerta *desv[t]
      }
      
    }
  # Paso 6: Construir el data frame de salida
  resultado <- data.frame(
    fecha = fechas,
    n.tx = n.tx,
    media = media,
    desv = desv,
    alerta = alerta
  )
  return(resultado)
  
}

# ============================================================
# EJERCICIO 2 - LEY DE BENFORD
# ============================================================


# ------------------------------------------------------------
# Funcion: benford.mad
# Recibe: x, vector numerico de montos.
# Devuelve: MAD respecto a la distribucion de Benford.
# ------------------------------------------------------------

benford.mad <- function(x) {
  
  # 1. Conservar montos positivos y validos
  x <- x[!is.na(x) & is.finite(x) & x > 0]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  # 2. Extraer el primer digito de cada monto
  digito <- floor(x / (10 ^ floor(log10(x))))
  
  # 3. Frecuencia y proporcion observada de cada digito
  frecuencia <- tabulate(digito, nbins = 9)
  p.observada <- frecuencia / sum(frecuencia)
  
  # 4. Proporcion teorica de Benford
  d <- 1:9
  p.benford <- log10(1 + 1 / d)
  
  # 5. Calcular la desviacion absoluta media
  mad <- mean(abs(p.observada - p.benford))
  
  return(mad)
}


# ------------------------------------------------------------
# Funcion: benford.canton
# Recibe:
#   df: base de transacciones
#   canal: canal que se desea analizar
#   cantones: catalogo de codigos y nombres de cantones
# Devuelve:
#   Data frame con cod_canton, canton, n, mad y clase,
#   ordenado de mayor a menor MAD.
# ------------------------------------------------------------

benford.canton <- function(df, canal, cantones) {
  
  resultado <- df |>
    dplyr::filter(
      .data$canal == .env$canal,
      !is.na(.data$monto_crc),
      is.finite(.data$monto_crc),
      .data$monto_crc > 0
    ) |>
    dplyr::group_by(cod_canton) |>
    dplyr::summarise(
      n = dplyr::n(),
      mad = benford.mad(monto_crc),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      cantones |> dplyr::select(cod_canton, canton),
      by = "cod_canton"
    ) |>
    dplyr::mutate(
      clase = dplyr::case_when(
        mad <= 0.006 ~ "Conformidad cercana",
        mad <= 0.012 ~ "Conformidad aceptable",
        mad <= 0.015 ~ "Conformidad marginal",
        mad > 0.015 ~ "No conformidad"
      )
    ) |>
    dplyr::select(cod_canton, canton, n, mad, clase) |>
    dplyr::arrange(dplyr::desc(mad))
  
  return(resultado)
}


# ============================================================
# EJERCICIO 3 - VALOR ACUMULADO CON INTERES
# ============================================================

# Funcion: valor.acumulado
#
# Recibe:
# df: base de transacciones
# tasa.anual: tasa efectiva anual
# canal: canal de transacciones seleccionado
#
# Devuelve:
# Lista con tabla, tasa.mensual, va e interes

valor.acumulado <- function(df, tasa.anual, canal) {
  
  # 1. Seleccionar las transacciones del canal
  datos.canal <- df |>
    dplyr::filter(.data$canal == .env$canal)
  
  # 2. Extraer el mes de cada transaccion
  datos.canal <- datos.canal |>
    dplyr::mutate(
      mes = as.integer(format(as.Date(fecha), "%m"))
    )
  
  # 3. Sumar los montos de cada mes
  resumen.mensual <- datos.canal |>
    dplyr::group_by(mes) |>
    dplyr::summarise(
      monto = sum(monto_crc, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 4. Crear una tabla con los 12 meses
  tabla <- data.frame(mes = 1:12) |>
    dplyr::left_join(resumen.mensual, by = "mes") |>
    dplyr::mutate(
      monto = dplyr::coalesce(monto, 0)
    )
  
  # 5. Calcular la tasa efectiva mensual
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  
  # 6. Calcular el factor de acumulacion
  tabla <- tabla |>
    dplyr::mutate(
      factor = (1 + tasa.mensual)^(12 - mes)
    )
  
  # 7. Calcular el valor acumulado
  va <- sum(tabla$monto * tabla$factor)
  
  # 8. Calcular los intereses ganados
  interes <- va - sum(tabla$monto)
  
  # 9. Devolver los resultados
  return(
    list(
      tabla = tabla,
      tasa.mensual = tasa.mensual,
      va = va,
      interes = interes
    )
  )
}


