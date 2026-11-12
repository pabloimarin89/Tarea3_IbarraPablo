
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
