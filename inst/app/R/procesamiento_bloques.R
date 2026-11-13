# Lectura y escritura por franjas: el tamaño depende del ancho, no del total de píxeles.
escribir_bloques_sentera <- function(entrada, filename, transformar, nbandas = 1L,
                                    nombres = NULL, max_celdas = 200000L, progreso = NULL) {
  if (file.exists(filename)) stop("Ya existe el archivo de salida: ", filename)
  if (!is.numeric(max_celdas) || length(max_celdas) != 1L || !is.finite(max_celdas) || max_celdas < 1) stop("Tamaño de bloque inválido.")
  ancho <- ncol(entrada); alto <- nrow(entrada)
  filas <- max(1L, as.integer(floor(max_celdas / ancho)))
  salida <- terra::rast(entrada[[1]], nlyrs = nbandas)
  if (!is.null(nombres)) names(salida) <- nombres
  leyendo <- FALSE; escribiendo <- FALSE; completo <- FALSE
  on.exit({
    if (leyendo) try(terra::readStop(entrada), silent = TRUE)
    if (escribiendo) try(terra::writeStop(salida), silent = TRUE)
    if (!completo && file.exists(filename)) unlink(filename)
  }, add = TRUE)
  terra::readStart(entrada); leyendo <- TRUE
  terra::writeStart(salida, filename = filename, overwrite = FALSE, datatype = "FLT4S",
    gdal = c("COMPRESS=LZW", "PREDICTOR=3", "BIGTIFF=IF_SAFER"), progress = 0)
  escribiendo <- TRUE
  ultimo_progreso <- -10L
  for (inicio in seq.int(1L, alto, by = filas)) {
    cantidad <- min(filas, alto - inicio + 1L)
    valores <- terra::readValues(entrada, row = inicio, nrows = cantidad, mat = TRUE)
    resultado <- transformar(valores)
    if (length(resultado) != cantidad * ancho * nbandas) stop("El bloque de salida no conserva sus dimensiones.")
    terra::writeValues(salida, resultado, start = inicio, nrows = cantidad)
    rm(valores, resultado)
    porcentaje <- as.integer(100 * (inicio + cantidad - 1L) / alto)
    if (!is.null(progreso) && (porcentaje >= ultimo_progreso + 10L || porcentaje == 100L)) {
      progreso(porcentaje)
      ultimo_progreso <- porcentaje
    }
  }
  terra::readStop(entrada); leyendo <- FALSE
  terra::writeStop(salida); escribiendo <- FALSE
  completo <- TRUE
  terra::rast(filename)
}

# Misma gramática validada del editor, evaluada sobre vectores de un bloque.
formula_bloque_sentera <- function(nombre, txt, factor) {
  arbol <- arbol_formula_cipat(txt, c("R", "G", "B", "RE", "NIR"))
  force(nombre); force(factor); force(arbol)
  function(v) {
    bandas <- list(R = v[, 1], G = v[, 2], B = v[, 3],
      RE = factor * v[, 4], NIR = factor * v[, 5])
    resolver <- function(x) {
      if (is.numeric(x)) return(x)
      if (is.symbol(x)) return(bandas[[as.character(x)]])
      op <- as.character(x[[1]]); a <- resolver(x[[2]])
      if (length(x) == 2L) return(switch(op, `+` = a, `-` = -a, `(` = a,
        sqrt = sqrt(a), abs = abs(a), log = log(a), exp = exp(a)))
      b <- resolver(x[[3]])
      switch(op, `+` = a + b, `-` = a - b, `*` = a * b, `^` = a ^ b,
        `/` = { r <- a / b; r[rep_len(!is.na(b) & abs(b) < 1e-8, length(r))] <- NA_real_; r })
    }
    r <- suppressWarnings(resolver(arbol))
    r <- rep_len(r, nrow(v))
    # La función original convierte infinitos/NaN en NA antes de sus filtros.
    r[!is.finite(r)] <- NA_real_
    if (nombre %in% c("NDVI", "NDRE", "GNDVI")) r <- pmin(1, pmax(-1, r))
    if (nombre == "CIRE") r <- pmin(3, pmax(0, r))
    if (nombre == "CVI") {
      excluir <- bandas$G <= 1 | r < 0 | r > 100
      r[is.na(excluir) | excluir] <- NA_real_
    }
    # Una fórmula constante también conserva las celdas sin soporte en la banda 1.
    if (!length(intersect(all.vars(arbol), names(bandas)))) r[!is.finite(bandas$R)] <- NA_real_
    r
  }
}

calcular_indice_archivo_sentera <- function(nombre, bandas, factor, formulas, filename,
                                             max_celdas = 200000L, limpiar_entrada = FALSE) {
  evaluar <- formula_bloque_sentera(nombre, formulas[[nombre]], factor)
  transformar <- function(v) {
    if (limpiar_entrada) {
      v[v == -10000 & !is.na(v)] <- NA_real_
      valido <- rowSums(!is.finite(v)) == 0L
      v[!valido, ] <- NA_real_
    }
    resultado <- evaluar(v)
    if (limpiar_entrada) resultado[!valido] <- NA_real_
    resultado
  }
  escribir_bloques_sentera(bandas, filename, transformar, nombres = nombre, max_celdas = max_celdas)
}
