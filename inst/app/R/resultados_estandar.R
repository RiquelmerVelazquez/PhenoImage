# Contrato de resultados E15. Los índices y sus estadísticas no se recalculan aquí.
nombre_archivo_cipat <- function(producto, lote, fecha, sensor, extension) {
  limpiar <- function(x) gsub("[^A-Za-z0-9_.-]", "_", as.character(x))
  paste0(paste(limpiar(c(as.character(producto), as.character(lote), as.character(fecha), as.character(sensor))), collapse = "_"), ".", extension)
}
archivo_salida_cipat <- function(p, producto, extension) {
  sensor <- if (identical(p$tipo, "phantom")) "RGB" else "Sentera"
  file.path(p$salida, nombre_archivo_cipat(producto, p$lote, p$fecha, sensor, extension))
}
crear_carpeta_resultados_cipat <- function(base, lote, fecha, sensor) {
  seguro <- gsub("[^A-Za-z0-9_.-]", "_", as.character(lote))
  nombre <- paste(seguro, as.character(fecha), sensor, sep = "_")
  ruta <- file.path(base, nombre)
  version <- 2L
  while (file.exists(ruta) || dir.exists(ruta)) {
    ruta <- file.path(base, paste0(nombre, "_v", sprintf("%02d", version)))
    version <- version + 1L
  }
  if (!dir.create(ruta, recursive = TRUE)) stop("No se pudo crear la carpeta de resultados.")
  normalizePath(ruta, winslash = "/", mustWork = TRUE)
}

normalizar_resultados_cipat <- function(d, sensor = "integrado") {
  n <- names(d)
  # Eliminar solo los campos expresamente retirados, también al importar bases antiguas.
  retirar <- grepl("^Canopy_volume_apparent", n) | startsWith(n, "Plant_ground_cover_image_CHM_") |
    grepl("^Plant_ground_cover_image_(NDVI|RGB)_(total_pix|veg_pix|total_area_m2|soil_area_m2)$", n)
  d <- d[, !retirar, drop = FALSE]
  n <- names(d)
  if (sensor %in% c("sentera", "Sentera")) names(d) <- sub("^(Red|Green|Blue|RE|NIR)_", "Sentera_\\1_", n)
  if (sensor %in% c("phantom", "RGB")) names(d) <- sub("^(Red|Green|Blue)_", "RGB_\\1_", n)
  if (anyDuplicated(names(d))) stop("Hay columnas duplicadas tras identificar las bandas por sensor.")
  n <- names(d)
  identidad <- intersect(c("Lote", "Fecha", "Plot", "id", "Original_ID", "Rep", "Row", "Column",
    "Parcela_Original_ID", "Parcela_Original_I", "Parcela_Rep", "Parcela_Row", "Parcela_Column",
    "N_line", "Cod_local", "Origen_datos"), n)
  bandas_s <- grep("^Sentera_(Red|Green|Blue|RE|NIR)_", n, value = TRUE)
  bandas_r <- grep("^RGB_(Red|Green|Blue)_", n, value = TRUE)
  altura <- grep("^Canopy_height_image_", n, value = TRUE)
  cov_n <- paste0("Plant_ground_cover_image_NDVI_", c("pct", "veg_area_m2", "polygon_area_m2", "valid_area_pct"))
  cov_r <- paste0("Plant_ground_cover_image_RGB_", c("pct", "veg_area_m2", "polygon_area_m2", "valid_area_pct"))
  pref_color <- c("HSB_Hue", "HSB_Saturation", "HSB_Brightness", "DGCI", "Lab_L", "Lab_a", "Lab_b")
  color <- unlist(lapply(pref_color, function(p) grep(paste0("^", p, "_"), n, value = TRUE)), use.names = FALSE)
  textura <- unlist(lapply(c("RGB_Entropy", "RGB_Contrast", "RGB_Homogeneity"),
    function(p) grep(paste0("^", p, "(_|$)"), n, value = TRUE)), use.names = FALSE)
  # Métricas históricas se conservan con su nombre: nunca se etiqueta Hue angular como HSV.
  color_antiguo <- grep("^(Hue|Saturation|Value)_", n, value = TRUE)
  resto <- setdiff(n, c(identidad, bandas_s, bandas_r, altura, cov_n, cov_r, color, color_antiguo, textura))
  attrs <- resto[startsWith(resto, "Parcela_")]
  metricas <- setdiff(resto, attrs)
  rgb_indices <- c("ExG", "NGRDI", "VARI", "GLI", "RGBVI", "GCC", "NGBDI", "TGI", "BCC", "RCC", "BGI", "BRVI", "GR", "GB", "ERVI", "MVARI", "VEG")
  ind_r <- metricas[sub("_(sum|mean|median|sd|cv|min|max|p[0-9]+)$", "", metricas) %in% rgb_indices]
  ind_s <- setdiff(metricas, ind_r)
  orden <- unique(c(identidad, attrs, bandas_s, ind_s, bandas_r, ind_r, altura, cov_n, cov_r,
                    color, color_antiguo, textura))
  d[, intersect(orden, names(d)), drop = FALSE]
}

# Conversión delegada a pliman; no se reemplaza su gamma, matriz o tratamiento de Hue.
# Los argumentos opcionales permiten contrastar con el código oficial en las pruebas.
color_hsb_lab_cipat <- function(v, hsb_fun = pliman::rgb_to_hsb,
                              lab_fun = pliman::rgb_to_lab) {
  if (is.null(dim(v))) v <- matrix(v, nrow = 1L)
  if (ncol(v) != 3L) stop("La conversión de color requiere tres canales RGB.")
  salida <- matrix(NA_real_, nrow(v), 7L)
  colnames(salida) <- c("HSB_Hue", "HSB_Saturation", "HSB_Brightness", "DGCI", "Lab_L", "Lab_a", "Lab_b")
  ok <- rowSums(!is.finite(v)) == 0 & rowSums(v < 0 | v > 1, na.rm = TRUE) == 0
  if (!any(ok)) return(salida)
  z <- v[ok, , drop = FALSE]
  # pliman 3.1.0 pierde la dimensión en rgb_to_srgb con una sola fila.
  # Duplicarla evita ese error sin modificar el color ni añadir observaciones.
  entrada <- if (nrow(z) == 1L) z[c(1L, 1L), , drop = FALSE] else z
  hsb <- as.matrix(hsb_fun(entrada))[seq_len(nrow(z)), , drop = FALSE]
  lab <- as.matrix(lab_fun(entrada))[seq_len(nrow(z)), , drop = FALSE]
  H <- hsb[, 1]; S <- hsb[, 2]; V <- hsb[, 3]
  dgci <- ((H - 60) / 60 + (1 - S / 100) + (1 - V / 100)) / 3
  salida[ok, ] <- cbind(H, S, V, dgci, lab)
  # NaN/Inf del paquete se representan como NoData; no se inventan colores.
  salida[!is.finite(salida)] <- NA_real_
  salida
}

# El CV no es interpretable para un ángulo ni para las coordenadas de intervalo Lab.
# Se conserva la columna para no romper el esquema; no altera índices ni bandas.
ajustar_estadisticas_color_cipat <- function(df, nombre) {
  if (nombre %in% c("HSB_Hue", "Lab_L", "Lab_a", "Lab_b") && "cv" %in% names(df)) {
    df$cv <- NA_real_
  }
  df
}

documentar_rasgos_color_cipat <- function(wb, p) {
  version <- as.character(utils::packageVersion("pliman"))
  d <- data.frame(Rasgo = c("HSB_Hue", "HSB_Saturation", "HSB_Brightness", "DGCI", "Lab_L", "Lab_a", "Lab_b", "GLCM"),
    Metodo = c("pliman::rgb_to_hsb(RGB/Escala_RGB): columna h; grados, sin envolver o corregir el resultado del paquete",
      "pliman::rgb_to_hsb: columna s; escala 0-100%", "pliman::rgb_to_hsb: columna b; escala 0-100%",
      "((H-60)/60 + (1-S/100) + (1-B/100))/3, usando las tres salidas de pliman",
      "pliman::rgb_to_lab: columna L; incluye rgb_to_srgb del propio paquete",
      "pliman::rgb_to_lab: columna a; verde negativo, rojo positivo",
      "pliman::rgb_to_lab: columna b; azul negativo, amarillo positivo",
      "Verde con máscara ExG; 32 niveles; ventana configurada; promedio de 4 direcciones a distancia 1"),
    Nota = c("CV=NA: magnitud angular; se conservan los demás estadísticos lineales solicitados, con cautela en discontinuidades del tono",
      "Los valores no finitos del paquete (p. ej. negro) se exportan como NoData", "",
      "No se recorta; uso previsto en vegetación verde; no equivale a concentración de clorofila",
      "CV=NA: coordenada de intervalo; la transformación es la del paquete, sin conversión alternativa",
      "CV=NA: coordenada con signo y cero convencional; no se usa abs(media)",
      "CV=NA: coordenada con signo y cero convencional; no se usa abs(media)",
      "na_opt=any: ventanas con NA se excluyen; rango de cuantización observado, registrado en Parametros"),
    Implementacion = c(rep(paste0("pliman ", version), 7), "GLCM: implementación conservada"),
    Referencia = c(rep("https://nepem-ufsc.github.io/pliman/reference/utils_colorspace.html", 3),
      "https://nepem-ufsc.github.io/pliman/articles/indexes.html",
      rep("https://nepem-ufsc.github.io/pliman/reference/utils_colorspace.html", 3),
      "Método independiente de la conversión de color de pliman"))
  openxlsx::addWorksheet(wb, "Rasgos_color_textura")
  openxlsx::writeData(wb, "Rasgos_color_textura", d)
  openxlsx::setColWidths(wb, "Rasgos_color_textura", 1:5, c(24, 85, 85, 24, 85))
}

# Muestreo auxiliar exclusivo para Otsu: conserva el ámbito histórico del umbral.
# La ventana temporal nunca se usa como entrada para guardar los productos.
muestra_otsu_cipat <- function(r, poligonos, margen = 0) {
  ventana <- if (inherits(poligonos, "SpatVector")) poligonos else terra::vect(poligonos)
  if (margen > 0) ventana <- terra::buffer(ventana, width = margen)
  muestra <- terra::crop(r, ventana, snap = "out")
  datos <- terra::spatSample(muestra, size = min(200000, terra::ncell(muestra)), method = "random", na.rm = TRUE,
                    values = TRUE, as.df = TRUE)
  attr(datos, "rango_muestreo") <- c(terra::global(muestra, "min", na.rm = TRUE)[1,1],
                                     terra::global(muestra, "max", na.rm = TRUE)[1,1])
  datos
}

# Evaluación vectorizada explícita: una conversión por bloque, nunca una por píxel.
calcular_color_archivo_cipat <- function(rgb, escala, filename, registrar = NULL, max_celdas = 200000L) {
  if (terra::nlyr(rgb) != 3L) stop("El cálculo de color requiere tres canales RGB.")
  if (length(escala) != 1L || !is.finite(escala) || escala <= 0) stop("Escala RGB inválida.")
  nombres <- c("HSB_Hue", "HSB_Saturation", "HSB_Brightness", "DGCI", "Lab_L", "Lab_a", "Lab_b")
  convertir <- function(v) color_hsb_lab_cipat(v / escala)
  avance <- if (is.null(registrar)) NULL else function(pct) registrar(paste0("Color HSB/DGCI/Lab: ", pct, "%"))
  escribir_bloques_sentera(rgb, filename, convertir, nbandas = 7L, nombres = nombres,
    max_celdas = max_celdas, progreso = avance)
}
