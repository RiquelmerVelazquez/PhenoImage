# PhenoImage | CIPAT · Grupo Papalotla 4.8.0
# Plataforma de Caracterización Fenotípica | Aplicación local Shiny
# Instalación y arranque: consulta LEEME.txt en la raíz del paquete.
# Abre INICIAR_PHENOIMAGE.R con Source en RStudio.

message("PhenoImage | 4.8.0 | Portada de resultados")
message("Carpeta de la app: ", normalizePath(getwd(), winslash = "/", mustWork = TRUE))

paquetes <- c("shiny", "terra", "sf", "exactextractr", "openxlsx",
              "autothresholdr", "DT", "future", "promises", "leaflet", "htmlwidgets", "zip", "pliman")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) stop("Instala primero: ", paste(faltan, collapse = ", "))
if (utils::packageVersion("shiny") < "1.8.1") stop("Actualiza shiny a 1.8.1 o posterior.")
if (utils::packageVersion("leaflet") < "2.2.2") stop("Actualiza leaflet: install.packages(\"leaflet\")")
if (utils::packageVersion("pliman") < "3.1.0") stop("Actualiza pliman: ejecuta INSTALAR_COLOR_PLIMAN.R")
library(shiny)
# I(1) fuerza un proceso separado incluso con un solo trabajador.
future::plan(future::multisession, workers = I(1))

normalizar_ruta <- function(x) {
  x <- trimws(x)
  x <- sub('^"', "", x)
  x <- sub('"$', "", x)
  gsub("\\", "/", x, fixed = TRUE)
}

division_segura <- function(a, b) terra::ifel(abs(b) < 1e-8, NA, a / b)

# Mantiene las correcciones, factor inter-cámara y filtros del script recibido.
source(file.path("R", "informacion_ortomosaico.R"), local = TRUE)
source(file.path("R", "ejemplos.R"), local = TRUE)
source(file.path("R", "indices_editables.R"), local = TRUE)
source(file.path("R", "seleccion_indices_rgb.R"), local = TRUE)
source(file.path("R", "catalogo_indices.R"), local = TRUE)
source(file.path("R", "sentera_multibanda.R"), local = TRUE)
source(file.path("R", "procesamiento_bloques.R"), local = TRUE)
source(file.path("R", "resultados_estandar.R"), local = TRUE)
source(file.path("R", "guardado_sentera.R"), local = TRUE)
source(file.path("R", "segmentacion_muestras.R"), local = TRUE)

calcular_indice <- function(nombre, b, factor, formulas) {
  bandas <- list(R = b[[1]], G = b[[2]], B = b[[3]],
                 RE = factor * b[[4]], NIR = factor * b[[5]])
  r <- ejecutar_formula_cipat(formulas[[nombre]], bandas)
  # Filtros históricos explícitos, conservados para estos nombres.
  if (nombre %in% c("NDVI", "NDRE", "GNDVI")) r <- terra::clamp(r, -1, 1)
  if (nombre == "CIRE") r <- terra::clamp(r, 0, 3)
  if (nombre == "CVI") r <- terra::ifel(bandas$G <= 1 | r < 0 | r > 100, NA, r)
  names(r) <- nombre
  r
}

# Estadísticas como en el script original: sin ponderación en los bordes.
# La cobertura y las áreas sí usan la fracción real de cada píxel en la parcela.
resumen_raster <- function(r, poligonos) {
  nombres <- c("sum", "mean", "median", "sd", "cv", "min", "max", "p10", "p25", "p75", "p90")
  f <- function(values, coverage_fraction) {
    v <- if (is.matrix(values) || is.data.frame(values)) values[, 1] else values
    v <- as.numeric(v)
    v <- v[is.finite(v) & is.finite(coverage_fraction) & coverage_fraction > 0]
    if (!length(v)) return(as.data.frame(as.list(stats::setNames(rep(NA_real_, 11), nombres))))
    media <- mean(v); desv <- stats::sd(v)
    q <- stats::quantile(v, c(.10, .25, .75, .90), names = FALSE)
    as.data.frame(as.list(stats::setNames(c(sum(v), media, stats::median(v), desv,
      if (abs(media) < 1e-8) NA_real_ else desv / media, min(v), max(v), q), nombres)))
  }
  # Extraer una parcela por vez limita el conjunto de píxeles en memoria.
  do.call(rbind, lapply(seq_len(nrow(poligonos)), function(j) {
    exactextractr::exact_extract(r, poligonos[j, ], fun = f, progress = FALSE)
  }))
}

resumen_cobertura <- function(mask, poligonos) {
  f <- function(values, coverage_fraction) {
    v <- if (is.matrix(values) || is.data.frame(values)) values[, 1] else values
    ok <- is.finite(v) & is.finite(coverage_fraction) & coverage_fraction > 0
    v <- v[ok]; w <- coverage_fraction[ok]
    total <- sum(w); verde <- sum(w[v == 1])
    data.frame(total_pix = total, veg_pix = verde,
               pct = if (total > 0) 100 * verde / total else NA_real_)
  }
  do.call(rbind, lapply(seq_len(nrow(poligonos)), function(j) {
    exactextractr::exact_extract(mask, poligonos[j, ], fun = f, progress = FALSE)
  }))
}

procesar_htp <- function(p) {
  p <- validar_entrada_sentera(p)
  es_multibanda <- identical(p$origen_sentera, "multibanda")
  dir.create(p$salida, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(p$salida)) stop("No se pudo crear la carpeta de resultados.")
  temporales <- tempfile("cipat_sentera_")
  dir.create(temporales)
  on.exit(unlink(temporales, recursive = TRUE, force = TRUE), add = TRUE)
  opciones_terra <- terra::terraOptions(print = FALSE)
  on.exit(do.call(terra::terraOptions, opciones_terra[intersect(names(opciones_terra),
    c("tempdir", "memfrac", "memmax", "memmin", "todisk", "progress"))]), add = TRUE)
  terra::terraOptions(tempdir = temporales, memfrac = .1,
    memmax = .5, memmin = 0, todisk = TRUE, progress = 0)
  registrar <- function(txt) {
    cat(format(Sys.time(), "%H:%M:%S"), " | ", txt, "\n", file = p$log, append = TRUE, sep = "")
  }
  guardar <- function(r, nombre) {
    ruta <- file.path(p$salida, nombre_tif_sentera(nombre, p$lote, p$fecha))
    terra::writeRaster(r, ruta, overwrite = FALSE, datatype = "FLT4S")
    terra::rast(ruta)
  }
  asegurar_temporal_excel_sentera()
  registrar("Leyendo archivos y comprobando bandas · cálculo por bloques de hasta 200000 celdas")
  rgb <- terra::rast(p$rgb)
  ms <- if (!es_multibanda) terra::rast(p$ms) else NULL
  if (es_multibanda && terra::nlyr(rgb) != 5L) stop("Multibanda Sentera requiere cinco bandas: rojo, verde, azul, red edge y NIR.")
  if (!es_multibanda && (terra::nlyr(rgb) < 3 || terra::nlyr(ms) < 3)) stop("Ambos TIFF deben tener al menos 3 bandas.")
  if (!nzchar(terra::crs(rgb)) || (!es_multibanda && !nzchar(terra::crs(ms)))) stop("Los TIFF necesitan un CRS definido.")
  crs_rgb <- sf::st_crs(terra::crs(rgb))
  if (isTRUE(sf::st_is_longlat(crs_rgb)) || !isTRUE(crs_rgb$units_gdal %in% c("metre", "meter", "m"))) {
    stop("Esta versión necesita el RGB en un CRS proyectado en metros, por ejemplo UTM.")
  }
  polys <- sf::st_read(p$polys, quiet = TRUE)
  if (!nrow(polys)) stop("El archivo de parcelas está vacío.")
  if (is.na(sf::st_crs(polys))) stop("Los polígonos necesitan un CRS definido.")
  polys <- sf::st_transform(sf::st_make_valid(polys), crs_rgb)
  if (!all(as.character(sf::st_geometry_type(polys)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("El archivo debe contener únicamente polígonos de parcelas.")
  }
  if (any(sf::st_is_empty(polys))) stop("Hay parcelas con geometría vacía.")
  if (!p$id %in% names(polys)) stop("No existe el campo ID indicado. Campos: ", paste(names(polys), collapse = ", "))
  ids <- as.character(polys[[p$id]])
  if (anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids)) stop("Los ID deben ser únicos y no estar vacíos.")
  # Conservar extensión, resolución y origen de la rejilla del ortomosaico recibido.
  # Los polígonos no recortan los productos de salida.
  if (es_multibanda) {
    registrar("Multibanda Sentera: lectura directa; sin matrices ni factor inter-cámara adicional")
    if (any(terra::datatype(rgb) == "INT1U")) stop("Carga las bandas numéricas corregidas; no un mosaico de 8 bits.")
    # Vista de las bandas originales: no se escribe una copia multibanda.
    mapa <- mapa_bandas_cipat(p, "multibanda", terra::nlyr(rgb))
    bandas <- rgb[[unname(mapa)]]
  } else {
    rgb <- rgb[[unname(mapa_bandas_cipat(p, "rgb", terra::nlyr(rgb)))]]
    canales_ms <- mapa_bandas_cipat(p, "ms", terra::nlyr(ms))
    registrar("Corrección espectral y alineación a la rejilla RGB")
    Red <- -0.034 * rgb[[3]] - 0.110 * rgb[[2]] + 1.150 * rgb[[1]]
    Green <- -0.199 * rgb[[3]] + 1.420 * rgb[[2]] - 0.329 * rgb[[1]]
    Blue <- 1.377 * rgb[[3]] - 0.182 * rgb[[2]] - 0.061 * rgb[[1]]
    alinear <- function(r) {
      if (terra::same.crs(r, Red)) terra::resample(r, Red, method = "bilinear")
      else terra::project(r, Red, method = "bilinear")
    }
    # Convención original: TIFF multiespectral, banda 1 = RE y banda 3 = NIR.
    RE <- alinear(-0.956 * ms[[canales_ms[["NIR"]]]] + ms[[canales_ms[["RE"]]]])
    NIR <- alinear(2.426 * ms[[canales_ms[["NIR"]]]] - 0.341 * ms[[canales_ms[["RE"]]]])
    bandas <- c(Red, Green, Blue, RE, NIR)
  }
  names(bandas) <- c("Red", "Green", "Blue", "RE", "NIR")
  if (!es_multibanda) bandas <- guardar(bandas, "Multibanda_corregida")
  rm(rgb, ms)
  invisible(gc())
  saveRDS(p, archivo_salida_cipat(p, "Entrada", "rds"))
  elegidos <- unique(c("NDVI", p$indices))
  rutas <- list()
  for (i in elegidos) {
    registrar(paste("Calculando", i))
    r <- calcular_indice_archivo_sentera(i, bandas, p$factor, p$formulas,
      filename = file.path(p$salida, nombre_tif_sentera(i, p$lote, p$fecha)),
      limpiar_entrada = es_multibanda)
    rutas[[i]] <- terra::sources(r)[1]
    rm(r)
    invisible(gc())
  }
  ndvi <- terra::rast(rutas$NDVI)
  registrar("Construyendo máscara NDVI")
  muestra <- muestra_otsu_cipat(ndvi, polys)
  vals <- as.numeric(muestra[[1]])
  vals <- vals[is.finite(vals)]
  if (!length(vals)) stop("No hay valores NDVI válidos dentro de la extensión de las parcelas.")
  if (p$metodo == "Otsu") {
    if (diff(range(vals)) < 1e-8) stop("NDVI constante: utiliza un umbral manual.")
    u <- autothresholdr::auto_thresh(as.integer(pmax(0, pmin(255, (vals + 1) / 2 * 255))), "Otsu")
    umbral <- (as.numeric(u) / 255) * 2 - 1
  } else umbral <- p$umbral
  registrar(paste("Umbral NDVI:", round(umbral, 4)))
  mask <- guardar(terra::ifel(ndvi >= umbral, 1, 0), "NDVI_Mascara_Binaria")
  guardar(terra::ifel(mask == 1, ndvi, NA), "NDVI_Mascara_Vegetacion")
  tabla <- data.frame(id = ids, Lote = p$lote, Fecha = as.character(p$fecha), stringsAsFactors = FALSE)
  for (b in names(bandas)) {
    registrar(paste("Extrayendo banda", b))
    df <- resumen_raster(terra::ifel(mask == 1, bandas[[b]], NA), polys)
    names(df) <- paste(b, names(df), sep = "_")
    tabla <- cbind(tabla, df)
  }
  indices_estadisticas <- if (es_multibanda) unique(p$indices) else elegidos
  for (i in indices_estadisticas) {
    registrar(paste("Extrayendo índice", i))
    df <- resumen_raster(terra::ifel(mask == 1, terra::rast(rutas[[i]]), NA), polys)
    names(df) <- paste(i, names(df), sep = "_")
    tabla <- cbind(tabla, df)
  }
  registrar("Calculando cobertura y áreas por parcela")
  cov <- resumen_cobertura(mask, polys)
  area <- prod(terra::res(mask))
  cov$total_area_m2 <- cov$total_pix * area
  cov$veg_area_m2 <- cov$veg_pix * area
  cov$soil_area_m2 <- (cov$total_pix - cov$veg_pix) * area
  cov$polygon_area_m2 <- as.numeric(sf::st_area(polys))
  cov$valid_area_pct <- 100 * cov$total_area_m2 / cov$polygon_area_m2
  cov <- cov[c("pct", "veg_area_m2", "polygon_area_m2", "valid_area_pct")]
  names(cov) <- paste0("Plant_ground_cover_image_NDVI_", names(cov))
  tabla <- cbind(tabla, cov)
  metadatos <- data.frame(Parametro = c("Lote", "Fecha", "RGB", "Multiespectral", "Parcelas", "Campo_ID",
      "Factor", "Metodo", "Umbral", "Muestreo_Otsu", "Bandas_multiespectral", "Estadisticas", "Cobertura"),
    Valor = c(p$lote, as.character(p$fecha), p$rgb, p$ms, p$polys, p$id, as.character(p$factor), p$metodo,
      as.character(umbral), "Aleatorio, hasta 200000 píxeles; extensión de las parcelas",
      if (es_multibanda) texto_bandas_cipat(mapa) else texto_bandas_cipat(canales_ms), "Sin ponderación, como script original; sd muestral; cv fracción",
      "Ponderada por fracción de píxel; denominador: área con NDVI válido"))
  metadatos <- rbind(metadatos, data.frame(
    Parametro = c("Origen_Sentera", "Correccion_matrices_en_Shiny", "Correccion_ISO_en_Shiny", "Escala_radiometrica"),
    Valor = c(p$origen_sentera, if (es_multibanda) "Omitida: entrada previamente corregida" else "Matrices originales",
      "No aplicada en esta aplicación", if (es_multibanda) "Unidades originales; reflectancia absoluta no validada" else "Unidades del flujo anterior")))
  metadatos <- rbind(metadatos, data.frame(
    Parametro = c("Indices_estadisticas", "NDVI_para_mascara", "Indices_con_constantes"),
    Valor = c(paste(indices_estadisticas, collapse = "; "), "Siempre calculado, aunque su casilla no esté marcada",
      "OSAVI/SAVI/EVI/MSAVI2: exploratorios hasta validar escala de reflectancia; no se reescalan automáticamente")))
  metadatos <- rbind(metadatos, data.frame(Parametro = c("Bandas_RGB","Modo_datos"),
    Valor = c(texto_bandas_cipat(mapa_bandas_cipat(p, clave_rgb_cipat(p), Inf)[c("R", "G", "B")]),if(isTRUE(p$datos_ejemplo)) "Ejemplo incluido" else "Archivos del usuario")))
  if (!is.null(p$muestras)) registrar("Clasificando RGB por muestras y calculando cobertura adicional")
  seg_muestras <- agregar_cobertura_muestras_cipat(p, tabla, polys, metadatos)
  tabla <- seg_muestras$tabla; metadatos <- seg_muestras$meta
  # Todos los GeoTIFF de salida están en su ubicación definitiva.
  ruta_mascara <- terra::sources(mask)[1]
  rm(bandas, ndvi, mask)
  rm(list = intersect(c("Red", "Green", "Blue", "RE", "NIR"), ls()))
  invisible(gc())
  # Solo se eliminan los temporales de esta ejecución, nunca los TIFF originales.
  unlink(temporales, recursive = TRUE, force = TRUE)
  registrar("Guardando respaldo RDS/CSV y Excel")
  tabla <- normalizar_resultados_cipat(tabla, "sentera")
  ruta_excel <- guardar_resultados_sentera(tabla, metadatos, p, umbral)
  registrar("Proceso terminado. Excel y GeoTIFF guardados.")
  list(tabla = tabla, excel = ruta_excel, ndvi = rutas$NDVI,
       mask = ruta_mascara, umbral = umbral, salida = p$salida)
}

# Módulo Phantom. Alturas de salida en cm; RGB y CHM conservan sus rejillas.
resumen_chm <- function(r, poligonos) {
  nombres <- c("mean", "median", "sd", "cv", "min", "max", "p10", "p25", "p75", "p90", "p95", "IQR")
  f <- function(values, coverage_fraction) {
    v <- if (is.data.frame(values) || is.matrix(values)) values[, 1] else values
    v <- as.numeric(v)
    v <- v[is.finite(v) & is.finite(coverage_fraction) & coverage_fraction > 0]
    if (!length(v)) return(as.data.frame(as.list(stats::setNames(rep(NA_real_, 12), nombres))))
    m <- mean(v); s <- stats::sd(v); q <- stats::quantile(v, c(.1, .25, .75, .9, .95), names = FALSE)
    as.data.frame(as.list(stats::setNames(c(m, stats::median(v), s,
      if (abs(m) < 1e-8) NA_real_ else s / m, min(v), max(v), q, stats::IQR(v)), nombres)))
  }
  do.call(rbind, lapply(seq_len(nrow(poligonos)), function(j) exactextractr::exact_extract(r, poligonos[j, ], fun = f, progress = FALSE)))
}

procesar_phantom <- function(p) {
  p <- preparar_indices_rgb_cipat(p)
  dir.create(p$salida, recursive = TRUE, showWarnings = FALSE)
  temporales <- tempfile("cipat_rgb_")
  on.exit(unlink(temporales, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(temporales, showWarnings = FALSE)
  opciones_terra <- terra::terraOptions(print = FALSE)
  on.exit(do.call(terra::terraOptions, opciones_terra[intersect(names(opciones_terra),
    c("tempdir", "memfrac", "memmax", "memmin", "todisk", "progress"))]), add = TRUE)
  terra::terraOptions(tempdir = temporales, memfrac = .4, progress = 0)
  registrar <- function(txt) cat(format(Sys.time(), "%H:%M:%S"), " | ", txt, "\n", file = p$log, append = TRUE, sep = "")
  guardar <- function(r, nombre) {
    ruta <- archivo_salida_cipat(p, nombre, "tif")
    terra::writeRaster(r, ruta, overwrite = FALSE, datatype = "FLT4S")
    terra::rast(ruta)
  }
  comprobar_crs <- function(r) {
    c <- sf::st_crs(terra::crs(r))
    if (is.na(c) || isTRUE(sf::st_is_longlat(c)) || !isTRUE(c$units_gdal %in% c("metre", "meter", "m"))) {
      stop("RGB y modelos de altura requieren un CRS proyectado en metros, por ejemplo UTM.")
    }
  }
  registrar("Phantom: leyendo RGB y parcelas")
  RGB <- terra::rast(p$rgb); comprobar_crs(RGB)
  if (terra::nlyr(RGB) < 3) stop("El RGB Phantom necesita al menos tres bandas.")
  polys <- sf::st_read(p$polys, quiet = TRUE)
  if (!nrow(polys) || is.na(sf::st_crs(polys))) stop("Revisa las parcelas y su CRS.")
  polys <- sf::st_transform(sf::st_make_valid(polys), terra::crs(RGB))
  if (any(sf::st_is_empty(polys)) || !all(as.character(sf::st_geometry_type(polys)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("Todas las parcelas deben ser polígonos válidos y no vacíos.")
  if (!p$id %in% names(polys)) stop("No existe el campo ID. Campos: ", paste(names(polys), collapse = ", "))
  ids <- as.character(polys[[p$id]])
  if (anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids)) stop("Los ID deben ser únicos y no estar vacíos.")
  # Mantener toda la rejilla del RGB; seleccionar canales no modifica su geometría.
  RGB <- RGB[[unname(mapa_bandas_cipat(p, "rgb_phantom", terra::nlyr(RGB)))]]
  R <- RGB[[1]]; G <- RGB[[2]]; B <- RGB[[3]]
  registrar("Calculando índices RGB y características de color")
  bandas_editor <- list(R = R, G = G, B = B)
  # Normalizar solo si alguna fórmula de índice pide expresamente rn, gn o bn.
  variables_formula <- unique(unlist(lapply(p$formulas, function(f) all.vars(parse(text = f)))))
  for (k in intersect(c("rn", "gn", "bn"), variables_formula)) {
    canal <- c(rn = "R", gn = "G", bn = "B")[[k]]
    bandas_editor[[k]] <- bandas_editor[[canal]] / p$escala
  }
  exg <- guardar(ejecutar_formula_cipat(p$formulas[["ExG"]], bandas_editor), "ExG")
  registrar("Calculando HSB y Lab con pliman; DGCI por bloques")
  color <- calcular_color_archivo_cipat(c(R, G, B), p$escala,
    file.path(temporales, "color_hsb_lab.tif"), registrar = registrar)
  nombres_color <- c("HSB_Hue", "HSB_Saturation", "HSB_Brightness", "DGCI", "Lab_L", "Lab_a", "Lab_b")
  names(color) <- nombres_color
  rasters <- list(Red = R, Green = G, Blue = B, ExG = exg)
  for (k in nombres_color) {
    registrar(paste("Guardando", k))
    rasters[[k]] <- guardar(color[[k]], k)
  }
  registrar("Color HSB/DGCI/Lab terminado")
  if (!"ExG" %in% p$indices) rasters$ExG <- NULL
  for (nm in setdiff(names(p$formulas), "ExG")) {
    rasters[[nm]] <- guardar(ejecutar_formula_cipat(p$formulas[[nm]], bandas_editor), nm)
  }
  muestra <- muestra_otsu_cipat(exg, polys, margen = max(terra::res(RGB)) * (p$ventana + 2))
  vals <- as.numeric(muestra[[1]]); vals <- vals[is.finite(vals)]
  if (!length(vals)) stop("El RGB no tiene valores válidos en la extensión de las parcelas.")
  if (p$metodo == "Otsu") {
    minv <- attr(muestra, "rango_muestreo")[1]
    maxv <- attr(muestra, "rango_muestreo")[2]
    if (!is.finite(minv) || !is.finite(maxv) || maxv <= minv) stop("ExG constante: usa un umbral manual.")
    u <- autothresholdr::auto_thresh(as.integer(pmax(0, pmin(255, 255 * (vals - minv) / (maxv - minv)))), "Otsu")
    umbral <- minv + as.numeric(u) / 255 * (maxv - minv)
  } else umbral <- p$umbral
  registrar(paste("Umbral ExG:", round(umbral, 3)))
  mask_rgb <- guardar(terra::ifel(exg > umbral, 1, 0), "Mascara_ExG_RGB")
  tabla <- data.frame(id = ids, Lote = p$lote, Fecha = as.character(p$fecha), stringsAsFactors = FALSE)
  atributos <- sf::st_drop_geometry(polys)
  atributos <- atributos[, setdiff(names(atributos), p$id), drop = FALSE]
  # Un archivo de parcelas puede contener solamente el ID y la geometría.
  if (ncol(atributos) > 0L) {
    names(atributos) <- paste0("Parcela_", names(atributos))
    tabla <- cbind(tabla, atributos)
  }
  for (nombre in names(rasters)) {
    registrar(paste("Extrayendo", nombre, "en vegetación RGB"))
    r <- terra::ifel(mask_rgb == 1, rasters[[nombre]], NA)
    df <- resumen_raster(r, polys)
    df <- ajustar_estadisticas_color_cipat(df, nombre)
    names(df) <- paste(nombre, names(df), sep = "_")
    tabla <- cbind(tabla, df)
  }
  registrar("Preparando modelo de altura")
  if (p$fuente_altura == "CHM") {
    CHM <- terra::rast(p$chm); comprobar_crs(CHM)
    if (terra::nlyr(CHM) != 1) stop("El CHM debe tener una sola banda.")
    polys_chm <- sf::st_transform(polys, terra::crs(CHM))
    # CHM conserva su propia extensión y rejilla originales.
  } else {
    DSM <- terra::rast(p$dsm); DTM <- terra::rast(p$dtm)
    comprobar_crs(DSM); comprobar_crs(DTM)
    if (terra::nlyr(DSM) != 1 || terra::nlyr(DTM) != 1) stop("DSM y DTM deben tener una sola banda.")
    polys_chm <- sf::st_transform(polys, terra::crs(DSM))
    # El CHM derivado conserva la rejilla completa del DSM de referencia.
    if (terra::same.crs(DTM, DSM)) DTM <- terra::resample(DTM, DSM, method = "bilinear")
    else DTM <- terra::project(DTM, DSM, method = "bilinear")
    CHM <- DSM - DTM
  }
  if (p$unidad == "m") CHM <- CHM * 100
  names(CHM) <- "CHM_cm"
  CHM <- guardar(CHM, "CHM_cm")
  mask_chm <- guardar(terra::ifel(CHM >= p$altura_min & CHM <= p$altura_max, 1, 0), "Mascara_CHM")
  filtrado <- guardar(terra::ifel(mask_chm == 1, CHM, NA), "CHM_filtrado_cm")
  df <- resumen_chm(filtrado, polys_chm)
  names(df) <- paste0("Canopy_height_image_", names(df))
  tabla <- cbind(tabla, df)
  cobertura <- function(mask, polygons, prefijo) {
    df <- resumen_cobertura(mask, polygons)
    area_pixel <- prod(terra::res(mask))
    df$total_area_m2 <- df$total_pix * area_pixel
    df$veg_area_m2 <- df$veg_pix * area_pixel
    df$soil_area_m2 <- (df$total_pix - df$veg_pix) * area_pixel
    df$polygon_area_m2 <- as.numeric(sf::st_area(polygons))
    df$valid_area_pct <- 100 * df$total_area_m2 / df$polygon_area_m2
    names(df) <- paste0(prefijo, names(df))
    df
  }
  registrar("Calculando cobertura RGB")
  cov_rgb <- cobertura(mask_rgb, polys, "Plant_ground_cover_image_RGB_")
  cov_rgb <- cov_rgb[paste0("Plant_ground_cover_image_RGB_", c("pct", "veg_area_m2", "polygon_area_m2", "valid_area_pct"))]
  tabla <- cbind(tabla, cov_rgb)
  capas <- list("RGB" = p$rgb, "ExG" = terra::sources(exg)[1], "Máscara RGB" = terra::sources(mask_rgb)[1],
    "CHM original (cm)" = terra::sources(CHM)[1], "CHM filtrado (cm)" = terra::sources(filtrado)[1],
    "Máscara CHM" = terra::sources(mask_chm)[1])
  for (nm in setdiff(names(p$formulas), "ExG")) capas[[nm]] <- terra::sources(rasters[[nm]])[1]
  for (nm in nombres_color) capas[[nm]] <- terra::sources(rasters[[nm]])[1]
  glcm_min <- glcm_max <- NA_real_
  if (isTRUE(p$texturas)) {
    registrar("Calculando texturas GLCM en banda verde: entropía, contraste y homogeneidad")
    raster::rasterOptions(tmpdir = temporales, maxmemory = 2e8)
    green_veg <- guardar(terra::ifel(mask_rgb == 1, G, NA), "Green_vegetacion")
    g <- raster::raster(terra::sources(green_veg)[1])
    min_g <- raster::cellStats(g, "min"); max_g <- raster::cellStats(g, "max")
    glcm_min <- min_g; glcm_max <- max_g
    etiquetas <- c("Entropy", "Contrast", "Homogeneity")
    if (is.finite(min_g) && is.finite(max_g) && max_g > min_g) {
      tx <- glcm::glcm(g, n_grey = 32, window = c(p$ventana, p$ventana),
        shift = list(c(1, 0), c(1, 1), c(0, 1), c(-1, 1)), min_x = min_g, max_x = max_g,
        statistics = c("entropy", "contrast", "homogeneity"), na_opt = "any")
      tx <- terra::rast(tx)
    } else {
      registrar("Banda verde constante o sin vegetación; texturas se exportan como NA.")
      tx <- c(green_veg * NA_real_, green_veg * NA_real_, green_veg * NA_real_)
    }
    for (j in seq_along(etiquetas)) {
      r <- guardar(tx[[j]], paste0(etiquetas[j], "_RGB"))
      df <- resumen_raster(r, polys)
      df$sum <- NULL
      names(df) <- paste0("RGB_", etiquetas[j], "_", names(df))
      tabla <- cbind(tabla, df)
      capas[[paste0("Textura ", etiquetas[j])]] <- terra::sources(r)[1]
    }
  }
  meta <- data.frame(Parametro = c("Tipo", "Lote", "Fecha", "RGB", "Parcelas", "Campo_ID", "Fuente_altura", "CHM", "DSM", "DTM",
    "Unidad_entrada", "Unidad_salida", "Filtro_altura_cm", "Escala_RGB", "Metodo_ExG", "Umbral_ExG", "GLCM", "Ventana_GLCM", "Muestreo_Otsu", "Cobertura"),
    Valor = c("Phantom", p$lote, as.character(p$fecha), p$rgb, p$polys, p$id, p$fuente_altura, p$chm, p$dsm, p$dtm,
      p$unidad, "cm", paste(p$altura_min, p$altura_max, sep = " a "), as.character(p$escala), p$metodo, as.character(umbral),
      as.character(p$texturas), paste(p$ventana, "x", p$ventana), "Aleatorio, hasta 200000 píxeles; área de muestreo con buffer; salida sin recorte", "Ponderada por fracción de píxel; denominador: área válida"))
  meta <- rbind(meta, data.frame(Parametro = c("Bandas_RGB","Modo_datos"),
    Valor = c(texto_bandas_cipat(mapa_bandas_cipat(p, "rgb_phantom", Inf)),if(isTRUE(p$datos_ejemplo)) "Ejemplo incluido" else "Archivos del usuario")))
  meta <- rbind(meta, data.frame(Parametro = c("Color", "HSB_escala", "Lab", "GLCM_direcciones", "GLCM_niveles", "GLCM_min", "GLCM_max", "GLCM_NA"),
    Valor = c("pliman::rgb_to_hsb; DGCI de la guía; valores no finitos como NoData", "H: grados; S y brillo: porcentaje",
      paste0("pliman::rgb_to_hsb y pliman::rgb_to_lab; versión ", utils::packageVersion("pliman")), "0,45,90,135 grados; distancia 1; promedio", "32",
      as.character(glcm_min), as.character(glcm_max), "any: excluye ventanas con NA")))
  if (!is.null(p$muestras)) registrar("Clasificando RGB por muestras y calculando cobertura adicional")
  seg_muestras <- agregar_cobertura_muestras_cipat(p, tabla, polys, meta)
  tabla <- seg_muestras$tabla; meta <- seg_muestras$meta
  if (!is.null(seg_muestras$ruta)) capas[["Máscara por muestras"]] <- seg_muestras$ruta
  tabla <- normalizar_resultados_cipat(tabla, "phantom")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "HTP_Phantom"); openxlsx::writeData(wb, "HTP_Phantom", tabla)
  openxlsx::freezePane(wb, "HTP_Phantom", firstRow = TRUE, firstCol = TRUE)
  openxlsx::addWorksheet(wb, "Parametros"); openxlsx::writeData(wb, "Parametros", meta)
  registrar_formulas_cipat(wb, p)
  documentar_rasgos_color_cipat(wb, p)
  openxlsx::writeData(wb, "Parametros", data.frame(
    Parametro = c("Indices_seleccionados", "ExG_para_mascara"),
    Valor = c(paste(p$indices, collapse = "; "), "Siempre calculado; su casilla controla las estadísticas")),
    startRow = nrow(meta) + 2L, colNames = FALSE)
  excel <- archivo_salida_cipat(p, "HTP", "xlsx")
  openxlsx::saveWorkbook(wb, excel, overwrite = FALSE)
  registrar("Phantom terminado: Excel y GeoTIFF guardados.")
  list(tabla = tabla, excel = excel, umbral = umbral, salida = p$salida, tipo = "phantom", fuentes = p, capas = capas)
}

procesar_modulo <- function(p) {
  if (p$tipo == "phantom") return(procesar_phantom(p))
  x <- procesar_htp(p)
  x$tipo <- "sentera"; x$fuentes <- p
  x$capas <- list("RGB" = p$rgb, "NDVI" = x$ndvi, "Máscara NDVI" = x$mask)
  if (!is.null(p$muestras)) x$capas[["Máscara por muestras"]] <- archivo_salida_cipat(p, "Mascara_Muestras", "tif")
  for (i in unique(c("NDVI", p$indices))) {
    x$capas[[i]] <- file.path(p$salida, nombre_tif_sentera(i, p$lote, p$fecha))
  }
  x
}

nombre_modulo_cipat <- function(tipo) {
  if (tipo == "phantom") "Rasgos estructurales y de color" else "Rasgos espectrales de la vegetación"
}
source(file.path("R", "exploracion.R"), local = TRUE)
source(file.path("R", "lectura_resultados.R"), local = TRUE)
source(file.path("R", "mapa_interactivo.R"), local = TRUE)
source(file.path("R", "bandas_entrada.R"), local = TRUE)
source(file.path("R", "entradas_productos.R"), local = TRUE)
source(file.path("R", "vectoriales.R"), local = TRUE)
source(file.path("R", "integrar_datos.R"), local = TRUE)
source(file.path("R", "testigos.R"), local = TRUE)
source(file.path("R", "resumen_resultados.R"), local = TRUE)
source(file.path("R", "analisis_selectivo.R"), local = TRUE)
procesar_modulo <- function(p) procesar_seleccion_cipat(p)
simbolo_seccion <- function(titulo) {
  t <- tolower(titulo)
  if (grepl("distribuci|dispersi|estad|comparaci|asociaci|explora", t)) return("chart-column")
  if (grepl("archivo|imagen|parcela", t)) return("layer-group")
  if (grepl("configura|identifica|variable|segmenta", t)) return("sliders")
  if (grepl("proces|estado|registro", t)) return("gears")
  if (grepl("salida|resultado", t)) return("chart-line")
  "seedling"
}
ayuda_contextual <- function(titulo = "Información", ..., compacta = FALSE) {
  tags$details(class = if (compacta) "context-help help-icon" else "context-help",
    tags$summary(title = titulo, icon("circle-info"), span(titulo)),
    div(class = "help-content", ...))
}
ayuda_breve <- function(...) ayuda_contextual("Información", ...)
tarjeta <- function(titulo, descripcion = NULL, ...) {
  div(class = "card", div(class = "card-heading",
    div(class = "card-emblem", icon(simbolo_seccion(titulo))),
    div(class = "card-heading-copy", h3(titulo)),
    tags$button(type = "button", class = "panel-collapse", title = "Contraer panel", `aria-label` = "Contraer panel", `aria-expanded` = "true", icon("minus")),
    if (!is.null(descripcion)) ayuda_contextual(paste("Acerca de", titulo), descripcion, compacta = TRUE)), ...)
}
encabezado <- function(etiqueta, titulo, texto) {
  div(class = "page-heading", div(class = "page-emblem", icon(simbolo_seccion(titulo))),
    h2(titulo), ayuda_contextual(paste("Acerca de", titulo), texto, compacta = TRUE))
}
metrica <- function(etiqueta, salida, nota, simbolo) {
  div(class = "metric", title = nota, div(class = "metric-icon", icon(simbolo)), div(class = "metric-label", etiqueta),
      div(class = "metric-value", textOutput(salida)))
}
metricas <- function() div(class = "kpi-grid",
  metrica("Parcelas evaluadas", "kpi_parcelas", "Evaluación procesada", "layer-group"),
  metrica(textOutput("etiqueta_cobertura"), "kpi_cobertura", "Promedio entre parcelas", "leaf"),
  metrica(textOutput("etiqueta_rasgo"), "kpi_ndvi", "Promedio entre parcelas", "seedling"),
  metrica(textOutput("etiqueta_umbral"), "kpi_umbral", "Utilizado en la máscara", "sliders"))

importador_cipat <- function(campo, titulo, nota = NULL) {
  div(class = "import-block",
    div(class = "file-title", icon(if (campo == "polys") "draw-polygon" else "file-image"), titulo),
    if (campo != "polys") div(class = "internal-path", style = "display:none!important", `aria-hidden` = "true", textInput(campo, NULL, value = "")),
    conditionalPanel(condicion_archivos_propios_cipat(), actionButton(paste0("elegir_", campo), "Importar archivo", icon = icon("file-import"), class = "import-button")),
    div(class = "import-name", textOutput(paste0("nombre_", campo))),
    if (campo != "polys") controles_ortomosaico_cipat(campo),
    if (!is.null(nota)) ayuda_breve(nota))
}

paleta_cabecera_cipat <- function() {
  colores <- c("Verde Papalotla" = "#1b552f", "Verde vivo" = "#27833e", "Azul" = "#294e61", "Grafito" = "#374b45", "Blanco" = "#ffffff")
  tags$details(class = "header-palette",
    tags$summary(title = "Cambiar color de la barra superior", `aria-label` = "Cambiar color de la barra superior", icon("palette")),
    div(class = "palette-popover", strong("Color de la barra"),
      div(class = "palette-options", lapply(seq_along(colores), function(i)
        tags$button(type = "button", class = "palette-swatch", style = paste0("--swatch:", colores[i]),
          `data-header-color` = unname(colores[i]), `aria-label` = names(colores)[i], title = names(colores)[i],
          `aria-pressed` = if (i == 1L) "true" else "false", span(class = "swatch-check", "✓")))),
      tags$label(`for` = "color_cabecera", "Personalizado"),
      tags$input(id = "color_cabecera", type = "color", value = "#1b552f", `aria-label` = "Color personalizado de la barra superior"),
      tags$small("Predeterminado: verde Papalotla")))
}

ui <- fluidPage(
  title = "PhenoImage | Fenotipado UAV · 4.8.0",
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "estilo.css?v=4.8.0-UNIFICADA"), tags$script(src = "interfaz.js?v=4.8.0-UNIFICADA")),
  div(class = "app-shell",
    tags$aside(class = "side-nav",
      div(class = "brand", tags$img(src = "cipat.png", alt = "CIPAT · Centro de Investigación de Pastos Tropicales", class = "side-logo")),
      div(class = "nav-label", "Módulo activo"),
      div(class = "module-switch", radioButtons("tipo", NULL, choices = c("Sentera Double 4K" = "sentera", "RGB convencional" = "phantom"), selected = "sentera", inline = TRUE)),
      div(class = "menu-list",
        actionButton("nav_inicio", span(class = "nav-text", "Inicio"), icon = icon("house"), class = "nav-button active", title = "Inicio", `aria-label` = "Inicio"),
        actionButton("nav_archivos", span(class = "nav-text", "Datos"), icon = icon("folder-open"), class = "nav-button", title = "Datos", `aria-label` = "Datos"),
        actionButton("nav_configuracion", span(class = "nav-text", "Parámetros"), icon = icon("sliders"), class = "nav-button", title = "Parámetros", `aria-label` = "Parámetros"),
        actionButton("nav_proceso", span(class = "nav-text", "Procesamiento"), icon = icon("gears"), class = "nav-button", title = "Procesamiento", `aria-label` = "Procesamiento"),
        actionButton("nav_resultados", span(class = "nav-text", "Resultados"), icon = icon("chart-simple"), class = "nav-button", title = "Resultados", `aria-label` = "Resultados"),
        actionButton("nav_exploracion", span(class = "nav-text", "Exploración"), icon = icon("magnifying-glass-chart"), class = "nav-button", title = "Exploración", `aria-label` = "Exploración"),
        actionButton("nav_ayuda", span(class = "nav-text", "Guía"), icon = icon("circle-question"), class = "nav-button", title = "Guía", `aria-label` = "Guía")),
      div(class = "side-footer", div(class = "local-pill", span(class = "dot"), "Sesión local"))),
    tags$main(class = "workspace",
      div(class = "topbar",
        div(class = "header-context", tags$button(id = "toggle_menu", type = "button", class = "toolbar-button", title = "Mostrar / contraer menú", `aria-label` = "Mostrar o contraer menú", `aria-expanded` = "true", icon("bars")),
          strong("PhenoImage", span(class = "build-badge", title = "Información del ortomosaico y asignación de bandas", "4.8.0")), span(class = "top-title", textOutput("modulo_actual"))),
        div(class = "header-actions", div(class = "status-pill", span(class = "dot"), textOutput("estado_corto")),
          paleta_cabecera_cipat(),
          tags$button(id = "pantalla_completa", type = "button", class = "toolbar-button", title = "Pantalla completa", `aria-label` = "Pantalla completa", icon("expand")),
          tags$img(src = "papalotla_transparente.png", alt = "Grupo Papalotla", class = "header-logo"))),
      tabsetPanel(id = "secciones", type = "hidden", selected = "inicio",
        tabPanelBody("inicio",
          div(class = "home-dashboard",
            div(class = "home-main",
              div(class = "card cover-panel",
                div(class = "card-heading", icon("seedling"), h3("Plataforma de fenotipado"),
                  span(class = "version-tag", "PhenoImage"),
                  tags$button(type = "button", class = "panel-collapse", title = "Contraer panel", `aria-label` = "Contraer panel", `aria-expanded` = "true", icon("minus"))),
                div(class = "cover-art",
                  div(class = "cover-copy", span(class = "cover-kicker", "GRUPO PAPALOTLA · CIPAT"),
                    h1("PhenoImage", br(), "Caracterización", br(), "Fenotípica"),
                    p("Caracterización vegetal mediante imágenes UAV")),
                  span(class = "hero-tag", "Ilustración conceptual")),
                div(class = "cover-footer",
                  div(icon("leaf"), span("Centro de Investigación de Pastos Tropicales")),
                  actionButton("comenzar", "Nueva evaluación", icon = icon("circle-plus"), class = "btn-primary")))),
            tags$aside(class = "home-tools",
              div(class = "launch-card spectral-launch",
                div(class = "launch-overview",
                  div(class = "launch-text",
                    span(class = "launch-kicker", "MÓDULO 01"), h3("Rasgos espectrales", br(), "de la vegetación"),
                    p(class = "launch-sensor", "(Sensor Sentera Double 4K)"),
                    p(class = "launch-description", "Vigor, respuesta espectral y cobertura.")),
                  tags$img(src = "modulo_sentera_v465.png", alt = "Ilustración conceptual de un UAV y capas espectrales sobre parcelas", class = "launch-illustration")),
                div(class = "launch-features", `aria-label` = "Características del módulo multiespectral",
                  span("NDVI / NDRE"), span("Índices espectrales"), span("Cobertura")),
                actionButton("seleccionar_sentera", "Abrir módulo", icon = icon("arrow-right"), class = "launch-button")),
              div(class = "launch-card structure-launch",
                div(class = "launch-overview",
                  div(class = "launch-text",
                    span(class = "launch-kicker", "MÓDULO 02"), h3("Rasgos estructurales", br(), "y de color"),
                    p(class = "launch-sensor", "(Sensor RGB convencional)"),
                    p(class = "launch-description", "Altura del dosel, cobertura, color y textura.")),
                  tags$img(src = "modulo_rgb_v465.png", alt = "Ilustración conceptual de altura del dosel, color y textura de la vegetación", class = "launch-illustration")),
                div(class = "launch-features", `aria-label` = "Características del módulo estructural y RGB",
                  span("Altura del dosel"), span("ExG / RGB"), span("Texturas")),
                actionButton("seleccionar_phantom", "Abrir módulo", icon = icon("arrow-right"), class = "launch-button")),
              div(class = "card quick-access",
                div(class = "card-heading", h3("Accesos")),
                div(class = "quick-links",
                  actionButton("ir_archivos", span(span(class = "quick-label-full", "Datos de entrada"), span(class = "quick-label-compact", "Datos")), title = "Datos de entrada", icon = icon("folder-open"), class = "quick-button"),
                  actionButton("ir_configuracion", "Parámetros", icon = icon("sliders"), class = "quick-button"),
                  actionButton("ir_resultados", span(span(class = "quick-label-full", "Abrir resultados"), span(class = "quick-label-compact", "Resultados")), title = "Abrir resultados", icon = icon("chart-line"), class = "quick-button")))),
            div(class = "home-summary", uiOutput("etiqueta_resultado"), metricas()))),
        tabPanelBody("archivos",
          encabezado("01 · PREPARACIÓN", "Datos de entrada", "Carga únicamente los archivos del módulo seleccionado en el menú lateral."),
          tarjeta("¿Qué deseas analizar?", "Elige antes de cargar archivos. Los polígonos de parcelas son comunes a todos los análisis.",
            radioButtons("modo_analisis", NULL, c("Análisis completo" = "completo", "Personalizar análisis" = "personalizado"), inline = TRUE),
            conditionalPanel("input.modo_analisis == 'personalizado' && input.tipo == 'phantom'",
              checkboxGroupInput("grupos_phantom", "Análisis RGB", grupos_disponibles_cipat("phantom"), selected = unname(grupos_disponibles_cipat("phantom")))),
            conditionalPanel("input.modo_analisis == 'personalizado' && input.tipo == 'sentera'",
              checkboxGroupInput("grupos_sentera", "Análisis Sentera", grupos_disponibles_cipat("sentera"), selected = unname(grupos_disponibles_cipat("sentera")))),
            conditionalPanel("input.tipo == 'sentera'", radioButtons("metodo", "Máscara para las estadísticas", c("NDVI · Otsu"="Otsu", "NDVI · umbral manual"="Manual", "Por muestras"="Muestras"))),
            conditionalPanel("input.tipo == 'phantom' && (input.modo_analisis == 'completo' || (input.grupos_phantom || []).some(function(x){return x != 'altura';}))", radioButtons("metodo_phantom", "Máscara para las estadísticas", c("ExG · Otsu"="Otsu", "ExG · umbral manual"="Manual", "Por muestras"="Muestras"))),
            textOutput("requisitos_analisis")),
          selector_ejemplos_cipat(),
          tabsetPanel(id = "tipo_entrada", type = "tabs",
            tabPanel("Ráster", value = "raster",
              div(class = "workspace-split raster-input-layout",
                div(class = "input-tools",
                  tarjeta("Imágenes y modelos digitales", "Importa las imágenes correspondientes al módulo activo.",
                    conditionalPanel("input.tipo == 'sentera'",
                      selectInput("origen_sentera", "Formato de entrada", choices = c(
                        "Ortomosaicos Sentera · RGB y RE/NIR" = "metashape",
                        "Multibanda Sentera" = "multibanda"), selected = "metashape"),
                      conditionalPanel("input.origen_sentera != 'multibanda'",
                        importador_cipat("rgb", "Ortomosaico RGB · Sentera", "Bandas rojo, verde y azul (R=1, G=2, B=3)."),
                        conditionalPanel("input.modo_analisis == 'completo' || input.metodo != 'Muestras' || (input.grupos_sentera || []).some(function(x){return x == 'indices' || x == 'bandas';})", importador_cipat("ms", "Ortomosaico (RE/NIR) · Sentera", "Bandas Red Edge e infrarrojo cercano (RE: banda 1; NIR: banda 3)."))),
                      conditionalPanel("input.origen_sentera == 'multibanda'",
                        importador_cipat("multibanda", "Multibanda Sentera", "GeoTIFF previamente corregido: 1 Rojo · 2 Verde · 3 Azul · 4 Red edge · 5 NIR."))),
                    conditionalPanel("input.tipo == 'phantom'",
                      conditionalPanel("input.tipo == 'sentera' || input.modo_analisis == 'completo' || (input.grupos_phantom || []).some(function(x){return x != 'altura';})", importador_cipat("rgb_phantom", "Ortomosaico RGB convencional", "Bandas rojo, verde y azul (R=1, G=2, B=3).")),
                      conditionalPanel("input.tipo == 'phantom' && (input.modo_analisis == 'completo' || (input.grupos_phantom || []).indexOf('altura') >= 0)",
                      radioButtons("fuente_altura", "Fuente del modelo de altura", c("CHM ya calculado" = "CHM", "DSM y DTM" = "DSM_DTM")),
                      conditionalPanel("input.fuente_altura == 'CHM'", importador_cipat("chm", "Modelo de altura del dosel (CHM)", "Altura de la vegetación respecto al suelo.")),
                      conditionalPanel("input.fuente_altura == 'DSM_DTM'",
                        importador_cipat("dsm", "Modelo digital de superficie (DSM)"),
                        importador_cipat("dtm", "Modelo digital del terreno (DTM)"),
                        ayuda_breve("El CHM se calcula a partir de DSM y DTM. Ambos modelos necesitan la misma referencia vertical y unidad de elevación."))))),
                  tarjeta("Vista previa", "Selecciona una imagen cargada para inspeccionarla antes de procesar.", uiOutput("raster_entrada_controles")),
                  div(class = "bottom-actions", actionButton("ver_vector", "Continuar a parcelas", icon = icon("arrow-right"), class = "btn-primary"))),
                div(class = "interactive-map-panel", leaflet::leafletOutput("raster_entrada_mapa", height = "620px"),
                  div(class = "map-caption", icon("map"), "Imágenes y modelos digitales · vista previa"))),
              uiOutput("raster_entrada_aviso")),
            tabPanel("Vectorial", value = "vector",
              div(class = "workspace-split",
                tarjeta("Parcelas vectoriales", "Importa el archivo de delimitación y comprueba su ubicación con las imágenes cargadas.",
                  conditionalPanel(condicion_archivos_propios_cipat(), selector_vectoriales_cipat()),
                  importador_cipat("polys", "Agregar vectorial · SHP o GPKG", "Cada importación se agrega al menú y se selecciona para el módulo activo. SHP: conserva .shp, .shx, .dbf y .prj. GPKG: utiliza una sola capa."),
                  uiOutput("vector_entrada_controles")),
                div(class = "interactive-map-panel", leaflet::leafletOutput("vector_entrada_mapa", height = "620px"),
                  div(class = "map-caption", icon("layer-group"), "Parcelas e imágenes de entrada · vista previa"))),
              uiOutput("vector_entrada_aviso")),
            tabPanel("Destino", value = "destino",
              tarjeta("Carpeta de resultados", "Las evaluaciones de ambos módulos se guardan en carpetas independientes.",
                div(class = "internal-path", style = "display:none!important", `aria-hidden` = "true", textInput("salida", NULL, value = "")),
                actionButton("elegir_salida", "Seleccionar carpeta", icon = icon("folder-open"), class = "import-button"),
                div(class = "import-name", textOutput("nombre_salida")),
                checkboxInput("crear_visores", "Guardar mapas interactivos de los productos al finalizar", TRUE),
                ayuda_breve("Los visores HTML y su catálogo se guardan junto a las salidas. Las vistas reducidas no sustituyen a los GeoTIFF.")))),
          uiOutput("revision_archivos"),
          div(class = "bottom-actions", actionButton("continuar_config", "Continuar a configuración", icon = icon("arrow-right"), class = "btn-primary"))),
        tabPanelBody("configuracion",
          encabezado("02 · PARÁMETROS", "Parámetros de análisis", "Los ajustes disponibles corresponden al módulo seleccionado."),
          div(class = "parameter-layout",
            div(class = "parameter-controls",
              tarjeta("Identificación de la evaluación", "Lote, fecha y parcelas pueden ser comunes a ambos sensores.",
                textInput("lote", "Lote / evaluación", "BH16_Precorte"),
                dateInput("fecha", "Fecha de evaluación", value = Sys.Date(), format = "dd/mm/yyyy"),
                conditionalPanel("input.tipo == 'sentera'", textInput("id_sentera", "Campo ID · Sentera", "id")),
                conditionalPanel("input.tipo == 'phantom'", textInput("id_phantom", "Campo ID · RGB convencional", "id")),
                ayuda_breve("Cada resultado conserva el lote, fecha, ID y parámetros de su ejecución."),
                conditionalPanel("input.tipo == 'sentera' && input.origen_sentera != 'multibanda'", numericInput("factor", "Factor inter-cámara Sentera", 2.7, min = .01, step = .1)),
                conditionalPanel("input.tipo == 'phantom' && (input.modo_analisis == 'completo' || (input.grupos_phantom || []).indexOf('altura') >= 0)",
                  selectInput("unidad_altura", "Unidad en el CHM o en DSM y DTM", choices = c("Centímetros (cm)" = "cm", "Metros (m)" = "m")),
                  ayuda_breve("Todas las alturas de salida se expresan en cm. Metros se convierten multiplicando por 100."),
                  numericInput("altura_min", "Altura mínima aceptada · cm", 2.5, min = 0, step = .5),
                  numericInput("altura_max", "Altura máxima aceptada · cm", 100, min = .1, step = 5),
                  checkboxInput("confirmar_altura", "Unidad de altura y referencia vertical verificadas", FALSE))),
              tarjeta("Variables y segmentación", "Los grupos y la máscara se eligen antes de cargar archivos, en Datos.",
                textOutput("mascara_actual"),
                actionButton("cambiar_analisis", "Cambiar análisis y máscara", icon = icon("sliders")),
                conditionalPanel("input.tipo == 'sentera'",
                  conditionalPanel("input.modo_analisis == 'completo' || (input.grupos_sentera || []).indexOf('indices') >= 0",
                  checkboxGroupInput("indices", "Índices del análisis",
                    choices = c("NDVI", "NDRE", "GNDVI", "CIRE", "CVI", "NDWI", "OSAVI", "SAVI", "EVI"), selected = c("NDVI", "NDRE", "GNDVI", "CIRE", "CVI", "NDWI"))),

                  conditionalPanel("input.metodo == 'Manual'", numericInput("umbral", "Umbral NDVI", .3, min = -1, max = 1, step = .01)),
                  ayuda_breve("OSAVI, SAVI y EVI requieren verificar la escala radiométrica."),
                  conditionalPanel("input.origen_sentera == 'multibanda'", ayuda_breve("Multibanda Sentera utiliza valores ya corregidos: no reaplica matrices ni factor 2.7. NDVI, NDRE y GNDVI disponibles al iniciar. Añade otros en Catálogo y fórmulas. NDVI genera la máscara cuando eliges Otsu o umbral manual; con muestras solo se calcula si lo seleccionas. OSAVI, SAVI y EVI son exploratorios hasta validar la escala."))),
                conditionalPanel("input.tipo == 'phantom' && (input.tipo == 'sentera' || input.modo_analisis == 'completo' || (input.grupos_phantom || []).some(function(x){return x != 'altura';}))",
                  conditionalPanel("input.modo_analisis == 'completo' || (input.grupos_phantom || []).indexOf('indices') >= 0",
                  checkboxGroupInput("indices_rgb", "Índices del análisis",
                    choices = names(indices_originales_cipat("phantom")),
                    selected = names(indices_originales_cipat("phantom")))),
                  ayuda_breve("ExG genera la máscara con Otsu o umbral manual; con muestras solo se calcula si lo seleccionas. Añade otros índices en Catálogo y fórmulas."),

                  conditionalPanel("input.metodo_phantom == 'Manual'", numericInput("umbral_phantom", "Umbral ExG · unidades RGB originales", 0, step = 1)),
                  numericInput("escala_rgb", "Escala RGB · valor máximo", 255, min = 1, step = 1),
                  conditionalPanel("input.modo_analisis == 'completo' || (input.grupos_phantom || []).indexOf('texturas') >= 0", numericInput("ventana", "Ventana GLCM · píxeles", 5, min = 3, max = 15, step = 2),
                    ayuda_breve("Entropía, contraste y homogeneidad; diez estadísticos por textura. Requiere raster y glcm; 32 niveles de gris y cuatro direcciones."))),
                conditionalPanel("(input.tipo == 'phantom' && input.metodo_phantom == 'Muestras') || (input.tipo == 'sentera' && input.metodo == 'Muestras')",
                actionButton("preparar_muestras", "Segmentación por muestras", icon = icon("paintbrush")),
                textOutput("estado_muestras")),
                ayuda_breve("Una sola máscara filtra bandas, índices, color y texturas. Altura utiliza su propio filtro CHM."),
                actionButton("editar_indices", "Catálogo y fórmulas", icon = icon("book-open")),
                ayuda_breve("Fórmulas independientes por módulo. Los cambios se aplican a la siguiente ejecución."),
                ayuda_contextual("Segmentación Otsu", "Otsu usa una muestra de hasta 200000 píxeles. Revisa la máscara antes de interpretar cobertura.")),
              div(class = "bottom-actions", actionButton("revisar_proceso", "Revisar y procesar", icon = icon("arrow-right"), class = "btn-primary"))),
            div(class = "parameter-visual",
              conditionalPanel("input.tipo == 'sentera'",
                div(class = "parameter-art spectral-art",
                  span(class = "art-kicker", "PhenoImage · FENOTIPADO UAV"), h3("Rasgos espectrales de la vegetación"),
                  tags$img(src = "multiespectral.svg", alt = "Ilustración conceptual de capas espectrales y vegetación"),
                  div(class = "art-traits", span("Índices de vegetación"), span("Cobertura por parcela")))),
              conditionalPanel("input.tipo == 'phantom'",
                div(class = "parameter-art structure-art",
                  span(class = "art-kicker", "PhenoImage · FENOTIPADO UAV"), h3("Rasgos estructurales y de color"),
                  tags$img(src = "morfoestructura.svg", alt = "Ilustración conceptual del dosel, la altura y la captura UAV"),
                  div(class = "art-traits", span("Altura del dosel"), span("Cobertura · color · textura")))),
              div(class = "art-caption", icon("seedling"), "Centro de Investigación de Pastos Tropicales", span("Ilustración conceptual"))))),
        tabPanelBody("proceso",
          encabezado("03 · EJECUCIÓN", "Procesamiento de la evaluación", "Revisa la configuración y sigue el avance del análisis."),
          div(class = "grid-two",
            tarjeta("Resumen de configuración", NULL, uiOutput("resumen_config"),
              div(class = "bottom-actions", actionButton("procesar", "Iniciar procesamiento", icon = icon("play"), class = "btn-primary"))),
            tarjeta("Estado de la evaluación", NULL,
              div(class = "run-title", div(class = "run-icon", icon("gears")), h3(textOutput("estado"))),
              ayuda_contextual("Durante el procesamiento", "Puedes recorrer los menús mientras se ejecuta el análisis. Los mapas y tablas se habilitan al finalizar."),
              actionButton("consultar_resultados", "Consultar resultados", icon = icon("arrow-right")))),
          tarjeta("Registro de actividad", "Avance de las etapas y mensajes del procesamiento.",
            verbatimTextOutput("registro", placeholder = TRUE)),
          ayuda_contextual("Acerca del registro", "Las salidas parciales se conservan si ocurre un error. El detalle aparece en este registro.")),
        tabPanelBody("resultados",
          div(class="results-page-heading",
            div(div(class="results-eyebrow", "04 · EVALUACIÓN"),h2("Resultados de la evaluación"),
              p("Resumen del ensayo y acceso a tus productos.")),
            div(class="results-heading-actions",actionButton("abrir_evaluacion", "Abrir Excel", icon=icon("folder-open")),uiOutput("resumen_descarga"))),
          ayuda_breve("Puedes abrir un Excel guardado sin reprocesar imágenes. Para el ortomosaico, conserva el TIFF en la ruta registrada."),
          uiOutput("resultados_contenido")),
        tabPanelBody("exploracion",
          encabezado("05 · ANÁLISIS DESCRIPTIVO", "Análisis exploratorio de datos", "Distribución, comparación de medias y asociación entre rasgos de la evaluación activa."),
          div(class="eda-source-bar",
            selectInput("eda_origen", "Datos para explorar", c("Evaluación del módulo activo"="activo", "Sentera + RGB de esta sesión"="integrada", "Base preparada para exportación"="preparada"), selected="activo"),
            ayuda_breve("En la base integrada puedes elegir una variable de cada sensor. La unión requiere el mismo lote, fecha e ID de parcela.")),
          uiOutput("eda_contenido")),
        tabPanelBody("ayuda",
          encabezado("DOCUMENTACIÓN", "Guía metodológica", "Sigue estas recomendaciones para comprobar entradas y resultados."),
          div(class = "grid-two",
            tarjeta("Antes de procesar", NULL, tags$ol(
              tags$li("Sentera: RGB más multiespectral original (RE=1, NIR=3). Phantom: RGB más CHM, o RGB más DSM y DTM."),
              tags$li("El RGB debe tener un CRS proyectado en metros, por ejemplo UTM."),
              tags$li("Verifica que las imágenes y los polígonos estén correctamente georreferenciados."),
              tags$li("Escribe el nombre exacto del campo ID y una carpeta de resultados."))),
            tarjeta("Cómo interpretar las salidas", NULL, tags$ul(
              tags$li("Las estadísticas espectrales describen solo los píxeles clasificados como vegetación."),
              tags$li("Cobertura NDVI y ExG indican vegetación detectada por cada índice; la cobertura CHM utiliza un filtro de altura."),
              tags$li("Área válida indica qué proporción de cada parcela tiene datos en la capa correspondiente."),
              tags$li("Las tarjetas muestran promedios simples entre parcelas con valores disponibles.")))),
          tarjeta("Acerca de esta versión", NULL,
            p("La aplicación procesa tus datos localmente. El navegador presenta la interfaz y R ejecuta el análisis."),
            p("Ejemplos opcionales: descarga el ZIP RGB o Sentera desde Releases. Con la aplicación detenida, ejecuta phenoImage::install_examples() en R y selecciona el ZIP. Después vuelve a abrir PhenoImage. No se requieren ejemplos para analizar tus propios archivos."),
            p("Sentera conserva su procesamiento. Phantom conserva sus fórmulas y permite configurar unidades, filtros y texturas. Las salidas de cada sensor son independientes."), p("En Phantom las alturas se exportan en cm. Las coberturas NDVI y RGB se exportan por separado. La máscara CHM se utiliza únicamente para filtrar las alturas."),
            p("La portada es una imagen ilustrativa generada. No corresponde a un ensayo ni representa tus resultados."),
            p("El remuestreo alinea rejillas; no corrige desplazamientos físicos entre imágenes.")))
      ),
      div(class = "page-footer", span("Grupo Papalotla · CIPAT"), span("Versión 4.8.0"))
    )
  )
)


server <- function(input, output, session) {
  ortomosaicos <- registrar_ortomosaicos_cipat(input, output, session)
  editor <- registrar_editor_indices_cipat(input, output, session)

  vectoriales <- registrar_vectoriales_cipat(input, output, session)
  ejemplos <- registrar_ejemplos_cipat(input,output,session,vectoriales,editor,ortomosaicos)
  muestras <- registrar_muestras_cipat(input, output, session, ortomosaicos$mapas)
  parcelas_seleccionadas <- vectoriales$parcelas
  resultados <- reactiveValues(sentera = NULL, phantom = NULL)
  ultima <- reactive({ req(input$tipo); resultados[[input$tipo]] })
  registrar_resumen_resultados_cipat(input, output, session, ultima)
  integracion <- registrar_integracion(input, output, session, reactive(list(sentera=resultados$sentera,phantom=resultados$phantom)))
  pendiente <- reactiveVal(NULL)
  log_actual <- reactiveVal(NULL)
  nombre_modulo <- nombre_modulo_cipat
  visores_sesion <- reactiveValues()
  visores_actual <- reactive({
    x <- ultima(); if (is.null(x)) return(NULL)
    v <- visores_sesion[[x$excel]]
    if (is.null(v)) x$visores else v
  })
  observeEvent(input$abrir_evaluacion, {
    if (tarea$status() == "running" || exportacion_visores$status() == "running") {
      showNotification("Espera a que termine la tarea actual antes de abrir otra evaluación.", type = "message"); return()
    }
    tryCatch({
      x <- leer_evaluacion_cipat(file.choose())
      x$visores <- recuperar_visores_cipat(x)
      resultados[[x$tipo]] <- x
      updateRadioButtons(session, "tipo", selected = x$tipo)
      updateTabsetPanel(session, "secciones", selected = "exploracion")
      showNotification("Evaluación recuperada. Puedes explorar sus variables y descargar el resumen.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })
  tarea <- shiny::ExtendedTask$new(function(p) {
    promises::future_promise({
      tryCatch({
        x <- procesar_modulo(p)
        if (isTRUE(p$crear_visores)) {
          cat("Preparando visores interactivos de los productos...\n", file = p$log, append = TRUE)
          x$visores <- tryCatch(exportar_visores_cipat(x, p$assets_visores, p$log),
            error = function(e) list(ok = FALSE, mensaje = conditionMessage(e)))
          if (isTRUE(x$visores$ok)) cat("Visores guardados: ", x$visores$creados, "/", x$visores$total, "\n", file = p$log, append = TRUE)
          else cat("Visores pendientes: ", if (!is.null(x$visores$mensaje)) x$visores$mensaje else "No fue posible crear los mapas; revisa el resumen de visores.", "\n", file = p$log, append = TRUE)
        }
        x
      }, error = function(e) {
        cat("ERROR: ", conditionMessage(e), "\n", file = p$log, append = TRUE)
        llamada <- conditionCall(e)
        if (!is.null(llamada)) cat("Llamada: ", paste(deparse(llamada), collapse = " "), "\n", file = p$log, append = TRUE)
        stop(conditionMessage(e), call. = FALSE)
      })
    }, seed = 20260930)
  })
  exportacion_visores <- shiny::ExtendedTask$new(function(x, assets) {
    promises::future_promise(exportar_visores_cipat(x, assets), seed = 20261001)
  })
  observeEvent(input$generar_visores, {
    req(ultima())
    if (tarea$status() == "running" || exportacion_visores$status() == "running") {
      showNotification("Espera a que termine la tarea actual.", type = "message"); return()
    }
    exportacion_visores$invoke(ultima(), normalizePath("www", winslash = "/", mustWork = TRUE))
  })
  observeEvent(exportacion_visores$status(), {
    if (exportacion_visores$status() == "success") {
      v <- exportacion_visores$result()
      visores_sesion[[v$excel]] <- v
      showNotification(paste(v$creados, "de", v$total, "visores creados."), type = if (isTRUE(v$ok)) "message" else "warning")
    }
    if (exportacion_visores$status() == "error") tryCatch(exportacion_visores$result(), error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })
  output$estado_visores <- renderText({
    if (exportacion_visores$status() == "running") return("Creando mapas interactivos…")
    v <- visores_actual(); if (is.null(v)) return("Puedes generar los visores de esta evaluación sin reprocesar imágenes.")
    if (!is.null(v$mensaje)) return(paste("Visores pendientes:", v$mensaje))
    paste(v$creados, "de", v$total, "visores guardados.", if (v$omitidos > 0) "Consulta Resumen_visores.csv para revisar los productos pendientes." else "")
  })
  output$descarga_visores_ui <- renderUI({
    v <- visores_actual()
    if (!is.null(v) && !is.null(v$zip) && file.exists(v$zip)) downloadButton("descargar_visores", "Descargar visores ZIP", icon = icon("download")) else NULL
  })
  output$descargar_visores <- downloadHandler(filename = function() paste0("PhenoImage_Visores_", gsub("[^[:alnum:]_-]", "_", ultima()$fuentes$lote), ".zip"),
    content = function(file) { req(visores_actual()$zip); if (!file.copy(visores_actual()$zip, file, overwrite = TRUE)) stop("No se pudo copiar el ZIP de visores.") })
  elegir <- function(campo) {
    tryCatch(updateTextInput(session, campo, value = normalizar_ruta(file.choose())),
      error = function(e) showNotification("No se seleccionó un archivo.", type = "message"))
  }
  for (campo in c("rgb", "ms", "multibanda", "rgb_phantom", "chm", "dsm", "dtm")) local({
    clave <- campo
    observeEvent(input[[paste0("elegir_", clave)]], elegir(clave))
  })
  for (campo in c("rgb", "ms", "multibanda", "rgb_phantom", "chm", "dsm", "dtm", "salida")) local({
    clave <- campo
    output[[paste0("nombre_", clave)]] <- renderText({
      ruta <- normalizar_ruta(input[[clave]])
      if (!length(ruta) || !nzchar(ruta)) "Sin archivo seleccionado" else basename(ruta)
    })
  })
  observeEvent(input$origen_sentera, {
    f <- editor[[if (identical(input$origen_sentera, "multibanda")) "sentera_multibanda" else "sentera"]]
    disponibles <- indices_entrada_sentera(names(f), input$origen_sentera)
    seleccion <- intersect(isolate(input$indices), disponibles)
    if (identical(input$origen_sentera, "multibanda") && !length(seleccion)) seleccion <- intersect(c("NDVI", "NDRE", "GNDVI"), disponibles)
    updateCheckboxGroupInput(session, "indices", choices = disponibles, selected = seleccion)
  }, ignoreNULL = FALSE)
  observeEvent(input$elegir_salida, {
    tryCatch({
      ruta <- if (.Platform$OS.type == "windows") utils::choose.dir(caption = "Selecciona la carpeta de resultados")
        else { ruta_archivo <- file.choose(); dirname(ruta_archivo) }
      if (length(ruta) && !is.na(ruta) && nzchar(ruta)) updateTextInput(session, "salida", value = normalizar_ruta(ruta))
    }, error = function(e) showNotification("No se seleccionó una carpeta.", type = "message"))
  })
  observeEvent(input$ver_vector, updateTabsetPanel(session, "tipo_entrada", selected = "vector"))
  observeEvent(input$seleccionar_sentera, {
    updateRadioButtons(session, "tipo", selected = "sentera")
    updateTabsetPanel(session, "secciones", selected = "archivos")
  })
  observeEvent(input$seleccionar_phantom, {
    updateRadioButtons(session, "tipo", selected = "phantom")
    updateTabsetPanel(session, "secciones", selected = "archivos")
  })
  paginas <- c("inicio", "archivos", "configuracion", "proceso", "resultados", "exploracion", "ayuda")
  for (pagina in paginas) local({
    destino <- pagina
    observeEvent(input[[paste0("nav_", destino)]], updateTabsetPanel(session, "secciones", selected = destino))
  })
  botones <- c(comenzar = "archivos", ir_archivos = "archivos", ir_configuracion = "configuracion",
    ir_resultados = "resultados", cambiar_analisis = "archivos", continuar_config = "configuracion", revisar_proceso = "proceso", consultar_resultados = "resultados")
  for (boton in names(botones)) local({
    clave <- boton; destino <- botones[[boton]]
    observeEvent(input[[clave]], updateTabsetPanel(session, "secciones", selected = destino))
  })
  observeEvent(input$secciones, session$sendCustomMessage("menuActivo", input$secciones))
  observe({ session$sendCustomMessage("procesando", tarea$status() == "running") })
  observeEvent(input$procesar, {
    if (tarea$status() == "running" || exportacion_visores$status() == "running") {
      showNotification("Ya hay una tarea en curso. Espera a que termine para iniciar otro análisis.", type = "message"); return()
    }
    tryCatch({
      es_phantom <- input$tipo == "phantom"
      parcelas <- parcelas_seleccionadas()
      p <- list(tipo = input$tipo,
        rgb = normalizar_ruta(if (es_phantom) input$rgb_phantom else if (identical(input$origen_sentera, "multibanda")) input$multibanda else input$rgb),
        origen_sentera = input$origen_sentera, mapas_bandas = ortomosaicos$mapas(), datos_ejemplo = isTRUE(ejemplos$es_ejemplo()),
        ms = normalizar_ruta(input$ms), polys = parcelas$ruta, salida = normalizar_ruta(input$salida),
        lote = trimws(input$lote), fecha = input$fecha, id = parcelas$id, factor = input$factor,
        formulas = editor[[if (!es_phantom && identical(input$origen_sentera, "multibanda")) "sentera_multibanda" else input$tipo]],
        indices = if (es_phantom) input$indices_rgb else input$indices,
        metodo = if (es_phantom) input$metodo_phantom else input$metodo,
        umbral = if (es_phantom) input$umbral_phantom else input$umbral,
        fuente_altura = input$fuente_altura, chm = normalizar_ruta(input$chm), dsm = normalizar_ruta(input$dsm), dtm = normalizar_ruta(input$dtm),
        unidad = input$unidad_altura, altura_min = input$altura_min, altura_max = input$altura_max,
        escala = input$escala_rgb, texturas = input$texturas, ventana = input$ventana,
        crear_visores = isTRUE(input$crear_visores), assets_visores = normalizePath("www", winslash = "/", mustWork = TRUE))
      p$grupos <- grupos_elegidos_cipat(input)
      p$muestras <- if (identical(p$metodo, "Muestras")) muestras() else NULL
      p <- validar_plan_cipat(p, confirmar_altura = input$confirmar_altura)
      if (!nzchar(p$salida)) stop("Selecciona una carpeta de resultados.")
      p$salida <- crear_carpeta_resultados_cipat(p$salida, p$lote, p$fecha,
        if (es_phantom) "RGB" else "Sentera")
      p$log <- archivo_salida_cipat(p, "Proceso", "txt")
      writeLines(paste("Iniciando", nombre_modulo(p$tipo)), p$log, useBytes = TRUE)
      resultados[[p$tipo]] <- NULL
      pendiente(p); log_actual(p$log)
      tarea$invoke(p)
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })
  observe({
    estado <- tarea$status()
    if (estado == "success") {
      x <- tarea$result()
      resultados[[x$tipo]] <- x
      updateRadioButtons(session, "tipo", selected = x$tipo)
      updateTabsetPanel(session, "secciones", selected = "resultados")
    }
    if (estado == "error") tryCatch(tarea$result(), error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })
  output$modulo_actual <- renderText({ req(input$tipo); nombre_modulo(input$tipo) })
  output$estado_corto <- renderText({
    if (tarea$status() == "running") return(paste("En proceso ·", nombre_modulo(pendiente()$tipo)))
    if (!is.null(ultima())) "Resultados disponibles" else "Lista para una evaluación"
  })
  output$estado <- renderText({
    if (tarea$status() == "initial") return("Listo para comenzar")
    modulo <- nombre_modulo(pendiente()$tipo)
    switch(tarea$status(), running = paste(modulo, "en proceso"), success = paste(modulo, "terminado"), error = paste(modulo, "se detuvo · revisa el registro"))
  })
  output$registro <- renderText({
    invalidateLater(1500, session)
    ruta <- log_actual()
    if (is.null(ruta) || !file.exists(ruta)) return("Aquí aparecerá el avance del último análisis iniciado.")
    paste(tail(readLines(ruta, warn = FALSE, encoding = "UTF-8"), 14), collapse = "\n")
  })
  mostrar_media <- function(columna, decimales = 2, sufijo = "") {
    x <- ultima()
    if (is.null(x) || !columna %in% names(x$tabla)) return("—")
    v <- x$tabla[[columna]]; v <- v[is.finite(v)]
    if (!length(v)) return("Sin datos")
    paste0(formatC(mean(v), digits = decimales, format = "f"), sufijo)
  }
  output$etiqueta_cobertura <- renderText({ "Cobertura verde · máscara elegida" })
  output$etiqueta_rasgo <- renderText({ if (is.null(ultima())) "Rasgo seleccionado" else etiqueta_eda(resumen_datos_cipat(ultima())$campo_rasgo) })
  output$etiqueta_umbral <- renderText({ if (input$tipo == "phantom") "Umbral ExG" else "Umbral NDVI" })
  output$kpi_parcelas <- renderText({ if (is.null(ultima())) "—" else as.character(nrow(ultima()$tabla)) })
  output$kpi_cobertura <- renderText({ mostrar_media(if (!is.null(ultima()) && "Plant_ground_cover_image_Green_pct" %in% names(ultima()$tabla)) "Plant_ground_cover_image_Green_pct" else if (!is.null(ultima()) && "Plant_ground_cover_image_Vegetation_pct" %in% names(ultima()$tabla)) "Plant_ground_cover_image_Vegetation_pct" else if (input$tipo == "phantom") "Plant_ground_cover_image_RGB_pct" else "Plant_ground_cover_image_NDVI_pct", 1, "%") })
  output$kpi_ndvi <- renderText({ if (is.null(ultima())) "—" else mostrar_media(resumen_datos_cipat(ultima())$campo_rasgo, 3) })
  output$kpi_umbral <- renderText({ x <- ultima(); if (is.null(x)) "—" else if (identical(x$fuentes$metodo, "Muestras")) "Por muestras" else if (!is.finite(x$umbral)) "No aplica" else formatC(x$umbral, digits = 3, format = "f") })
  output$etiqueta_resultado <- renderUI({
    x <- ultima()
    div(class = "section-heading", h3("Evaluación activa"),
      if (!is.null(x)) span(class = "eyebrow", paste(x$fuentes$lote, x$fuentes$fecha)) else span(class = "eyebrow", "SIN EVALUACIÓN PROCESADA"))
  })
  output$revision_archivos <- renderUI({
    div(class = "tip", "Los requisitos se validan según los análisis seleccionados al iniciar el procesamiento.")
  })
  output$mascara_actual <- renderText({
    if (!any(grupos_elegidos_cipat(input) != "altura")) return("Altura: filtro CHM; no necesita máscara de vegetación.")
    paste("Máscara seleccionada:", if (input$tipo == "phantom") input$metodo_phantom else input$metodo)
  })
  output$requisitos_analisis <- renderText({
    g <- grupos_elegidos_cipat(input)
    if (!length(g)) return("Marca al menos un análisis.")
    reqs <- c("Polígonos de parcelas", if (any(g != "altura")) if (input$tipo == "phantom") "RGB" else "Imagen Sentera",
      if ("altura" %in% g) "CHM o DSM y DTM")
    paste("Necesitas:", paste(reqs, collapse = " + "), ". Prepara las muestras y los parámetros antes de procesar.")
  })
  output$resumen_config <- renderUI({
    g <- grupos_elegidos_cipat(input)
    fila <- function(n, v) tags$tr(tags$th(n), tags$td(v))
    tags$table(class = "table", tags$tbody(fila("Evaluación", input$lote), fila("Fecha", as.character(input$fecha)),
      fila("Análisis", paste(names(grupos_disponibles_cipat(input$tipo))[unname(grupos_disponibles_cipat(input$tipo)) %in% g], collapse = " · ")),
      fila("Máscara", if (!any(g != "altura")) "No requerida para CHM" else if (input$tipo == "phantom") input$metodo_phantom else input$metodo),
      fila("Índices", if (!"indices" %in% g) "No solicitados" else paste(if (input$tipo == "phantom") input$indices_rgb else input$indices, collapse = ", "))))
  })
  # Atributos compartidos por mapas y exploración; comparación solo en Exploración.
  datos_comparacion <- reactive({ req(ultima()); preparar_eda(ultima()) })
  eda_activa <- reactive(is.null(input$eda_origen) || input$eda_origen=="activo")
  contexto_eda <- reactive({
    if(eda_activa()) {
      x<-ultima();return(list(x=x,datos=if(is.null(x))NULL else datos_comparacion()))
    }
    tryCatch({
      base<-if(input$eda_origen=="preparada")integracion$base() else eda_unir_sesion(list(sentera=resultados$sentera,phantom=resultados$phantom))
      d<-eda_base_integrada(base)
      x<-list(tipo="integrado",tabla=d$tabla,fuentes=list(lote=base$tabla$Lote[1],fecha=base$tabla$Fecha[1]))
      list(x=x,datos=d)
    },error=function(e)list(x=list(error=conditionMessage(e)),datos=NULL))
  })
  datos_exploracion <- reactive({req(!is.null(contexto_eda()$datos));contexto_eda()$datos})
  ultima_exploracion <- reactive(contexto_eda()$x)
  datos_testigos <- reactive({ if(!is.null(contexto_eda()$datos)) contexto_eda()$datos else datos_comparacion() })
  marcas_testigos <- registrar_testigos(input,output,session,datos_testigos)
  agrupacion_exploracion <- reactive({
    k <- input$eda_grupo
    if (!is.null(k) && k %in% unname(datos_exploracion()$grupos)) k else "__ninguno__"
  })
  output$resultados_contenido <- renderUI({
    x <- ultima()
    if (is.null(x)) return(tagList(div(class = "card empty-state", icon("chart-simple"),
      h3(nombre_modulo(input$tipo)), p("Procesa una evaluación o usa Abrir Excel para consultar un resultado guardado.")), integracion_ui()))
    tagList(div(class="results-meta-strip",
      div(icon("seedling"),span("Lote"),strong(resumen_texto_cipat(x$fuentes$lote))),
      div(icon("calendar-days"),span("Fecha"),strong(resumen_texto_cipat(x$fuentes$fecha))),
      div(icon("camera"),span("Sensor"),strong(if(x$tipo=="phantom") "RGB convencional" else "Sentera Doble 4K")),
      span(class="results-ready",icon("circle-check"),"Evaluación disponible")),
      div(class="card results-workspace",
      tabsetPanel(id = "vista_resultados", selected="Resumen",
        tabPanel("Resumen", uiOutput("resumen_portada")),
        tabPanel("Mapas",
          div(class = "workspace-split",
            div(class = "tool-panel", div(class = "tool-heading", icon("map-location-dot"), "Mapa de parcelas"), uiOutput("mapa_controles")),
            div(class = "interactive-map-panel", leaflet::leafletOutput("mapa_parcelas", height = "650px"),
              div(class = "map-caption", icon("hand-pointer"), "Pulsa una parcela para consultar su ficha"))),
          uiOutput("mapa_aviso"),
          ayuda_contextual("Vista estática de capas",  div(class = "workspace-split",
          div(class = "tool-panel", div(class = "tool-heading", icon("sliders"), "Visualización"),
            selectInput("capa_mapa", "Capa", choices = names(x$capas), selected = if (x$tipo == "phantom") "ExG" else "NDVI"),
            checkboxInput("mostrar_parcelas", "Mostrar parcelas", TRUE),
            ayuda_breve("Vista previa reducida. GeoTIFF a resolución de salida. Al abrir un Excel guardado, las capas deben permanecer junto al archivo y las parcelas en su ruta original.")),
          div(class = "map-frame", plotOutput("mapa_general", height = "620px"))))),
        tabPanel("Productos ráster",
          div(class = "workspace-split",
            div(class = "tool-panel", div(class = "tool-heading", icon("layer-group"), "Productos de la evaluación"), uiOutput("producto_controles")),
            div(class = "interactive-map-panel", leaflet::leafletOutput("producto_mapa", height = "650px"),
              div(class = "map-caption", icon("file-image"), textOutput("producto_archivo")))),
          uiOutput("producto_aviso"),
          tarjeta("Exportar mapas interactivos", "Guarda un visor HTML por producto, un catálogo y todos los recursos necesarios en un ZIP.",
            div(class = "bottom-actions", actionButton("generar_visores", "Generar visores HTML", icon = icon("globe"), class = "btn-primary"), uiOutput("descarga_visores_ui")),
            div(class = "panel-text", textOutput("estado_visores")),
            ayuda_breve("Descomprime el ZIP y abre index.html. Conserva la carpeta recursos junto a los HTML. Las imágenes incluidas son vistas reducidas; el ZIP no incluye los TIFF originales."))),
        tabPanel("Tabla HTP", ayuda_contextual("Consultar la tabla", "Busca parcelas y desplázate horizontalmente para consultar las variables."), DT::DTOutput("tabla")),
        tabPanel("Calidad de datos", uiOutput("resumen_criterio"), DT::DTOutput("resumen_revision_tabla")),
        tabPanel("Archivos", uiOutput("resumen_archivos"), integracion_ui()))),
      ayuda_contextual("Ubicación de resultados", div(class = "path-note", basename(x$salida))))
  })
  output$tabla <- DT::renderDT({
    req(ultima())
    DT::datatable(ultima()$tabla, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10,
      language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ filas", info = "_START_ a _END_ de _TOTAL_ filas", paginate = list(previous = "Anterior", `next` = "Siguiente"))))
  })
  output$descargar <- downloadHandler(filename = function() { req(ultima()); basename(ultima()$excel) },
    content = function(file) { req(ultima()); file.copy(ultima()$excel, file, overwrite = TRUE) })
  output$mapa_general <- renderPlot({
    x <- ultima(); req(x)
    validate(need(length(x$capas), "No se encontraron TIFF para esta evaluación. El análisis exploratorio sigue disponible."))
    req(input$capa_mapa)
    capa <- input$capa_mapa
    req(capa %in% names(x$capas))
    r <- terra::rast(x$capas[[capa]])
    polys <- if (nzchar(x$fuentes$polys) && file.exists(x$fuentes$polys))
      sf::st_transform(sf::st_read(x$fuentes$polys, quiet = TRUE), terra::crs(r)) else NULL
    if (capa == "RGB") {
      r <- r[[bandas_rgb_resultado_cipat(x, terra::nlyr(r))]]
      # La vista también conserva toda la extensión disponible.
    }
    es_mask <- startsWith(capa, "Máscara")
    r <- terra::aggregate(r, fact = max(1, ceiling(max(nrow(r), ncol(r)) / 800)), fun = if (es_mask) "modal" else "mean", na.rm = TRUE)
    graphics::par(mar = c(3, 3, 3, 4), bg = "white", col.axis = "#718079", col.lab = "#718079", col.main = "#17332b")
    if (capa == "RGB") terra::plotRGB(r, stretch = "lin", axes = TRUE, main = "Ortomosaico RGB")
    else if (es_mask) terra::plot(r, main = paste(capa, "· 0 / 1"), col = c("#cdbd9b", "#267543"), range = c(0, 1))
    else if (capa == "NDVI") terra::plot(r, main = capa, col = hcl.colors(80, "RdYlGn"), range = c(-1, 1))
    else terra::plot(r, main = capa, col = hcl.colors(80, "Viridis"))
    if (isTRUE(input$mostrar_parcelas) && !is.null(polys)) graphics::plot(sf::st_geometry(polys), add = TRUE, border = "#ffffff", lwd = .8)
    if (isTRUE(input$mostrar_parcelas) && is.null(polys)) graphics::mtext("Parcelas no disponibles en la ruta registrada.", side = 1, line = 2, cex = .7)
  })
  registrar_entradas_cipat(input, output, session, parcelas_seleccionadas, ortomosaicos$mapas)
  registrar_productos_cipat(input, output, session, ultima, datos_comparacion)
  registrar_mapa_cipat(input, output, session, ultima, datos_comparacion)
  registrar_exploracion(input, output, session, ultima_exploracion, datos_exploracion, agrupacion_exploracion, marcas_testigos)

}
shinyApp(ui, server)
