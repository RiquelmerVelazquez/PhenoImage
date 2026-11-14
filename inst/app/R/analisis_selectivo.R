# E28: planificación y ejecución según los productos solicitados.
grupos_disponibles_cipat <- function(tipo) {
  if (identical(tipo, "phantom")) c("Bandas RGB" = "bandas", "Índices RGB" = "indices",
    "Cobertura vegetal" = "cobertura", "Altura del dosel · CHM" = "altura",
    "Color · HSB, DGCI y Lab" = "color", "Texturas GLCM" = "texturas")
  else c("Bandas Sentera" = "bandas", "Índices espectrales" = "indices", "Cobertura vegetal" = "cobertura")
}
grupos_elegidos_cipat <- function(input) {
  tipo <- input$tipo
  if (is.null(input$modo_analisis) || identical(input$modo_analisis, "completo")) unname(grupos_disponibles_cipat(tipo))
  else input[[paste0("grupos_", tipo)]]
}
necesita_imagen_cipat <- function(p) any(p$grupos %in% c("bandas", "indices", "cobertura", "color", "texturas"))
necesita_espectral_cipat <- function(p) identical(p$tipo, "sentera") &&
  (any(p$grupos %in% c("bandas", "indices")) || (necesita_imagen_cipat(p) && !identical(p$metodo, "Muestras")))

validar_plan_cipat <- function(p, confirmar_altura = TRUE) {
  if (!length(p$grupos) || any(!p$grupos %in% unname(grupos_disponibles_cipat(p$tipo)))) stop("Selecciona al menos un análisis disponible.")
  p$grupos <- unique(p$grupos); p$texturas <- "texturas" %in% p$grupos
  p$indices <- if ("indices" %in% p$grupos) unique(p$indices) else character()
  if ("indices" %in% p$grupos && !length(p$indices)) stop("Marca al menos un índice, o desmarca el grupo Índices.")
  if (!length(p$polys) || !nzchar(p$polys) || !file.exists(p$polys)) stop("Carga los polígonos de parcelas.")
  if (!nzchar(p$id) || !nzchar(p$lote) || is.null(p$fecha) || is.na(p$fecha)) stop("Completa lote, fecha e ID de parcelas.")
  if (!p$metodo %in% c("Otsu", "Manual", "Muestras")) stop("Selecciona una máscara de vegetación válida.")
  requeridos <- "polys"
  if (necesita_imagen_cipat(p)) requeridos <- c(requeridos, "rgb")
  if (necesita_espectral_cipat(p) && !identical(p$origen_sentera, "multibanda")) requeridos <- c(requeridos, "ms")
  if ("altura" %in% p$grupos) {
    requeridos <- c(requeridos, if (p$fuente_altura == "CHM") "chm" else c("dsm", "dtm"))
    if (!isTRUE(confirmar_altura)) stop("Verifica la unidad y referencia de altura y marca la casilla de confirmación.")
    if (!p$unidad %in% c("cm", "m") || !all(is.finite(c(p$altura_min, p$altura_max))) || p$altura_min < 0 || p$altura_max <= p$altura_min) stop("Revisa la unidad y el filtro de altura.")
  }
  for (k in requeridos) if (length(p[[k]]) != 1L || !nzchar(p[[k]]) || !file.exists(p[[k]])) stop("Carga un archivo válido para: ", k)
  if (necesita_imagen_cipat(p)) {
    r <- terra::rast(p$rgb)
    mapa_bandas_cipat(p, clave_rgb_cipat(p), terra::nlyr(r))
    if (p$metodo == "Muestras") {
      if (is.null(p$muestras)) stop("Prepara las muestras, genera la vista previa y pulsa Aplicar máscara antes de procesar.")
      validar_modelo_muestras_cipat(p)
    } else {
      p$muestras <- NULL
      if (p$metodo == "Manual" && (length(p$umbral) != 1L || !is.finite(p$umbral))) stop("Indica un umbral manual válido.")
      if (p$tipo == "sentera" && p$metodo == "Manual" && abs(p$umbral) > 1) stop("NDVI requiere un umbral entre -1 y 1.")
    }
  } else p$muestras <- NULL
  if (p$tipo == "sentera") {
    p <- validar_entrada_sentera(p)
    if (!is.finite(p$factor) || p$factor <= 0) stop("El factor Sentera debe ser positivo.")
    if ("ms" %in% requeridos) mapa_bandas_cipat(p, "ms", terra::nlyr(terra::rast(p$ms)))
  }
  if (p$tipo == "phantom" && necesita_imagen_cipat(p) && (length(p$escala) != 1L || !is.finite(p$escala) || p$escala <= 0)) stop("Indica una escala RGB positiva.")
  auxiliares <- if (necesita_imagen_cipat(p) && p$metodo != "Muestras") if (p$tipo == "phantom") "ExG" else "NDVI" else character()
  formulas <- unique(c(p$indices, auxiliares))
  if (any(!formulas %in% names(p$formulas)) || any(vapply(p$formulas[formulas], is.null, logical(1)))) stop("Revisa las fórmulas de los índices solicitados y de la máscara.")
  p$formulas <- p$formulas[formulas]
  if (isTRUE(p$texturas)) {
    faltan <- c("raster", "glcm")[!vapply(c("raster", "glcm"), requireNamespace, logical(1), quietly = TRUE)]
    if (length(faltan)) stop("Para texturas instala: ", paste(faltan, collapse = ", "), ". O selecciona Personalizar y desmarca Texturas.")
    if (!is.finite(p$ventana) || p$ventana %% 2 != 1 || p$ventana < 3 || p$ventana > 15) stop("La ventana GLCM debe ser impar entre 3 y 15.")
  } else p$ventana <- 5
  p
}

comprobar_metros_e28 <- function(r) {
  c <- sf::st_crs(terra::crs(r))
  if (is.na(c) || isTRUE(sf::st_is_longlat(c)) || !c$units_gdal %in% c("metre", "meter", "m")) stop("Las imágenes deben tener CRS proyectado en metros, por ejemplo UTM.")
}

procesar_seleccion_cipat <- function(p) {
  registrar <- function(s) cat(format(Sys.time(), "%H:%M:%S"), " | ", s, "\n", sep = "", file = p$log, append = TRUE)
  temporales <- tempfile("pheno_e28_"); dir.create(temporales)
  op <- terra::terraOptions(print = FALSE)
  on.exit({ do.call(terra::terraOptions, op[intersect(names(op), c("tempdir", "memfrac", "memmax", "memmin", "todisk", "progress"))]); unlink(temporales, recursive = TRUE) }, add = TRUE)
  terra::terraOptions(tempdir = temporales, memfrac = .4, progress = 0)
  guardar <- function(r, n) {
    f <- archivo_salida_cipat(p, n, "tif")
    terra::writeRaster(r, f, overwrite = FALSE, datatype = "FLT4S", gdal = c("COMPRESS=LZW", "BIGTIFF=IF_SAFER")); terra::rast(f)
  }
  saveRDS(p, archivo_salida_cipat(p, "Entrada", "rds"))
  imagen <- necesita_imagen_cipat(p); espectral <- necesita_espectral_cipat(p)
  referencia <- if (imagen) p$rgb else if (p$fuente_altura == "CHM") p$chm else p$dsm
  ref <- terra::rast(referencia); comprobar_metros_e28(ref)
  polys <- sf::st_read(p$polys, quiet = TRUE)
  if (!nrow(polys) || is.na(sf::st_crs(polys))) stop("Revisa el archivo de parcelas y su CRS.")
  polys <- sf::st_transform(sf::st_make_valid(polys), terra::crs(ref))
  if (any(sf::st_is_empty(polys)) || !all(as.character(sf::st_geometry_type(polys)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("Las parcelas deben ser polígonos válidos.")
  if (!p$id %in% names(polys)) stop("No existe el campo ID de parcelas.")
  ids <- as.character(polys[[p$id]])
  if (anyNA(ids) || anyDuplicated(ids) || any(!nzchar(trimws(ids)))) stop("Los ID de parcelas deben ser únicos y no vacíos.")
  tabla <- data.frame(id = ids, Lote = p$lote, Fecha = as.character(p$fecha), stringsAsFactors = FALSE)
  attrs <- sf::st_drop_geometry(polys)[, setdiff(names(polys), c(p$id, attr(polys, "sf_column"))), drop = FALSE]
  if (ncol(attrs)) { names(attrs) <- paste0("Parcela_", names(attrs)); tabla <- cbind(tabla, attrs) }
  capas <- list(); umbral <- NA_real_; rutas_indices <- list(); banda_texto <- "No requerido"
  meta <- data.frame(Parametro = c("Tipo", "Lote", "Fecha", "RGB", "Multiespectral", "Parcelas", "Campo_ID", "Origen_Sentera", "Analisis_seleccionados", "Metodo", "Metodo_ExG", "Mascara_estadisticas", "Ambito_mascara", "Unidad_salida"),
    Valor = c(if (p$tipo == "phantom") "Phantom" else "Sentera", p$lote, as.character(p$fecha), if (imagen) p$rgb else "", if (espectral) p$ms else "", p$polys, p$id,
      p$origen_sentera, paste(p$grupos, collapse = "; "), if (imagen) p$metodo else "No requerida", if (imagen) p$metodo else "No requerida",
      if (imagen) if (p$metodo == "Muestras") "Muestras RGB" else if (p$tipo == "phantom") "ExG" else "NDVI" else "No requerida",
      "Bandas, índices, color y texturas usan solo la máscara VERDE; altura usa su filtro CHM independiente", if ("altura" %in% p$grupos) "cm" else "No aplica (sin altura)"), stringsAsFactors = FALSE)
  if (imagen) {
    registrar("Preparando canales de la imagen")
    mapa <- mapa_bandas_cipat(p, clave_rgb_cipat(p), terra::nlyr(ref))
    rgb <- ref[[unname(mapa[c("R", "G", "B")])]]; names(rgb) <- c("Red", "Green", "Blue")
    banda_texto <- texto_bandas_cipat(mapa)
    capas$RGB <- p$rgb
    bandas <- rgb
    multibanda <- p$tipo == "sentera" && identical(p$origen_sentera, "multibanda")
    if (espectral && multibanda) {
      if (terra::nlyr(ref) != 5L || any(terra::datatype(ref) == "INT1U")) stop("Para análisis espectrales carga la multibanda Sentera numérica de cinco bandas corregidas.")
      bandas <- ref[[unname(mapa)]]; names(bandas) <- c("Red", "Green", "Blue", "RE", "NIR")
    } else if (espectral) {
      ms <- terra::rast(p$ms); if (!nzchar(terra::crs(ms))) stop("El ortomosaico RE/NIR necesita CRS.")
      mm <- mapa_bandas_cipat(p, "ms", terra::nlyr(ms))
      Red <- -.034 * rgb[[3]] - .110 * rgb[[2]] + 1.150 * rgb[[1]]
      Green <- -.199 * rgb[[3]] + 1.420 * rgb[[2]] - .329 * rgb[[1]]
      Blue <- 1.377 * rgb[[3]] - .182 * rgb[[2]] - .061 * rgb[[1]]
      alinear <- function(x) if (terra::same.crs(x, Red)) terra::resample(x, Red, method = "bilinear") else terra::project(x, Red, method = "bilinear")
      RE <- alinear(-.956 * ms[[mm[["NIR"]]]] + ms[[mm[["RE"]]]])
      NIR <- alinear(2.426 * ms[[mm[["NIR"]]]] - .341 * ms[[mm[["RE"]]]])
      bandas <- c(Red, Green, Blue, RE, NIR); names(bandas) <- c("Red", "Green", "Blue", "RE", "NIR")
      bandas <- guardar(bandas, "Multibanda_corregida")
    }
    if (multibanda) bandas <- terra::ifel(bandas == -10000, NA, bandas)
    editor <- list(R = rgb[[1]], G = rgb[[2]], B = rgb[[3]])
    for (k in c("rn", "gn", "bn")) editor[[k]] <- editor[[c(rn = "R", gn = "G", bn = "B")[[k]]]] / p$escala
    auxiliar <- if (p$metodo == "Muestras") character() else if (p$tipo == "phantom") "ExG" else "NDVI"
    for (n in unique(c(p$indices, auxiliar))) {
      registrar(paste("Calculando", n))
      r <- if (p$tipo == "phantom") guardar(ejecutar_formula_cipat(p$formulas[[n]], editor), n)
      else calcular_indice_archivo_sentera(n, bandas, p$factor, p$formulas, archivo_salida_cipat(p, n, "tif"), limpiar_entrada = multibanda)
      rutas_indices[[n]] <- terra::sources(r)[1]
      if (n %in% p$indices) capas[[n]] <- rutas_indices[[n]]
    }
    registrar(paste("Preparando máscara única:", p$metodo))
    if (p$metodo == "Muestras") {
      validar_modelo_muestras_cipat(p)
      modelo <- p$muestras$modelo
      clases <- escribir_bloques_sentera(rgb, archivo_salida_cipat(p, "Mascara_Clases_Muestras", "tif"),
        function(v) predecir_muestras_cipat(v, modelo), nombres = "Clase", max_celdas = 200000L)
      mask <- guardar(terra::ifel(clases == 1, 1, 0), "Mascara_Vegetacion")
      capas[["Máscara de clases por muestras"]] <- terra::sources(clases)[1]
      datos_muestras <- p$muestras$datos
      datos_muestras$incluir_seco <- any(modelo$clases == 2L)
      utils::write.csv(datos_muestras, archivo_salida_cipat(p, "Muestras_RGB", "csv"), row.names = FALSE)
      meta <- rbind(meta, data.frame(Parametro = c("Muestras_clases", "Material_seco_evaluado", "Codigos_clases", "Cobertura_total_definicion"),
        Valor = c(if (any(modelo$clases == 2L)) "Verde; seco; suelo" else "Verde; suelo", any(modelo$clases == 2L),
          if (any(modelo$clases == 2L)) "0=suelo; 1=verde; 2=seco" else "0=suelo; 1=verde; seco=no evaluado",
          if (any(modelo$clases == 2L)) "Verde + seco" else "Igual a verde; seco no evaluado")))
      saveRDS(p$muestras, archivo_salida_cipat(p, "Modelo_Muestras", "rds"))
      meta <- rbind(meta, data.frame(Parametro = c("Muestras_zonas", "Muestras_pixeles", "Muestras_fuente", "Muestras_metodo"),
        Valor = c(length(unique(p$muestras$datos$zona)), nrow(p$muestras$datos), p$muestras$firma, modelo$metodo)))
    } else {
      ind <- terra::rast(rutas_indices[[auxiliar]])
      if (p$metodo == "Manual") umbral <- p$umbral else {
        muestra <- muestra_otsu_cipat(ind, polys, margen = if (p$tipo == "phantom") max(terra::res(rgb)) * (p$ventana + 2) else 0)
        vals <- as.numeric(muestra[[1]]); vals <- vals[is.finite(vals)]
        if (!length(vals)) stop("No hay valores válidos para calcular Otsu.")
        if (p$tipo == "phantom") {
          rango <- attr(muestra, "rango_muestreo"); lo <- rango[1]; hi <- rango[2]
        } else { lo <- -1; hi <- 1; if (diff(range(vals)) < 1e-8) stop("NDVI constante: usa umbral manual o muestras.") }
        if (!all(is.finite(c(lo, hi))) || hi <= lo) stop("Índice constante: usa umbral manual o muestras.")
        u <- autothresholdr::auto_thresh(as.integer(pmax(0, pmin(255, 255 * (vals - lo) / (hi - lo)))), "Otsu")
        umbral <- lo + as.numeric(u) / 255 * (hi - lo)
      }
      mask <- guardar(if (p$tipo == "phantom") terra::ifel(ind > umbral, 1, 0) else terra::ifel(ind >= umbral, 1, 0), "Mascara_Vegetacion")
    }
    capas[["Máscara de vegetación"]] <- terra::sources(mask)[1]
    extraer <- function(r, nombre, color = FALSE, textura = FALSE) {
      q <- resumen_raster(terra::ifel(mask == 1, r, NA), polys)
      if (color) q <- ajustar_estadisticas_color_cipat(q, nombre)
      if (textura) q$sum <- NULL
      names(q) <- paste(nombre, names(q), sep = "_"); q
    }
    if ("bandas" %in% p$grupos) for (n in names(bandas)) {
      registrar(paste("Estadísticas de banda", n)); tabla <- cbind(tabla, extraer(bandas[[n]], n))
      capas[[n]] <- terra::sources(guardar(bandas[[n]], n))[1]
    }
    for (n in p$indices) { registrar(paste("Estadísticas de", n)); tabla <- cbind(tabla, extraer(terra::rast(rutas_indices[[n]]), n)) }
    if ("color" %in% p$grupos) {
      registrar("Calculando HSB, DGCI y Lab con pliman")
      color <- calcular_color_archivo_cipat(rgb, p$escala, file.path(temporales, "color.tif"), registrar)
      for (n in names(color)) {
        r <- guardar(color[[n]], n); tabla <- cbind(tabla, extraer(r, n, color = TRUE)); capas[[n]] <- terra::sources(r)[1]
      }
    }
    if ("texturas" %in% p$grupos) {
      registrar("Calculando texturas GLCM con la máscara elegida")
      raster::rasterOptions(tmpdir = temporales, maxmemory = 2e8)
      green <- guardar(terra::ifel(mask == 1, rgb[[2]], NA), "Green_vegetacion")
      g <- raster::raster(terra::sources(green)[1]); lo <- raster::cellStats(g, "min"); hi <- raster::cellStats(g, "max")
      etiquetas <- c("Entropy", "Contrast", "Homogeneity")
      if (is.finite(lo) && is.finite(hi) && hi > lo) {
        tx <- terra::rast(glcm::glcm(g, n_grey = 32, window = c(p$ventana, p$ventana),
          shift = list(c(1, 0), c(1, 1), c(0, 1), c(-1, 1)), min_x = lo, max_x = hi,
          statistics = c("entropy", "contrast", "homogeneity"), na_opt = "any"))
      } else tx <- c(green * NA_real_, green * NA_real_, green * NA_real_)
      for (i in 1:3) {
        r <- guardar(tx[[i]], paste0(etiquetas[i], "_RGB")); tabla <- cbind(tabla, extraer(r, paste0("RGB_", etiquetas[i]), textura = TRUE)); capas[[paste("Textura", etiquetas[i])]] <- terra::sources(r)[1]
      }
      meta <- rbind(meta, data.frame(Parametro = c("GLCM_min", "GLCM_max", "GLCM_niveles", "Ventana_GLCM", "GLCM_direcciones", "GLCM_NA"), Valor = c(lo, hi, 32, p$ventana, "0,45,90,135; distancia 1; promedio", "any")))
    }
    if ("cobertura" %in% p$grupos) {
      registrar("Calculando cobertura por clases con un único denominador válido")
      if (p$metodo == "Muestras") {
        q <- resumen_clases_cobertura_cipat(clases, polys, any(p$muestras$modelo$clases == 2L))
      } else {
        q <- resumen_cobertura(mask, polys)
        q$veg_area_m2 <- q$veg_pix * prod(terra::res(mask)); q$polygon_area_m2 <- as.numeric(sf::st_area(polys))
        q$valid_area_pct <- 100 * q$total_pix * prod(terra::res(mask)) / q$polygon_area_m2
        q <- q[c("pct", "veg_area_m2", "polygon_area_m2", "valid_area_pct")]
        names(q) <- paste0("Plant_ground_cover_image_Green_", names(q))
      }
      tabla <- cbind(tabla, q)
    }
  }
  if ("altura" %in% p$grupos) {
    registrar("Calculando altura del dosel")
    if (p$fuente_altura == "CHM") CHM <- terra::rast(p$chm) else {
      DSM <- terra::rast(p$dsm); DTM <- terra::rast(p$dtm); comprobar_metros_e28(DSM); comprobar_metros_e28(DTM)
      if (terra::nlyr(DSM) != 1L || terra::nlyr(DTM) != 1L) stop("DSM y DTM deben tener una sola banda.")
      DTM <- if (terra::same.crs(DTM, DSM)) terra::resample(DTM, DSM, method = "bilinear") else terra::project(DTM, DSM, method = "bilinear")
      CHM <- DSM - DTM
    }
    comprobar_metros_e28(CHM); if (terra::nlyr(CHM) != 1L) stop("El CHM debe tener una sola banda.")
    if (p$unidad == "m") CHM <- CHM * 100
    CHM <- guardar(CHM, "CHM_cm"); filtrado <- guardar(terra::ifel(CHM >= p$altura_min & CHM <= p$altura_max, CHM, NA), "CHM_filtrado_cm")
    q <- resumen_chm(filtrado, sf::st_transform(polys, terra::crs(CHM))); names(q) <- paste0("Canopy_height_image_", names(q)); tabla <- cbind(tabla, q)
    capas[["CHM original (cm)"]] <- terra::sources(CHM)[1]; capas[["CHM filtrado (cm)"]] <- terra::sources(filtrado)[1]
    meta <- rbind(meta, data.frame(Parametro = c("CHM", "DSM", "DTM", "Unidad_entrada", "Filtro_altura_cm"), Valor = c(p$chm, p$dsm, p$dtm, p$unidad, paste(p$altura_min, p$altura_max, sep = " a "))))
  }
  meta <- rbind(meta, data.frame(Parametro = c("Bandas_RGB", "Bandas_multiespectral", "Factor", "Umbral", "Umbral_ExG", "Escala_RGB", "GLCM", "Color", "Indices_seleccionados", "Cobertura"),
    Valor = c(banda_texto, if (espectral && !multibanda) texto_bandas_cipat(mm) else if (espectral) banda_texto else "No requerido", p$factor, umbral, umbral, p$escala, "texturas" %in% p$grupos,
      if ("color" %in% p$grupos) paste("pliman", utils::packageVersion("pliman")) else "No solicitado", paste(p$indices, collapse = "; "), "Ponderada por fracción de píxel; denominador: área válida en la máscara elegida")))
  meta <- rbind(meta, data.frame(Parametro = "Estadisticas", Valor = "Bandas e índices: estadísticos sin ponderación por fracción de píxel; sd muestral; CV fracción. Cobertura: ponderada por fracción de píxel."))
  tabla <- normalizar_resultados_cipat(tabla, p$tipo)
  utils::write.csv(tabla, archivo_salida_cipat(p, "HTP", "csv"), row.names = FALSE)
  saveRDS(list(tabla = tabla, metadatos = meta, parametros = p, umbral = umbral), archivo_salida_cipat(p, "HTP_Respaldo", "rds"))
  wb <- openxlsx::createWorkbook(); hoja <- if (p$tipo == "phantom") "HTP_Phantom" else "HTP"
  openxlsx::addWorksheet(wb, hoja); openxlsx::writeData(wb, hoja, tabla); openxlsx::freezePane(wb, hoja, firstRow = TRUE, firstCol = TRUE)
  openxlsx::addWorksheet(wb, "Parametros"); openxlsx::writeData(wb, "Parametros", meta)
  if (length(p$formulas)) registrar_formulas_cipat(wb, p)
  if ("color" %in% p$grupos) documentar_rasgos_color_cipat(wb, p)
  # Catálogo explícito de productos: permite reabrir selecciones sin inventar capas.
  openxlsx::addWorksheet(wb, "Productos"); openxlsx::writeData(wb, "Productos", data.frame(Producto = names(capas), Archivo = unlist(capas, use.names = FALSE)))
  excel <- archivo_salida_cipat(p, "HTP", "xlsx"); openxlsx::saveWorkbook(wb, excel, overwrite = FALSE)
  registrar("Análisis seleccionados terminados")
  list(tabla = tabla, excel = excel, umbral = umbral, salida = p$salida, tipo = p$tipo, fuentes = p, capas = capas)
}
