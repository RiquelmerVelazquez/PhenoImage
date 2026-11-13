# Reabre las salidas Excel de esta app sin recalcular imágenes.
leer_evaluacion_cipat <- function(ruta) {
  ruta <- normalizePath(ruta, winslash = "/", mustWork = TRUE)
  hojas <- openxlsx::getSheetNames(ruta)
  if (!"Parametros" %in% hojas || !any(c("HTP", "HTP_Phantom") %in% hojas)) {
    stop("Selecciona un Excel HTP generado por esta aplicación, con hojas HTP/HTP_Phantom y Parametros.")
  }
  tipo <- if ("HTP_Phantom" %in% hojas) "phantom" else "sentera"
  tabla <- openxlsx::read.xlsx(ruta, sheet = if (tipo == "phantom") "HTP_Phantom" else "HTP", check.names = FALSE)
  tabla <- normalizar_resultados_cipat(normalizar_columnas_ndre_cipat(tabla), tipo)
  meta <- openxlsx::read.xlsx(ruta, sheet = "Parametros", check.names = FALSE)
  if (!all(c("Parametro", "Valor") %in% names(meta))) stop("La hoja Parametros no tiene el formato esperado.")
  if (!"id" %in% names(tabla) || !nrow(tabla)) stop("El Excel necesita una tabla HTP con parcelas y campo id.")
  tabla$id <- as.character(tabla$id)
  if (anyNA(tabla$id) || anyDuplicated(tabla$id) || any(!nzchar(trimws(tabla$id)))) stop("Revisa los ID del Excel: deben ser únicos y no estar vacíos.")
  # Excel puede devolver como lógico una métrica sin ningún valor disponible.
  metricas <- setdiff(names(tabla)[!startsWith(names(tabla), "Parcela_")], c("id", "Lote", "Fecha"))
  for (k in metricas) if (is.logical(tabla[[k]]) && all(is.na(tabla[[k]]))) tabla[[k]] <- as.numeric(tabla[[k]])
  obtener <- function(k, defecto = "") {
    i <- match(k, as.character(meta$Parametro))
    if (is.na(i) || is.na(meta$Valor[i])) defecto else as.character(meta$Valor[i])
  }
  carpeta <- dirname(ruta)
  fuentes <- list(tipo = tipo, lote = obtener("Lote", basename(carpeta)), fecha = obtener("Fecha"),
    polys = obtener("Parcelas"), id = obtener("Campo_ID", "id"), rgb = obtener("RGB"), salida = carpeta)
  fuentes$origen_sentera <- obtener("Origen_Sentera", "metashape")
  clave <- clave_rgb_cipat(fuentes)
  fuentes$mapas_bandas <- list()
  fuentes$mapas_bandas[[clave]] <- if (identical(clave, "multibanda")) {
    texto <- obtener("Bandas_multiespectral")
    if (startsWith(texto, "R=")) leer_texto_bandas_cipat(texto, clave) else bandas_predeterminadas_cipat(clave)
  } else leer_texto_bandas_cipat(obtener("Bandas_RGB"), clave)
  if (identical(tipo, "sentera") && !identical(clave, "multibanda"))
    fuentes$mapas_bandas$ms <- leer_texto_bandas_cipat(obtener("Bandas_multiespectral"), "ms")
  candidatos <- if (tipo == "phantom") c("ExG" = "ExG.tif", "Máscara RGB" = "Mascara_ExG_RGB.tif",
    "CHM original (cm)" = "CHM_cm.tif", "CHM filtrado (cm)" = "CHM_filtrado_cm.tif", "Máscara CHM" = "Mascara_CHM.tif",
    "Textura Entropy" = "Entropy_RGB.tif", "Textura Contrast" = "Contrast_RGB.tif", "Textura Homogeneity" = "Homogeneity_RGB.tif")
    else c("NDVI" = "NDVI.tif", "Máscara NDVI" = "NDVI_Mascara_Binaria.tif",
      "NDRE" = "NDRE.tif", "GNDVI" = "GNDVI.tif", "CIRE" = "CIRE.tif", "CVI" = "CVI.tif", "NDWI" = "NDWI.tif", "OSAVI" = "OSAVI.tif", "SAVI" = "SAVI.tif", "EVI" = "EVI.tif")
  if (tipo == "sentera") {
    indices <- unique(c("NDVI", sub("_mean$", "", names(tabla)[grepl("_mean$", names(tabla)) &
      !grepl("^(Sentera_|Red_|Green_|Blue_|RE_|NIR_|Plant_ground_cover_image_NDVI_)", names(tabla))])))
    candidatos <- c(stats::setNames(paste0(indices, ".tif"), indices),
      "Máscara NDVI" = "NDVI_Mascara_Binaria.tif", "NDVI · vegetación" = "NDVI_Mascara_Vegetacion.tif")
    candidatos <- vapply(candidatos, function(archivo) {
      indice <- tools::file_path_sans_ext(archivo)
      opciones <- c(nombre_tif_sentera(indice, fuentes$lote, fuentes$fecha),
        paste0(indice, "_", fuentes$lote, "_", fuentes$fecha, ".tif"), archivo)
      if (identical(indice, "NDRE")) opciones <- c(opciones,
        nombre_tif_sentera("NDRE1", fuentes$lote, fuentes$fecha), "NDRE1.tif")
      existentes <- opciones[file.exists(file.path(carpeta, opciones))]
      if (length(existentes)) existentes[1] else archivo
    }, character(1))
  }
  if (tipo == "phantom" && "Formulas" %in% hojas) {
    formulas <- openxlsx::read.xlsx(ruta, sheet = "Formulas", check.names = FALSE)
    if ("Indice" %in% names(formulas)) {
      indices_rgb <- as.character(formulas$Indice)
      indices_rgb <- unique(indices_rgb[!is.na(indices_rgb) & grepl("^[A-Za-z][A-Za-z0-9_]{0,39}$", indices_rgb)])
      adicionales <- setdiff(indices_rgb, names(candidatos))
      candidatos <- c(candidatos, stats::setNames(paste0(adicionales, ".tif"), adicionales))
    }
  }
  if (tipo == "phantom") {
    adicionales <- c("HSB_Hue", "HSB_Saturation", "HSB_Brightness", "DGCI", "Lab_L", "Lab_a", "Lab_b")
    candidatos <- c(candidatos, stats::setNames(paste0(adicionales, ".tif"), adicionales))
    candidatos <- vapply(candidatos, function(archivo) {
      opciones <- c(nombre_archivo_cipat(tools::file_path_sans_ext(archivo), fuentes$lote, fuentes$fecha, "RGB", "tif"), archivo)
      existentes <- opciones[file.exists(file.path(carpeta, opciones))]
      if (length(existentes)) existentes[1] else archivo
    }, character(1))
  }
  capas <- as.list(stats::setNames(file.path(carpeta, unname(candidatos)), names(candidatos)))
  capas <- capas[vapply(capas, file.exists, logical(1))]
  if (nzchar(fuentes$rgb) && file.exists(fuentes$rgb)) capas <- c(list(RGB = fuentes$rgb), capas)
  umbral <- suppressWarnings(as.numeric(obtener(if (tipo == "phantom") "Umbral_ExG" else "Umbral", NA_character_)))
  list(tabla = tabla, excel = ruta, umbral = umbral, salida = carpeta, tipo = tipo, fuentes = fuentes, capas = capas)
}
