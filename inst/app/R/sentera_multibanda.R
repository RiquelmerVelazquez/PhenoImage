# Entrada Multibanda Sentera: valores de bandas corregidas, sin renormalización automática.
validar_entrada_sentera <- function(p) {
  if (is.null(p$origen_sentera)) p$origen_sentera <- "metashape"
  if (!p$origen_sentera %in% c("multibanda", "metashape")) stop("Selecciona un origen Sentera válido.")
  if (identical(p$origen_sentera, "multibanda")) {
    p$factor <- 1
    p$ms <- ""
  }
  p
}

preparar_bandas_sentera <- function(b, filename = NULL) {
  if (terra::nlyr(b) != 5L) stop("Multibanda Sentera necesita exactamente cinco bandas: rojo, verde, azul, red edge y NIR.")
  if (any(terra::datatype(b) == "INT1U")) stop("Carga las bandas numéricas corregidas en coma flotante; no un mosaico de visualización de 8 bits.")
  if (!is.null(filename)) {
    limpiar <- function(v) {
      v[v == -10000 & !is.na(v)] <- NA_real_
      valido <- rowSums(!is.finite(v)) == 0L
      v[!valido, ] <- NA_real_
      v
    }
    return(escribir_bloques_sentera(b, filename, limpiar, nbandas = 5L,
      nombres = c("Red", "Green", "Blue", "RE", "NIR")))
  }
  # terra reconoce el NoData declarado. Se excluye también el centinela del TIFF preparado en QGIS.
  b <- terra::ifel(b == -10000, NA, b)
  valido <- is.finite(b[[1]])
  for (k in 2:5) valido <- valido & is.finite(b[[k]])
  b <- terra::mask(b, terra::ifel(valido, 1, NA))
  names(b) <- c("Red", "Green", "Blue", "RE", "NIR")
  b
}

indices_entrada_sentera <- function(nombres, origen) {
  # Cada origen conserva su propia biblioteca; todos sus índices pueden seleccionarse.
  unique(nombres)
}

# Nombres estables de productos; nunca se utilizan para escribir la entrada.
nombre_tif_sentera <- function(producto, lote, fecha) {
  nombre_archivo_cipat(producto, lote, fecha, "Sentera", "tif")
}
