# Utilidades de instalación y distribución. No modifica preferencias de R.
cipat_paquetes <- c("shiny", "terra", "sf", "exactextractr", "openxlsx",
                    "autothresholdr", "DT", "future", "promises", "leaflet",
                    "htmlwidgets", "htmltools", "zip", "pliman")
cipat_minimos <- c(shiny = "1.8.1", leaflet = "2.2.2", pliman = "3.1.0")

cipat_preparar_biblioteca <- function() {
  if (getRversion() < "4.1.0") stop("Se requiere R 4.1.0 o posterior.", call. = FALSE)
  rutas <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1]]
  rutas <- rutas[nzchar(rutas)]
  if (!length(rutas)) stop("R no tiene una biblioteca de usuario configurada (R_LIBS_USER).", call. = FALSE)
  ruta <- path.expand(rutas[[1]])
  if (!dir.exists(ruta) && !dir.create(ruta, recursive = TRUE)) {
    stop("No se pudo crear la biblioteca de usuario: ", ruta, call. = FALSE)
  }
  if (file.access(ruta, 2) != 0L) stop("La biblioteca de usuario no permite escritura: ", ruta, call. = FALSE)
  .libPaths(c(ruta, .libPaths()))
  ruta
}

cipat_pendientes <- function(paquetes = cipat_paquetes) {
  paquetes[vapply(paquetes, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) return(TRUE)
    if (p %in% names(cipat_minimos)) return(utils::packageVersion(p) < cipat_minimos[[p]])
    FALSE
  }, logical(1))]
}

cipat_verificar_archivos <- function(raiz) {
  archivos <- c("app.R", "R/analisis_selectivo.R", "R/segmentacion_muestras.R", "R/distribucion.R", "R/informacion_ortomosaico.R", "R/ejemplos.R", "R/indices_editables.R",
    "R/seleccion_indices_rgb.R", "R/catalogo_indices.R", "R/sentera_multibanda.R",
    "R/procesamiento_bloques.R", "R/guardado_sentera.R", "R/resultados_estandar.R", "R/exploracion.R",
    "R/lectura_resultados.R", "R/mapa_interactivo.R", "R/bandas_entrada.R",
    "R/entradas_productos.R", "R/vectoriales.R", "R/integrar_datos.R",
    "R/testigos.R", "R/resumen_resultados.R", "www/estilo.css", "www/interfaz.js",
    "www/fonts/Montserrat-variable.ttf", "www/cipat.png", "www/papalotla.png")
  faltan <- archivos[!file.exists(file.path(raiz, archivos))]
  if (length(faltan)) stop("El paquete está incompleto. Descomprime de nuevo el ZIP completo. Faltan: ",
    paste(faltan, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
