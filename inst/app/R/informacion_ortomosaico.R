# Ficha de metadatos y asignación espectral. Lee cabeceras, no el ráster completo.
bandas_predeterminadas_cipat <- function(clave) {
  switch(clave, multibanda = c(R = 1L, G = 2L, B = 3L, RE = 4L, NIR = 5L),
    rgb =, rgb_phantom = c(R = 1L, G = 2L, B = 3L), ms = c(RE = 1L, NIR = 3L),
    stop("Archivo sin canales espectrales configurables."))
}

validar_asignacion_bandas_cipat <- function(mapa, n, clave) {
  canales <- names(bandas_predeterminadas_cipat(clave))
  if (!is.numeric(mapa) || length(mapa) != length(canales) ||
      is.null(names(mapa)) || anyDuplicated(names(mapa)) || !setequal(names(mapa), canales))
    stop("Asigna una banda a cada canal requerido.")
  mapa <- mapa[canales]
  if (any(!is.finite(mapa)) || any(mapa != floor(mapa)) || any(mapa < 1 | mapa > n))
    stop("Las bandas asignadas deben ser números enteros disponibles en el archivo.")
  if (anyDuplicated(unname(mapa))) stop("Cada canal debe utilizar una banda diferente.")
  stats::setNames(as.integer(mapa), canales)
}

mapa_bandas_cipat <- function(p, clave, n) {
  mapa <- p$mapas_bandas[[clave]]
  if (is.null(mapa)) mapa <- bandas_predeterminadas_cipat(clave)
  validar_asignacion_bandas_cipat(mapa, n, clave)
}

texto_bandas_cipat <- function(mapa) paste(paste(names(mapa), mapa, sep = "="), collapse = "; ")

leer_texto_bandas_cipat <- function(texto, clave) {
  if (length(texto) != 1L || is.na(texto) || !nzchar(texto)) return(bandas_predeterminadas_cipat(clave))
  partes <- strsplit(texto, ";", fixed = TRUE)[[1]]
  pares <- strsplit(trimws(partes), "=", fixed = TRUE)
  if (any(lengths(pares) != 2L)) stop("La asignación de bandas guardada no tiene un formato válido.")
  mapa <- stats::setNames(suppressWarnings(as.numeric(vapply(pares, `[[`, character(1), 2L))),
                         vapply(pares, `[[`, character(1), 1L))
  validar_asignacion_bandas_cipat(mapa, Inf, clave)
}

clave_rgb_cipat <- function(p) {
  if (identical(p$tipo, "phantom")) "rgb_phantom"
  else if (identical(p$origen_sentera, "multibanda")) "multibanda" else "rgb"
}

bandas_rgb_resultado_cipat <- function(x, n) {
  p <- x$fuentes; if (is.null(p$tipo)) p$tipo <- x$tipo
  unname(mapa_bandas_cipat(p, clave_rgb_cipat(p), n)[c("R", "G", "B")])
}

controles_ortomosaico_cipat <- function(clave) {
  shiny::div(class = "ortho-file-actions",
    shiny::actionButton(paste0("info_orto_", clave), "Información", icon = shiny::icon("circle-info")),
    if (clave %in% c("multibanda", "rgb", "rgb_phantom", "ms"))
      shiny::actionButton(paste0("bandas_orto_", clave), "Asignar bandas", icon = shiny::icon("layer-group")),
    shiny::uiOutput(paste0("resumen_bandas_", clave)))
}

ficha_ortomosaico_cipat <- function(ruta) {
  r <- terra::rast(ruta)
  crs <- sf::st_crs(terra::crs(r))
  unidad <- if (!is.na(crs) && !is.null(crs$units_gdal)) crs$units_gdal else "unidades del CRS"
  fila <- function(k, v) shiny::tags$tr(shiny::tags$th(k), shiny::tags$td(v))
  fmt <- function(x) paste(format(x, digits = 8, trim = TRUE, big.mark = " "), collapse = " × ")
  nombre_crs <- if (is.na(crs)) "Sin CRS definido" else {
    nombre <- tryCatch(crs$Name, error = function(e) NULL)
    if (length(nombre) == 1L && !is.na(nombre) && nzchar(nombre)) nombre else crs$input
  }
  tam <- file.info(ruta)$size / 1024^2
  extension <- as.vector(terra::ext(r))
  shiny::tagList(
    shiny::div(class = "ortho-file-name", basename(ruta)),
    shiny::div(class = "ortho-facts",
      shiny::div(shiny::strong(format(nrow(r), big.mark = " ")), shiny::span("Filas")),
      shiny::div(shiny::strong(format(ncol(r), big.mark = " ")), shiny::span("Columnas")),
      shiny::div(shiny::strong(terra::nlyr(r)), shiny::span("Bandas"))),
    shiny::tags$table(class = "ortho-info-table", shiny::tags$tbody(
      fila("Resolución X × Y", paste(fmt(terra::res(r)), unidad)),
      fila("Sistema de coordenadas", nombre_crs),
      fila("Código EPSG", if (is.na(crs) || is.na(crs$epsg)) "No identificado" else as.character(crs$epsg)),
      fila("Tipo de datos", paste(unique(terra::datatype(r)), collapse = ", ")),
      fila("Tamaño del archivo", paste(format(round(tam, 2), trim = TRUE), "MB")),
      fila("Extensión X", paste(extension[1], "a", extension[2])),
      fila("Extensión Y", paste(extension[3], "a", extension[4])))),
    shiny::tags$details(shiny::tags$summary("Nombres de las bandas"),
      shiny::tags$ol(lapply(names(r), shiny::tags$li))),
    shiny::tags$p(class = "ib-note", "La resolución se expresa en unidades del CRS. La unidad vertical del CHM se confirma por separado en Parámetros."))
}

registrar_ortomosaicos_cipat <- function(input, output, session) {
  claves <- c("multibanda", "rgb", "rgb_phantom", "ms")
  estado <- shiny::reactiveValues()
  activo <- shiny::reactiveVal(NULL)
  ruta_archivo <- function(k) {
    ruta <- input[[k]]
    if (length(ruta) != 1L || !nzchar(ruta) || !file.exists(ruta)) stop("Importa primero un archivo válido.")
    ruta
  }
  asignacion <- function(k) {
    registro <- estado[[k]]
    if (!is.null(registro) && identical(registro$ruta, input[[k]])) registro$mapa
    else bandas_predeterminadas_cipat(k)
  }
  for (k in c(claves, "chm", "dsm", "dtm")) local({
    clave <- k
    shiny::observeEvent(input[[paste0("info_orto_", clave)]], {
      tryCatch(shiny::showModal(shiny::modalDialog(title = "Información del ortomosaico",
        ficha_ortomosaico_cipat(ruta_archivo(clave)), size = "l", easyClose = TRUE,
        footer = shiny::modalButton("Cerrar"))),
        error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })
    if (clave %in% claves) {
      output[[paste0("resumen_bandas_", clave)]] <- shiny::renderUI({
        shiny::tags$p(class = "ib-note band-assignment-summary", texto_bandas_cipat(asignacion(clave)))
      })
      shiny::observeEvent(input[[paste0("bandas_orto_", clave)]], {
        tryCatch({
          ruta <- ruta_archivo(clave); r <- terra::rast(ruta); n <- terra::nlyr(r)
          mapa <- asignacion(clave)
          activo(list(clave = clave, ruta = ruta, n = n))
          opciones <- stats::setNames(seq_len(n), paste("Banda", seq_len(n)))
          nombres <- c(R = "Rojo (R)", G = "Verde (G)", B = "Azul (B)", RE = "Red Edge (RE)", NIR = "Infrarrojo cercano (NIR)")
          shiny::showModal(shiny::modalDialog(title = "Asignación de bandas", size = "m",
            shiny::div(class = "ortho-file-name", basename(ruta)),
            shiny::tags$p(class = "ib-note", "Indica qué banda del archivo corresponde a cada canal. Esta asignación se usa en la vista previa y en los nuevos análisis."),
            shiny::div(class = "band-assignment-grid", lapply(names(mapa), function(canal)
              shiny::selectInput(paste0("asignar_banda_", canal), nombres[[canal]], choices = opciones, selected = mapa[[canal]]))),
            shiny::tags$p(class = "ib-note", "La asignación no modifica el TIFF ni añade correcciones radiométricas."),
            footer = shiny::tagList(shiny::modalButton("Cancelar"),
              shiny::actionButton("aplicar_asignacion_bandas", "Aplicar asignación", class = "btn-primary"))))
        }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
      })
    }
  })
  shiny::observeEvent(input$aplicar_asignacion_bandas, {
    tryCatch({
      a <- activo(); if (is.null(a)) stop("Abre de nuevo el panel de asignación.")
      if (!identical(a$ruta, input[[a$clave]])) stop("El archivo cambió. Abre de nuevo el panel de asignación.")
      canales <- names(bandas_predeterminadas_cipat(a$clave))
      mapa <- stats::setNames(vapply(canales, function(k) {
        valor <- suppressWarnings(as.numeric(input[[paste0("asignar_banda_", k)]]))
        if (length(valor) == 1L) valor else NA_real_
      }, numeric(1)), canales)
      mapa <- validar_asignacion_bandas_cipat(mapa, a$n, a$clave)
      estado[[a$clave]] <- list(ruta = a$ruta, mapa = mapa)
      shiny::removeModal()
      shiny::showNotification("Asignación de bandas actualizada.", type = "message")
    }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
  })
  guardar <- function() stats::setNames(lapply(claves,function(k) estado[[k]]),claves)
  restaurar <- function(registros) {for(k in claves) estado[[k]]<-registros[[k]];invisible(NULL)}
  list(mapas = shiny::reactive(stats::setNames(lapply(claves, asignacion), claves)),guardar=guardar,restaurar=restaurar)
}
