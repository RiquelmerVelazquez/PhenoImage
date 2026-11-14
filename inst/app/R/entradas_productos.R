# Entradas, productos y exportación de visores. Las fórmulas HTP no cambian.
rasters_entrada_cipat <- function(input) {
  archivos <- if (identical(input$tipo, "phantom")) {
    if (identical(input$fuente_altura, "DSM_DTM")) list(RGB = input$rgb_phantom, DSM = input$dsm, DTM = input$dtm)
    else list(RGB = input$rgb_phantom, CHM = input$chm)
  } else if (identical(input$origen_sentera, "multibanda")) list(RGB = input$multibanda)
  else list(RGB = input$rgb, `Ortomosaico (RE/NIR) · Sentera` = input$ms)
  archivos[vapply(archivos, function(r) length(r) == 1L && nzchar(r) && file.exists(r), logical(1))]
}

vector_entrada_cipat <- function(ruta, campo_id) {
  if (!length(ruta) || !nzchar(ruta) || !file.exists(ruta)) stop("Importa las parcelas en este panel para ver el mapa.")
  p <- sf::st_read(ruta, quiet = TRUE)
  if (!nrow(p) || is.na(sf::st_crs(p))) stop("Revisa que el vector contenga parcelas y tenga su CRS definido.")
  if (!all(as.character(sf::st_geometry_type(p)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("La capa debe contener polígonos de parcelas.")
  p <- sf::st_make_valid(sf::st_zm(p, drop = TRUE, what = "ZM"))
  p <- p[!sf::st_is_empty(p), , drop = FALSE]
  if (!nrow(p) || !all(as.character(sf::st_geometry_type(p)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("Revisa las geometrías del vector en QGIS.")
  atributos <- sf::st_drop_geometry(p)
  usar_id <- length(campo_id) == 1L && campo_id %in% names(atributos)
  nombres <- if (usar_id) as.character(atributos[[campo_id]]) else as.character(seq_len(nrow(p)))
  p <- sf::st_transform(p, 4326)
  b <- sf::st_bbox(p)
  if (any(!is.finite(b)) || b["xmin"] < -180 || b["xmax"] > 180 || b["ymin"] < -85 || b["ymax"] > 85) stop("Revisa el CRS del vector: las coordenadas transformadas no son válidas para el visor.")
  fichas <- vapply(seq_len(nrow(p)), function(i) {
    filas <- lapply(names(atributos)[vapply(atributos, is.atomic, logical(1))], function(k)
      shiny::tags$tr(shiny::tags$th(k), shiny::tags$td(valor_mapa_cipat(atributos[[k]][i]))))
    as.character(shiny::tags$div(class = "cipat-popup", shiny::tags$div(class = "popup-kicker", "PARCELAS · DATOS VECTORIALES"),
      shiny::tags$h4(paste(if (usar_id) "Parcela" else "Elemento", nombres[i])), shiny::tags$table(filas)))
  }, character(1))
  list(ok = TRUE, poligonos = p, limites = b, fichas = fichas, usar_id = usar_id, etiquetas = paste(if (usar_id) "Parcela" else "Elemento", nombres))
}

mapa_base_cipat <- function(limites) {
  m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1, maxZoom = 24, zoomSnap = .5))
  m <- leaflet::addScaleBar(m, position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))
  leaflet::fitBounds(m, unname(limites["xmin"]), unname(limites["ymin"]), unname(limites["xmax"]), unname(limites["ymax"]))
}

limites_imagen_cipat <- function(r) {
  # SpatExtent usa xmin,xmax,ymin,ymax. Sus accesores $ conservan nombres:
  # c(xmin=e$xmin) produciría xmin.xmin, que st_bbox no reconoce.
  coordenadas <- stats::setNames(as.numeric(as.vector(terra::ext(r)))[c(1L, 3L, 2L, 4L)], c("xmin", "ymin", "xmax", "ymax"))
  if (length(coordenadas) != 4L || any(!is.finite(coordenadas)) ||
      coordenadas["xmin"] >= coordenadas["xmax"] || coordenadas["ymin"] >= coordenadas["ymax"])
    stop("La imagen no tiene una extensión válida para el visor.")
  crs <- terra::crs(r)
  if (!nzchar(crs)) stop("La imagen no tiene CRS definido para calcular sus límites.")
  b <- sf::st_bbox(coordenadas, crs = sf::st_crs(crs))
  limites <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(b), 4326))
  if (any(!is.finite(limites)) || limites["xmin"] < -180 || limites["xmax"] > 180 || limites["ymin"] < -85 || limites["ymax"] > 85)
    stop("Los límites de la imagen no son válidos para Leaflet. Revisa el CRS del archivo.")
  limites
}

agregar_imagen_cipat <- function(m, r, opacidad = .9) {
  leaflet::addRasterImage(m, r$imagen, colors = if (r$rgb) "Spectral" else r$paleta, opacity = opacidad,
    project = FALSE, group = "Imagen", layerId = "imagen", maxBytes = 4 * 1024 * 1024)
}

mapa_vacio_entrada_cipat <- function() {
  m <- leaflet::setView(leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1, maxZoom = 24, zoomSnap = .5)), lng = 0, lat = 0, zoom = 1)
  # Vuelve a encuadrar al mostrar el panel o cambiar su tamaño. Las bandas y
  # paletas no cambian el tamaño del mapa y conservan el acercamiento elegido.
  htmlwidgets::onRender(m, "function(el, x) {
    var lastSize = '';
    function reportSize() {
      var r = el.getBoundingClientRect();
      var size = r.width > 0 && r.height > 0 ? Math.round(r.width) + 'x' + Math.round(r.height) : '';
      if (size !== lastSize) {
        lastSize = size;
        if (size && window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue(el.id + '_visible', size + ':' + Date.now(), {priority: 'event'});
        }
      }
    }
    if (window.ResizeObserver) { var observer = new ResizeObserver(reportSize); observer.observe(el); }
    window.addEventListener('resize', reportSize);
    if (window.jQuery) jQuery(document).on('shown.bs.tab', reportSize);
    requestAnimationFrame(reportSize);
  }")
}

limpiar_mapa_entrada_cipat <- function(id, session) {
  # Actualiza las capas sin destruir el visor al cambiar de módulo o archivo.
  m <- leaflet::leafletProxy(id, session = session)
  m <- leaflet::clearShapes(m)
  m <- leaflet::clearImages(m)
  m <- leaflet::clearControls(m)
  m <- leaflet::removeLayersControl(m)
  leaflet::addScaleBar(m, position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))
}

ajustar_mapa_entrada_cipat <- function(m, limites) {
  leaflet::fitBounds(m, unname(limites["xmin"]), unname(limites["ymin"]), unname(limites["xmax"]), unname(limites["ymax"]))
}

registrar_entradas_cipat <- function(input, output, session, parcelas, mapas_bandas) {
  vector <- shiny::reactive({
    p <- parcelas()
    tryCatch(vector_entrada_cipat(p$ruta, p$id), error = function(e) list(ok = FALSE, mensaje = conditionMessage(e)))
  })
  raster <- registrar_raster_entrada_cipat(input, output, session, vector, mapas_bandas)
  capas <- shiny::reactive({ rasters_entrada_cipat(input) })
  bandas <- registrar_bandas_entrada_cipat(input, output, session, capas, "vector_entrada_fondo", "vector_entrada", shiny::reactive(vector()$poligonos), mapas_bandas = mapas_bandas)
  output$vector_entrada_controles <- shiny::renderUI({
    v <- vector(); if (!isTRUE(v$ok)) return(shiny::div(class = "tip", v$mensaje))
    opciones <- names(capas())
    activo <- shiny::isolate(input$vector_entrada_fondo)
    if (is.null(activo) || !activo %in% c("__ninguna__", opciones)) activo <- if ("RGB" %in% opciones) "RGB" else "__ninguna__"
    shiny::tagList(shiny::selectInput("vector_entrada_fondo", "Superponer ráster", c("Sin imagen" = "__ninguna__", stats::setNames(opciones, opciones)), selected = activo),
      shiny::uiOutput("vector_entrada_bandas"),
      shiny::sliderInput("vector_entrada_opacidad", "Opacidad de la imagen", min = 0, max = 1, value = .9, step = .1),
      ayuda_breve("Pulsa una parcela para ver sus atributos. Puedes superponer la composición RGB o una banda individual. La imagen es una vista reducida; la selección de bandas y colores solo afecta a su visualización."))
  })
  imagen <- shiny::reactive({
    v <- vector(); if (!isTRUE(v$ok)) return(list(ok = TRUE, imagen = NULL))
    bandas$imagen()
  })
  output$vector_entrada_mapa <- leaflet::renderLeaflet(mapa_vacio_entrada_cipat())
  shiny::outputOptions(output, "vector_entrada_mapa", suspendWhenHidden = FALSE)
  vista <- new.env(parent = emptyenv()); vista$clave <- NULL
  shiny::observe({
    v <- vector()
    m <- limpiar_mapa_entrada_cipat("vector_entrada_mapa", session)
    if (!isTRUE(v$ok)) {
      vista$clave <- NULL
      m <- leaflet::setView(m, lng = 0, lat = 0, zoom = 1)
      return(leaflet::addControl(m, as.character(shiny::tags$div(class = "tip", v$mensaje)), position = "topright"))
    }
    visible <- input$vector_entrada_mapa_visible
    clave <- paste(input$tipo, parcelas()$ruta, input$vector_entrada_fondo, if (is.null(visible)) "" else visible, sep = "|")
    r <- imagen()
    if (!identical(vista$clave, clave)) {
      limites <- if(isTRUE(r$ok) && !is.null(r$imagen)) tryCatch(limites_imagen_cipat(r$imagen),error=function(e) v$limites) else v$limites
      m <- ajustar_mapa_entrada_cipat(m, limites); vista$clave <- clave
    }
    if (isTRUE(r$ok) && !is.null(r$imagen)) m <- tryCatch(agregar_imagen_cipat(m, r, if (is.null(input$vector_entrada_opacidad)) .9 else input$vector_entrada_opacidad),
      error = function(e) leaflet::addControl(m, as.character(shiny::tags$span(paste("Imagen no disponible:", conditionMessage(e)))), position = "topright"))
    if (isTRUE(r$ok) && !is.null(r$imagen) && !r$rgb && !isTRUE(r$sin_datos))
      m <- leaflet::addLegend(m, pal = r$paleta, values = as.numeric(terra::values(r$imagen)), title = as.character(shiny::tags$span(if (!is.null(r$titulo)) r$titulo else input$vector_entrada_fondo)), position = "bottomright", na.label = "Sin datos")
    leaflet::addPolygons(m, data = v$poligonos, layerId = as.character(seq_len(nrow(v$poligonos))), group = "Parcelas", color = "#f3de7c", weight = 2, fillOpacity = .03,
      popup = v$fichas, label = v$etiquetas, highlightOptions = leaflet::highlightOptions(color = "#ffffff", weight = 3, bringToFront = TRUE))
  })
  output$vector_entrada_aviso <- shiny::renderUI({
    r <- imagen(); if (!isTRUE(r$ok)) shiny::div(class = "tip", paste("La superposición no está disponible:", r$mensaje)) else NULL
  })
  invisible(list(vector = vector, imagen = imagen, capas = capas, bandas = bandas, raster = raster))
}

registrar_raster_entrada_cipat <- function(input, output, session, vector, mapas_bandas) {
  capas <- shiny::reactive({ rasters_entrada_cipat(input) })
  bandas <- registrar_bandas_entrada_cipat(input, output, session, capas, "raster_entrada_capa", "raster_entrada", mapas_bandas = mapas_bandas)
  output$raster_entrada_controles <- shiny::renderUI({
    nombres <- names(capas())
    if (!length(nombres)) return(shiny::div(class = "tip", "Importa una imagen para abrir su vista previa."))
    activo <- shiny::isolate(input$raster_entrada_capa)
    if (is.null(activo) || !activo %in% nombres) activo <- if ("RGB" %in% nombres) "RGB" else nombres[1]
    shiny::tagList(
      shiny::selectInput("raster_entrada_capa", "Imagen a visualizar", stats::setNames(nombres, nombres), selected = activo),
      shiny::uiOutput("raster_entrada_bandas"),
      shiny::sliderInput("raster_entrada_opacidad", "Opacidad de la imagen", min = 0, max = 1, value = .9, step = .1),
      shiny::checkboxInput("raster_entrada_parcelas", "Superponer parcelas importadas", FALSE),
      ayuda_breve("El visor funciona antes de importar las parcelas. Permite ver la composición RGB o seleccionar una banda con escala de grises u otra paleta. Se utiliza una copia reducida; las elevaciones conservan las unidades del archivo original."))
  })
  imagen <- bandas$imagen
  output$raster_entrada_mapa <- leaflet::renderLeaflet(mapa_vacio_entrada_cipat())
  shiny::outputOptions(output, "raster_entrada_mapa", suspendWhenHidden = FALSE)
  vista <- new.env(parent = emptyenv()); vista$clave <- NULL
  shiny::observe({
    r <- imagen()
    m <- limpiar_mapa_entrada_cipat("raster_entrada_mapa", session)
    if (!isTRUE(r$ok)) {
      vista$clave <- NULL
      m <- leaflet::setView(m, lng = 0, lat = 0, zoom = 1)
      return(leaflet::addControl(m, as.character(shiny::tags$div(class = "tip", r$mensaje)), position = "topright"))
    }
    tryCatch({
      visible <- input$raster_entrada_mapa_visible
      clave <- paste(input$tipo, capas()[[input$raster_entrada_capa]], if (is.null(visible)) "" else visible, sep = "|")
      if (!identical(vista$clave, clave)) { m <- ajustar_mapa_entrada_cipat(m, limites_imagen_cipat(r$imagen)); vista$clave <- clave }
      m <- agregar_imagen_cipat(m, r, if (is.null(input$raster_entrada_opacidad)) .9 else input$raster_entrada_opacidad)
      if (!r$rgb && !isTRUE(r$sin_datos)) m <- leaflet::addLegend(m, pal = r$paleta, values = as.numeric(terra::values(r$imagen)),
        title = as.character(shiny::tags$span(if (!is.null(r$titulo)) r$titulo else input$raster_entrada_capa)), position = "bottomright", na.label = "Sin datos")
      v <- vector(); superponer <- isTRUE(input$raster_entrada_parcelas) && isTRUE(v$ok)
      if (superponer) m <- leaflet::addPolygons(m, data = v$poligonos, layerId = as.character(seq_len(nrow(v$poligonos))),
        group = "Parcelas", color = "#f3de7c", weight = 2, fillOpacity = .03, popup = v$fichas, label = v$etiquetas,
        highlightOptions = leaflet::highlightOptions(color = "#ffffff", weight = 3, bringToFront = TRUE))
      leaflet::addLayersControl(m, overlayGroups = if (superponer) c("Imagen", "Parcelas") else "Imagen",
        options = leaflet::layersControlOptions(collapsed = TRUE))
    }, error = function(e) leaflet::addControl(m, as.character(shiny::tags$div(class = "tip", paste("No se pudo abrir la imagen:", conditionMessage(e)))), position = "topright"))
  })
  output$raster_entrada_aviso <- shiny::renderUI({
    r <- imagen()
    if (!isTRUE(r$ok)) return(NULL) # El mensaje se muestra en el propio visor.
    avisos <- character()
    if (isTRUE(input$raster_entrada_parcelas) && !isTRUE(vector()$ok)) avisos <- c(avisos, "Importa un vector válido en Vectorial para superponer las parcelas.")
    if (!r$rgb && !is.null(r$banda)) avisos <- c(avisos, paste("Vista individual:", r$titulo, "· escala en valores de la banda del archivo."))
    if (isTRUE(r$sin_datos)) avisos <- c(avisos, "Sin píxeles válidos en la vista reducida de esta imagen.")
    if (length(avisos)) shiny::div(class = "tip", paste(avisos, collapse = " ")) else NULL
  })
  invisible(list(imagen = imagen, bandas = bandas, capas = capas))
}

productos_cipat <- function(x) {
  capas <- x$capas[vapply(x$capas, file.exists, logical(1))]
  archivos <- list.files(x$salida, pattern = "\\.tiff?$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE)
  normalizar <- function(r) normalizePath(r, winslash = "/", mustWork = TRUE)
  existentes <- if (length(capas)) vapply(capas, normalizar, character(1)) else character()
  etiquetas <- c(Multibanda_corregida = "Multibanda corregida · banda 1", NDVI_Mascara_Vegetacion = "NDVI · vegetación", Green_vegetacion = "Banda verde · vegetación")
  for (r in archivos) {
    if (normalizar(r) %in% existentes) next
    base <- tools::file_path_sans_ext(basename(r))
    if (x$tipo %in% c("sentera", "phantom")) {
      sufijo <- tools::file_path_sans_ext(nombre_archivo_cipat("", x$fuentes$lote, x$fuentes$fecha, if (x$tipo == "phantom") "RGB" else "Sentera", "tif"))
      if (endsWith(base, sufijo)) base <- substr(base, 1L, nchar(base) - nchar(sufijo))
    }
    nombre <- if (base %in% names(etiquetas)) etiquetas[[base]] else base
    nombre <- make.unique(c(names(capas), nombre), sep = " · ")[length(capas) + 1L]
    capas[[nombre]] <- r; existentes <- c(existentes, normalizar(r))
  }
  capas
}

metrica_producto_cipat <- function(capa, tabla, tipo) {
  especiales <- c(`Máscara de vegetación` = "Plant_ground_cover_image_Vegetation_pct",
    `Máscara por muestras` = if (tipo == "phantom") "Plant_ground_cover_image_Muestras_RGB_pct" else "Plant_ground_cover_image_Muestras_Sentera_pct",
    `Máscara NDVI` = "Plant_ground_cover_image_NDVI_pct", `Máscara RGB` = "Plant_ground_cover_image_RGB_pct",
    `Máscara CHM` = "Canopy_height_image_p90", `CHM original (cm)` = "Canopy_height_image_p90", `CHM filtrado (cm)` = "Canopy_height_image_p90",
    `Textura Entropy` = "RGB_Entropy_mean", `Textura Contrast` = "RGB_Contrast_mean", `Textura Homogeneity` = "RGB_Homogeneity_mean", `NDVI · vegetación` = "NDVI_mean")
  if ("Plant_ground_cover_image_Green_pct" %in% names(tabla)) especiales["Máscara de vegetación"] <- "Plant_ground_cover_image_Green_pct"
  especiales["Máscara de clases por muestras"] <- "Plant_ground_cover_image_Total_pct"
  nombre <- if (capa %in% names(especiales)) especiales[[capa]] else paste0(capa, "_mean")
  if (!nombre %in% names(tabla) && capa %in% c("Red", "Green", "Blue", "RE", "NIR")) {
    nombre <- paste0(if (tipo == "phantom") "RGB_" else "Sentera_", capa, "_mean")
  }
  if (nombre %in% names(tabla) && is.numeric(tabla[[nombre]])) nombre else NULL
}

visor_producto_cipat <- function(x, capa, parcelas = NULL, mostrar_parcelas = TRUE) {
  r <- preparar_imagen_cipat(x, capa, if (is.null(parcelas)) NULL else parcelas$poligonos)
  m <- mapa_base_cipat(if (is.null(parcelas)) limites_imagen_cipat(r$imagen) else parcelas$limites)
  m <- agregar_imagen_cipat(m, r)
  if (!r$rgb && !isTRUE(r$sin_datos)) m <- leyenda_imagen_cipat(m, r, capa)
  titulo <- as.character(shiny::tags$div(class = "product-map-title", shiny::tags$strong(capa), shiny::tags$br(),
    shiny::tags$small(paste(x$fuentes$lote, x$fuentes$fecha, sep = " · ")),
    if (!r$rgb && r$bandas > 1L) shiny::tags$div("Vista de banda 1"),
    if (isTRUE(r$sin_datos)) shiny::tags$div("Sin píxeles válidos en la vista reducida")))
  m <- leaflet::addControl(m, titulo, position = "topright")
  if (!is.null(parcelas) && isTRUE(mostrar_parcelas)) {
    p <- parcelas; nombre <- metrica_producto_cipat(capa, p$tabla, x$tipo)
    gen <- campo_mapa_cipat(p$grupos, c("genotipo", "genotype", "originali", "original1", "material"))
    rep <- campo_mapa_cipat(p$grupos, c("rep", "repeticion", "repetición", "repetition", "bloque", "block"))
    campo <- function(k, i) if (k %in% names(p$tabla)) valor_mapa_cipat(p$tabla[[k]][i]) else "Sin definir"
    fichas <- vapply(seq_along(p$ids), function(i) popup_mapa_cipat(p$ids[i], if (is.null(nombre)) "No aplica" else p$tabla[[nombre]][i],
      if (is.null(nombre)) "Imagen de referencia · sin métrica asociada" else paste("HTP ·", etiqueta_eda(nombre)), campo(gen, i), campo(rep, i), x$fuentes$lote, x$fuentes$fecha), character(1))
    m <- leaflet::addPolygons(m, data = p$poligonos, layerId = p$ids, group = "Parcelas", color = "#f3de7c", weight = 1.5, fillOpacity = .02,
      popup = fichas, label = paste("Parcela", p$ids), highlightOptions = leaflet::highlightOptions(color = "#ffffff", weight = 3, bringToFront = TRUE))
  }
  if (!is.null(parcelas) && isTRUE(mostrar_parcelas)) m <- leaflet::addLayersControl(m, overlayGroups = c("Imagen", "Parcelas"), options = leaflet::layersControlOptions(collapsed = TRUE))
  else m <- leaflet::addLayersControl(m, overlayGroups = "Imagen", options = leaflet::layersControlOptions(collapsed = TRUE))
  m
}

registrar_productos_cipat <- function(input, output, session, ultima, datos) {
  capas <- shiny::reactive({ shiny::req(ultima()); productos_cipat(ultima()) })
  parcelas <- shiny::reactive({ shiny::req(ultima()); tryCatch(preparar_mapa_cipat(ultima(), datos()), error = function(e) NULL) })
  output$producto_controles <- shiny::renderUI({
    nombres <- names(capas())
    activo <- shiny::isolate(input$producto_raster)
    if (is.null(activo) || !activo %in% nombres) activo <- if (length(nombres)) nombres[1] else NULL
    shiny::tagList(shiny::selectInput("producto_raster", "Producto", stats::setNames(nombres, nombres), selected = activo),
      shiny::checkboxInput("producto_parcelas", "Superponer parcelas", TRUE),
      shiny::downloadButton("descargar_producto_tif", "Descargar TIFF"),
      ayuda_breve("Cada producto tiene su propia escala y visor. Los valores de las fichas son métricas HTP por parcela; no son el valor del píxel pulsado."))
  })
  output$producto_mapa <- leaflet::renderLeaflet({
    x <- ultima(); shiny::req(x, input$producto_raster)
    shiny::validate(shiny::need(input$producto_raster %in% names(capas()), "No hay productos ráster disponibles."))
    x$capas <- capas()
    tryCatch(visor_producto_cipat(x, input$producto_raster, parcelas(), isTRUE(input$producto_parcelas)),
      error = function(e) { shiny::validate(shiny::need(FALSE, paste("No se pudo abrir el producto:", conditionMessage(e)))) })
  })
  output$descargar_producto_tif <- shiny::downloadHandler(filename = function() { shiny::req(input$producto_raster); basename(capas()[[input$producto_raster]]) },
    content = function(file) { shiny::req(input$producto_raster, input$producto_raster %in% names(capas())); if (!file.copy(capas()[[input$producto_raster]], file, overwrite = TRUE)) stop("No se pudo copiar el TIFF seleccionado.") })
  output$producto_archivo <- shiny::renderText({
    shiny::req(input$producto_raster, input$producto_raster %in% names(capas()))
    basename(capas()[[input$producto_raster]])
  })
  output$producto_aviso <- shiny::renderUI({
    if (is.null(parcelas())) shiny::div(class = "tip", "El ráster se puede visualizar, pero las parcelas no están disponibles en su ruta registrada.") else NULL
  })
}

exportar_visores_cipat <- function(x, assets = NULL, log = NULL) {
  capas <- productos_cipat(x)
  if (!length(capas)) stop("No hay TIFF disponibles para exportar visores.")
  carpeta <- file.path(x$salida, "Visores_interactivos")
  version <- 2L
  while (dir.exists(carpeta)) {
    carpeta <- file.path(x$salida, paste0("Visores_interactivos_v", sprintf("%02d", version)))
    version <- version + 1L
  }
  dir.create(carpeta, recursive = TRUE)
  carpeta <- normalizePath(carpeta, winslash = "/", mustWork = TRUE)
  x$capas <- capas
  p <- tryCatch(preparar_mapa_cipat(x, preparar_eda(x)), error = function(e) NULL)
  filas <- vector("list", length(capas))
  css <- ".cipat-popup{font:12px/1.5 'Segoe UI',Arial,sans-serif;min-width:210px;max-width:290px;color:#2d4835}.cipat-popup h4{font-size:18px;margin:8px 0}.cipat-popup th{text-align:left;padding-right:12px;color:#70806c}.popup-kicker,.popup-meta{font-size:10px;color:#70806c}.popup-trait{padding:12px;background:#edf4e7;margin:12px 0}.popup-trait span,.popup-trait strong{display:block}.popup-trait strong{font-size:24px}.leaflet-container{background:#e8eee5}.product-map-title{font:12px/1.5 'Segoe UI',Arial,sans-serif;padding:9px}.product-map-title strong{font-size:15px}"
  for (i in seq_along(capas)) {
    capa <- names(capas)[i]
    if (!is.null(log)) cat("Visor ", i, "/", length(capas), ": ", capa, "\n", file = log, append = TRUE)
    seguro <- chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", capa)
    seguro <- tolower(gsub("[^A-Za-z0-9_-]+", "_", seguro))
    nombre <- nombre_archivo_cipat(paste0("Visor_", seguro), x$fuentes$lote, x$fuentes$fecha, if (x$tipo == "phantom") "RGB" else "Sentera", "html")
    estado <- tryCatch({
      m <- visor_producto_cipat(x, capa, p)
      m <- htmlwidgets::prependContent(m, htmltools::tags$style(htmltools::HTML(css)))
      htmlwidgets::saveWidget(m, file.path(carpeta, nombre), selfcontained = FALSE, libdir = "recursos", title = paste("PhenoImage ·", capa))
      "Creado"
    }, error = function(e) paste("No creado:", conditionMessage(e)))
    filas[[i]] <- data.frame(Producto = capa, Archivo_TIFF = basename(capas[[i]]), Visor_HTML = if (identical(estado, "Creado")) nombre else "", Estado = estado, stringsAsFactors = FALSE)
  }
  tabla <- do.call(rbind, filas)
  utils::write.csv(tabla, file.path(carpeta, "Resumen_visores.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(assets)) for (logo in c("cipat.png", "papalotla.png")) if (file.exists(file.path(assets, logo))) file.copy(file.path(assets, logo), file.path(carpeta, logo), overwrite = FALSE)
  tarjetas <- lapply(seq_len(nrow(tabla)), function(i) htmltools::tags$article(class = "producto",
    htmltools::tags$span(class = "etiqueta", sprintf("PRODUCTO %02d", i)), htmltools::tags$h2(tabla$Producto[i]), htmltools::tags$p(tabla$Archivo_TIFF[i]),
    if (nzchar(tabla$Visor_HTML[i])) htmltools::tags$a(href = tabla$Visor_HTML[i], "Abrir mapa interactivo →") else htmltools::tags$p(class = "error", tabla$Estado[i])))
  portada <- htmltools::tags$html(lang = "es", htmltools::tags$head(htmltools::tags$meta(charset = "UTF-8"), htmltools::tags$meta(name = "viewport", content = "width=device-width,initial-scale=1"),
    htmltools::tags$title("PhenoImage · Productos de fenotipado"), htmltools::tags$style(htmltools::HTML("body{margin:0;background:#f2f5ef;color:#28442f;font:14px/1.6 'Segoe UI',Arial,sans-serif}main{max-width:1100px;margin:auto;padding:32px}header{display:flex;align-items:center;gap:24px;border-bottom:1px solid #d5e0cf;padding-bottom:22px}header img{max-width:160px;max-height:65px}h1{font-size:30px;margin:25px 0 8px}.catalogo{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:18px}.producto{background:white;padding:22px;border:1px solid #dbe4d6;border-top:3px solid #27833e;border-radius:5px}.producto h2{font-size:20px;margin:10px 0}.etiqueta{font-size:9px;letter-spacing:1.5px;color:#718565}.producto p{font-size:12px;color:#66775d;overflow-wrap:anywhere}a{display:inline-block;background:#27833e;color:white;text-decoration:none;padding:8px 12px;border-radius:4px}.nota{color:#6c7f61;font-size:12px}.error{color:#9c503a!important}"))),
    htmltools::tags$body(htmltools::tags$main(htmltools::tags$header(
      if (file.exists(file.path(carpeta, "cipat.png"))) htmltools::tags$img(src = "cipat.png", alt = "CIPAT"),
      if (file.exists(file.path(carpeta, "papalotla.png"))) htmltools::tags$img(src = "papalotla.png", alt = "Grupo Papalotla")),
      htmltools::tags$h1("Productos de fenotipado"), htmltools::tags$p(paste(x$fuentes$lote, x$fuentes$fecha, nombre_modulo_cipat(x$tipo), sep = " · ")),
      htmltools::tags$p(class = "nota", "Mapas de visualización reducida. Conserva esta carpeta junto con recursos. Los GeoTIFF y el Excel originales son las salidas de análisis."),
      htmltools::tags$div(class = "catalogo", tarjetas))))
  writeLines(c("<!DOCTYPE html>", as.character(portada)), file.path(carpeta, "index.html"), useBytes = TRUE)
  zipfile <- archivo_salida_cipat(x$fuentes, "Visores_interactivos", "zip")
  if (file.exists(zipfile)) zipfile <- archivo_salida_cipat(x$fuentes, paste0("Visores_interactivos_v", sprintf("%02d", version - 1L)), "zip")
  archivos_visor <- list.files(carpeta, all.files = TRUE, no.. = TRUE)
  zip::zipr(zipfile, files = archivos_visor, root = carpeta, recurse = TRUE)
  list(ok = any(tabla$Estado == "Creado"), tipo = x$tipo, excel = x$excel, carpeta = carpeta, zip = zipfile,
    creados = sum(tabla$Estado == "Creado"), total = nrow(tabla), omitidos = sum(tabla$Estado != "Creado"))
}

recuperar_visores_cipat <- function(x) {
  carpetas <- list.dirs(x$salida, recursive = FALSE, full.names = TRUE)
  carpetas <- carpetas[grepl("^Visores_interactivos", basename(carpetas))]
  if (length(carpetas)) for (carpeta in rev(sort(carpetas))) {
    archivo <- file.path(carpeta, "Resumen_visores.csv")
    if (!file.exists(archivo)) next
    t <- tryCatch(utils::read.csv(archivo, fileEncoding = "UTF-8", stringsAsFactors = FALSE), error = function(e) NULL)
    z <- list.files(x$salida, pattern = "^Visores_interactivos.*\\.zip$", full.names = TRUE)
    if (!is.null(t) && "Estado" %in% names(t) && length(z)) return(list(ok = any(t$Estado == "Creado"), tipo = x$tipo,
      excel = x$excel, carpeta = carpeta, zip = z[which.max(file.info(z)$mtime)], creados = sum(t$Estado == "Creado"), total = nrow(t), omitidos = sum(t$Estado != "Creado")))
  }
  archivo_plano <- file.path(x$salida, "Resumen_visores.csv")
  if (file.exists(archivo_plano)) {
    t <- utils::read.csv(archivo_plano, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    z <- list.files(x$salida, pattern = "^Visores_interactivos.*\\.zip$", full.names = TRUE)
    if ("Estado" %in% names(t) && length(z)) return(list(ok = any(t$Estado == "Creado"), tipo = x$tipo,
      excel = x$excel, carpeta = x$salida, zip = z[1], creados = sum(t$Estado == "Creado"), total = nrow(t), omitidos = sum(t$Estado != "Creado")))
  }
  zips <- list.files(x$salida, pattern = "^Visores_interactivos.*\\.zip$", full.names = TRUE)
  if (!length(zips)) return(NULL)
  zips <- zips[order(file.info(zips)$mtime, decreasing = TRUE)]
  for (ruta in zips) {
    carpeta <- sub("\\.zip$", "", ruta)
    archivo <- file.path(carpeta, "Resumen_visores.csv")
    if (!file.exists(archivo)) next
    t <- tryCatch(utils::read.csv(archivo, fileEncoding = "UTF-8", stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(t) || !"Estado" %in% names(t)) next
    return(list(ok = any(t$Estado == "Creado"), tipo = x$tipo, excel = x$excel, carpeta = carpeta, zip = ruta,
      creados = sum(t$Estado == "Creado"), total = nrow(t), omitidos = sum(t$Estado != "Creado")))
  }
  NULL
}
