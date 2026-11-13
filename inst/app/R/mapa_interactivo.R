# Visor local de parcelas y copias reducidas de los TIFF. No recalcula HTP.
# Referencias: https://rstudio.github.io/leaflet/articles/shiny.html
# https://rstudio.github.io/leaflet/reference/addRasterImage.html
campo_mapa_cipat <- function(grupos, candidatos) {
  normalizados <- tolower(gsub("[^[:alnum:]]", "", names(grupos)))
  for (nombre in candidatos) {
    i <- match(nombre, normalizados)
    if (!is.na(i)) return(unname(grupos[i]))
  }
  "__ninguno__"
}

preparar_mapa_cipat <- function(x, datos) {
  ruta <- x$fuentes$polys
  if (is.null(ruta) || !nzchar(ruta) || !file.exists(ruta))
    stop("No se encontró el archivo de parcelas en la ruta registrada en el Excel. Conserva el SHP con sus archivos auxiliares, o el GPKG, en esa ruta.")
  p <- sf::st_read(ruta, quiet = TRUE)
  if (!x$fuentes$id %in% names(p)) stop("El archivo de parcelas no contiene el campo ID registrado: ", x$fuentes$id)
  if (is.na(sf::st_crs(p))) stop("El archivo de parcelas no tiene un CRS definido. Asígnale su CRS correcto en QGIS.")
  ids <- as.character(p[[x$fuentes$id]])
  if (anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids)) stop("El ID del archivo de parcelas debe ser único y no estar vacío.")
  if (!all(as.character(sf::st_geometry_type(p)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("El visor necesita polígonos de parcelas.")
  p <- sf::st_zm(p, drop = TRUE, what = "ZM")
  p <- sf::st_make_valid(p)
  conservar <- !sf::st_is_empty(p)
  omitidas <- sum(!conservar)
  p <- p[conservar, , drop = FALSE]; ids <- ids[conservar]
  if (!nrow(p)) stop("No hay geometrías de parcelas disponibles.")
  if (!all(as.character(sf::st_geometry_type(p)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("Revisa las geometrías de parcelas en QGIS; la reparación no produjo únicamente polígonos.")
  d <- datos$tabla
  if (anyNA(d$id) || anyDuplicated(as.character(d$id))) stop("Los ID de la tabla de resultados deben ser únicos.")
  posicion <- match(ids, as.character(d$id))
  d <- d[posicion, , drop = FALSE]
  # Una parcela del SHP sin fila HTP se mantiene visible, con valores NA.
  # Los atributos del SHP completan la ficha sin sustituir los guardados en HTP.
  for (k in setdiff(names(sf::st_drop_geometry(p)), x$fuentes$id)) {
    nuevo <- paste0("Parcela_", k)
    if (!nuevo %in% names(d) && is.atomic(p[[k]]) && !is.matrix(p[[k]])) d[[nuevo]] <- p[[k]]
  }
  p <- sf::st_transform(p, 4326)
  b <- sf::st_bbox(p)
  if (any(!is.finite(b)) || b["xmin"] < -180 || b["xmax"] > 180 || b["ymin"] < -85 || b["ymax"] > 85)
    stop("Las coordenadas transformadas no son válidas para el mapa. Revisa el CRS de parcelas.")
  grupos <- names(d)[startsWith(names(d), "Parcela_")]
  grupos <- grupos[vapply(d[grupos], function(v) is.atomic(v) && !is.matrix(v), logical(1))]
  list(ok = TRUE, poligonos = p, ids = ids, tabla = d, limites = b, sin_resultado = sum(is.na(posicion)),
    omitidas = omitidas, grupos = stats::setNames(grupos, sub("^Parcela_", "", grupos)))
}

valor_mapa_cipat <- function(v) {
  if (!length(v) || is.na(v[1])) return("Sin datos")
  if (is.numeric(v)) {
    if (!is.finite(v[1])) return("Sin datos")
    return(format(signif(v[1], 5), trim = TRUE, scientific = FALSE))
  }
  if (!nzchar(trimws(as.character(v[1])))) "Sin datos" else as.character(v[1])
}

identificacion_mapa_cipat <- function(p, campo = "__id__") {
  # Los valores del selector siguen siendo ID únicos; el genotipo es una etiqueta.
  if (is.null(campo) || !campo %in% names(p$tabla)) return(paste("Parcela", p$ids))
  valores <- vapply(seq_along(p$ids), function(i) valor_mapa_cipat(p$tabla[[campo]][i]), character(1))
  ifelse(valores == "Sin datos", paste("Parcela", p$ids), paste(valores, "· Parcela", p$ids))
}

popup_mapa_cipat <- function(id, valor, variable, genotipo, repeticion, lote, fecha) {
  fila <- function(nombre, contenido) shiny::tags$tr(shiny::tags$th(nombre), shiny::tags$td(contenido))
  # Las etiquetas de htmltools escapan todos los contenidos provenientes del SHP/Excel.
  as.character(shiny::tags$div(class = "cipat-popup",
    shiny::tags$div(class = "popup-kicker", "PhenoImage · FICHA DE PARCELA"),
    shiny::tags$h4(paste("Parcela", id)),
    shiny::tags$table(fila("Genotipo", genotipo), fila("Repetición", repeticion)),
    shiny::tags$div(class = "popup-trait", shiny::tags$span(variable), shiny::tags$strong(valor_mapa_cipat(valor))),
    shiny::tags$div(class = "popup-meta", paste(lote, fecha, sep = " · "))))
}

paleta_mapa_cipat <- function(valores) {
  finitos <- valores[is.finite(valores)]
  dominio <- if (length(finitos)) range(finitos) else c(0, 1)
  if (dominio[1] == dominio[2]) dominio <- dominio + c(-1, 1) * max(1e-6, abs(dominio[1]) * .001)
  leaflet::colorNumeric(grDevices::hcl.colors(80, "Viridis"), domain = dominio, na.color = "#c5cbd0")
}

preparar_imagen_cipat <- function(x, capa, p, limite = 800L, bandas_rgb = NULL) {
  if (is.null(capa) || identical(capa, "__ninguna__")) return(list(ok = TRUE, imagen = NULL))
  if (!capa %in% names(x$capas)) stop("La capa seleccionada no está disponible.")
  ruta <- x$capas[[capa]]
  if (!file.exists(ruta)) stop("No se encontró el TIFF de la capa seleccionada.")
  r <- terra::rast(ruta)
  if (!nzchar(terra::crs(r))) stop("El TIFF no tiene CRS definido.")
  rgb <- identical(capa, "RGB"); mascara <- startsWith(capa, "Máscara")
  if (rgb && terra::nlyr(r) < 3L) stop("El ortomosaico RGB necesita al menos tres bandas.")
  bandas <- terra::nlyr(r)
  if (rgb && is.null(bandas_rgb)) bandas_rgb <- bandas_rgb_resultado_cipat(x, bandas)
  r <- if (rgb) r[[bandas_rgb]] else r[[1]]
  # Reducir solo para visualizar; no recortar la extensión a las parcelas.
  reducir <- function(imagen) {
    factor <- max(1L, ceiling(max(nrow(imagen), ncol(imagen)) / limite))
    if (factor > 1L) terra::aggregate(imagen, fact = factor, fun = if (mascara) "modal" else "mean", na.rm = TRUE) else imagen
  }
  r <- reducir(r)
  if (rgb) {
    # Contraste solo de visualización, sobre la copia reducida y sin escribir el TIFF.
    r <- terra::stretch(r, minv = 0, maxv = 255, minq = 0, maxq = 1)
    terra::RGB(r) <- 1:3
  }
  r <- leaflet::projectRasterForLeaflet(r, method = if (mascara) "ngb" else "bilinear")
  r <- reducir(r)
  if (rgb) {
    r <- terra::clamp(r, 0, 255)
    terra::RGB(r) <- 1:3
    pal <- NULL
  } else {
    v <- as.numeric(terra::values(r)); v <- v[is.finite(v)]
    dominio <- if (mascara) c(0, 1) else if (identical(capa, "NDVI")) c(-1, 1) else if (length(v)) range(v) else c(0, 1)
    if (diff(dominio) == 0) dominio <- dominio + c(-1, 1) * max(1e-6, abs(dominio[1]) * .001)
    colores <- if (mascara) c("#cdbd9b", "#267543") else if (identical(capa, "NDVI")) grDevices::hcl.colors(80, "RdYlGn") else grDevices::hcl.colors(80, "Viridis")
    pal <- leaflet::colorNumeric(colores, domain = dominio, na.color = "#00000000")
  }
  list(ok = TRUE, imagen = r, paleta = pal, rgb = rgb, capa = capa, bandas = bandas, sin_datos = if (rgb) FALSE else !length(v))
}

registrar_mapa_cipat <- function(input, output, session, ultima, datos) {
  parcelas <- shiny::reactive({
    shiny::req(ultima())
    tryCatch(preparar_mapa_cipat(ultima(), datos()), error = function(e) list(ok = FALSE, mensaje = conditionMessage(e)))
  })
  output$mapa_controles <- shiny::renderUI({
    p <- parcelas()
    if (!isTRUE(p$ok)) return(shiny::div(class = "tip", p$mensaje))
    x <- ultima(); opciones <- variables_eda(p$tabla)
    if (!length(opciones)) return(shiny::div(class = "tip", "No hay variables numéricas disponibles para colorear las parcelas."))
    inicial <- if (x$tipo == "phantom") "Canopy_height_image_p90" else "NDVI_mean"
    if (!inicial %in% unname(opciones)) inicial <- unname(opciones[1])
    fondo <- if ("RGB" %in% names(x$capas)) "RGB" else "__ninguna__"
    campo_etiqueta <- campo_mapa_cipat(p$grupos, c("originali", "original1", "genotipo", "genotype", "material"))
    if (identical(campo_etiqueta, "__ninguno__")) campo_etiqueta <- "__id__"
    shiny::tagList(
      shiny::selectInput("mapa_variable", "Variable por parcela", opciones, selected = inicial),
      shiny::selectInput("mapa_fondo", "Imagen de fondo", c("Sin imagen" = "__ninguna__", stats::setNames(names(x$capas), names(x$capas))), selected = fondo),
      shiny::checkboxInput("mapa_colorear", "Colorear parcelas por valor", TRUE),
      shiny::selectInput("mapa_etiqueta", "Identificar parcelas con", c("ID de parcela" = "__id__", p$grupos), selected = campo_etiqueta),
      shiny::checkboxInput("mapa_etiquetas_fijas", "Mostrar etiquetas sobre las parcelas", FALSE),
      shiny::selectInput("mapa_buscar", "Buscar genotipo / parcela", c("Todo el ensayo" = "", stats::setNames(p$ids, identificacion_mapa_cipat(p, campo_etiqueta))), selected = ""),
      shiny::actionButton("mapa_centrar", "Centrar ensayo", icon = shiny::icon("crosshairs")),
      shiny::actionButton("mapa_recargar", "Actualizar mapa", icon = shiny::icon("rotate")),
      ayuda_contextual("Campos de identificación",
        shiny::selectInput("mapa_genotipo", "Campo de genotipo", c("Sin definir" = "__ninguno__", p$grupos),
          selected = campo_mapa_cipat(p$grupos, c("genotipo", "genotype", "originali", "original1", "material"))),
        shiny::selectInput("mapa_repeticion", "Campo de repetición", c("Sin definir" = "__ninguno__", p$grupos),
          selected = campo_mapa_cipat(p$grupos, c("rep", "repeticion", "repetición", "repetition", "bloque", "block")))),
      ayuda_breve("Selecciona Original_I u otro atributo para identificar las parcelas. Puedes buscar por genotipo y mostrar sus etiquetas en el mapa. El ID conserva la identificación única de cada repetición. La escala representa valores por parcela, no medias por genotipo."))
  })
  shiny::observe({
    p <- parcelas(); shiny::req(isTRUE(p$ok), input$mapa_etiqueta)
    etiquetas <- identificacion_mapa_cipat(p, input$mapa_etiqueta)
    seleccionado <- shiny::isolate(input$mapa_buscar)
    if (is.null(seleccionado) || !seleccionado %in% p$ids) seleccionado <- ""
    shiny::updateSelectInput(session, "mapa_buscar", choices = c("Todo el ensayo" = "", stats::setNames(p$ids, etiquetas)), selected = seleccionado)
  })
  fondo_activo <- shiny::reactive({
    x <- ultima(); shiny::req(x)
    fondo <- input$mapa_fondo
    if (is.null(fondo)) fondo <- if ("RGB" %in% names(x$capas)) "RGB" else "__ninguna__"
    fondo
  })
  imagen <- shiny::reactive({
    p <- parcelas(); shiny::req(isTRUE(p$ok))
    tryCatch(preparar_imagen_cipat(ultima(), fondo_activo(), p$poligonos),
      error = function(e) list(ok = FALSE, mensaje = conditionMessage(e)))
  })
  error_imagen <- shiny::reactiveVal(NULL)
  error_mapa <- shiny::reactiveVal(NULL)
  # Guarda la vista sin crear una dependencia reactiva del movimiento del mapa.
  vista <- new.env(parent = emptyenv())
  vista$clave <- NULL; vista$busqueda <- NULL; vista$centrar <- NULL; vista$recargar <- NULL
  output$mapa_parcelas <- leaflet::renderLeaflet({
    p <- parcelas(); shiny::validate(shiny::need(isTRUE(p$ok), if (isTRUE(p$ok)) "" else p$mensaje))
    x <- ultima(); r <- imagen(); opciones <- variables_eda(p$tabla)
    shiny::validate(shiny::need(length(opciones) > 0L, "No hay variables numéricas para el mapa."))
    variable <- input$mapa_variable
    if (is.null(variable) || !variable %in% unname(opciones)) {
      variable <- if (x$tipo == "phantom") "Canopy_height_image_p90" else "NDVI_mean"
      if (!variable %in% unname(opciones)) variable <- unname(opciones[1])
    }
    campo_etiqueta <- input$mapa_etiqueta
    if (is.null(campo_etiqueta)) campo_etiqueta <- campo_mapa_cipat(p$grupos, c("originali", "original1", "genotipo", "genotype", "material"))
    genotipo <- input$mapa_genotipo
    if (is.null(genotipo)) genotipo <- campo_mapa_cipat(p$grupos, c("genotipo", "genotype", "originali", "original1", "material"))
    repeticion <- input$mapa_repeticion
    if (is.null(repeticion)) repeticion <- campo_mapa_cipat(p$grupos, c("rep", "repeticion", "repetición", "repetition", "bloque", "block"))
    colorear <- is.null(input$mapa_colorear) || isTRUE(input$mapa_colorear)
    fijas <- isTRUE(input$mapa_etiquetas_fijas)
    buscar <- if (is.null(input$mapa_buscar)) "" else input$mapa_buscar
    centrar <- input$mapa_centrar; recargar <- input$mapa_recargar; clave <- x$excel
    b <- p$limites
    nueva <- !identical(vista$clave, clave)
    centro <- !identical(vista$centrar, centrar) || !identical(vista$recargar, recargar)
    cambio_busqueda <- !identical(vista$busqueda, buscar)
    if (!nueva && !centro && cambio_busqueda && nzchar(buscar) && buscar %in% p$ids)
      b <- sf::st_bbox(p$poligonos[match(buscar, p$ids), , drop = FALSE])
    if (!nueva && !centro && !cambio_busqueda) {
      limites <- shiny::isolate(input$mapa_parcelas_bounds)
      if (!is.null(limites)) {
        actual <- unlist(limites[c("west", "south", "east", "north")], use.names = FALSE)
        if (length(actual) == 4L && all(is.finite(actual)) && actual[1] < actual[3] && actual[2] < actual[4])
          b <- stats::setNames(actual, c("xmin", "ymin", "xmax", "ymax"))
      }
    }
    error_imagen(NULL); error_mapa(NULL)
    tryCatch({
      # Construye todas las capas en el widget: no dependen de mensajes proxy iniciales.
      m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1, maxZoom = 24, zoomSnap = .5))
      m <- leaflet::addScaleBar(m, position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))
      if (isTRUE(r$ok) && !is.null(r$imagen)) m <- tryCatch(
        leaflet::addRasterImage(m, r$imagen, colors = if (r$rgb) "Spectral" else r$paleta,
          opacity = .9, project = FALSE, group = "Imagen UAV", layerId = "imagen_uav", maxBytes = 4 * 1024 * 1024),
        error = function(e) { error_imagen(conditionMessage(e)); m })
      v <- p$tabla[[variable]]; v[!is.finite(v)] <- NA_real_
      pal <- paleta_mapa_cipat(v); etiqueta <- etiqueta_eda(variable)
      campo <- function(nombre, i) if (!is.null(nombre) && nombre %in% names(p$tabla)) valor_mapa_cipat(p$tabla[[nombre]][i]) else "Sin definir"
      fichas <- vapply(seq_along(p$ids), function(i) popup_mapa_cipat(p$ids[i], v[i], etiqueta,
        campo(genotipo, i), campo(repeticion, i), x$fuentes$lote, x$fuentes$fecha), character(1))
      identificacion <- identificacion_mapa_cipat(p, campo_etiqueta)
      etiquetas <- if (fijas) identificacion else paste(identificacion, vapply(v, valor_mapa_cipat, character(1)), sep = " · ")
      m <- leaflet::addPolygons(m, data = p$poligonos, layerId = p$ids, group = "Parcelas",
        color = "#33543b", weight = 1.5, fillColor = pal(v), fillOpacity = if (colorear) .64 else 0,
        popup = fichas, popupOptions = leaflet::popupOptions(maxWidth = 320), label = etiquetas,
        labelOptions = leaflet::labelOptions(permanent = fijas, sticky = !fijas, direction = "center", textOnly = fijas, className = "cipat-parcel-label"),
        highlightOptions = leaflet::highlightOptions(color = "#e4b54f", weight = 3, bringToFront = TRUE))
      if (colorear && any(is.finite(v))) m <- leaflet::addLegend(m, pal = pal, values = v,
        title = as.character(shiny::tags$span(etiqueta)), position = "bottomright", layerId = "leyenda_parcelas", na.label = "Sin datos")
      else if (colorear) m <- leaflet::addLegend(m, colors = "#c5cbd0", labels = "Sin datos",
        title = as.character(shiny::tags$span(etiqueta)), position = "bottomright", layerId = "leyenda_parcelas")
      if (isTRUE(r$ok) && !is.null(r$imagen) && !r$rgb && !colorear && !isTRUE(r$sin_datos) && is.null(shiny::isolate(error_imagen())))
        m <- leaflet::addLegend(m, pal = r$paleta, values = as.numeric(terra::values(r$imagen)),
          title = as.character(shiny::tags$span(paste("Imagen ·", r$capa))), position = "bottomright", layerId = "leyenda_imagen", na.label = "Sin datos")
      m <- leaflet::fitBounds(m, unname(b["xmin"]), unname(b["ymin"]), unname(b["xmax"]), unname(b["ymax"]))
      vista$clave <- clave; vista$busqueda <- buscar; vista$centrar <- centrar; vista$recargar <- recargar
      m
    }, error = function(e) {
      error_mapa(conditionMessage(e))
      shiny::validate(shiny::need(FALSE, paste("No se pudo construir el mapa:", conditionMessage(e))))
    })
  })
  output$mapa_aviso <- shiny::renderUI({
    p <- parcelas(); if (!isTRUE(p$ok)) return(NULL)
    r <- imagen(); avisos <- character()
    if (!isTRUE(r$ok)) avisos <- c(avisos, paste("Imagen no disponible:", r$mensaje))
    if (isTRUE(r$sin_datos)) avisos <- c(avisos, "La vista reducida de la imagen no tiene píxeles válidos.")
    if (!is.null(error_imagen())) avisos <- c(avisos, paste("Imagen no disponible:", error_imagen()))
    if (!is.null(error_mapa())) avisos <- c(avisos, paste("Mapa no disponible:", error_mapa()))
    if (!is.null(input$mapa_error_cliente) && nzchar(input$mapa_error_cliente))
      avisos <- c(avisos, paste("Error del visor en el navegador:", input$mapa_error_cliente, "Pulsa Actualizar mapa."))
    if (p$sin_resultado) avisos <- c(avisos, paste(p$sin_resultado, "parcelas del vector no tienen fila en la tabla HTP."))
    if (p$omitidas) avisos <- c(avisos, paste(p$omitidas, "geometrías vacías omitidas del visor."))
    if (length(avisos)) shiny::div(class = "tip", paste(avisos, collapse = " ")) else NULL
  })
  shiny::outputOptions(output, "mapa_parcelas", suspendWhenHidden = FALSE)
}
