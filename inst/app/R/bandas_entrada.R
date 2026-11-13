# Inspección de bandas para las vistas de entrada. No escribe ni calcula HTP.
paletas_banda_cipat <- function() {
  c("Escala de grises" = "grises", "Viridis" = "viridis", "Terreno" = "terreno")
}

informacion_bandas_cipat <- function(ruta, capa, mapa = NULL) {
  r <- terra::rast(ruta)
  if (!nzchar(terra::crs(r))) stop("El TIFF necesita un CRS definido para visualizarse.")
  n <- terra::nlyr(r)
  originales <- names(r)
  etiquetas <- paste("Banda", seq_len(n))
  if (!is.null(mapa)) {
    canales <- c(R = "Rojo (R)", G = "Verde (G)", B = "Azul (B)", RE = "Red Edge (RE)", NIR = "Infrarrojo cercano (NIR)")
    for (k in names(mapa)) {
      i <- mapa[[k]]
      if (is.finite(i) && i >= 1 && i <= n) etiquetas[i] <- paste(etiquetas[i], canales[[k]], sep = " · ")
    }
  }
  # Los canales sin función definida conservan su número y, si existe, su nombre.
  for (i in seq_len(n)) {
    nombre <- originales[i]
    generico <- !length(nombre) || is.na(nombre) || !nzchar(nombre) || grepl("^(band|banda|lyr|layer)[_. ]*[0-9]+$", nombre, ignore.case = TRUE)
    if (identical(etiquetas[i], paste("Banda", i)) && !generico) etiquetas[i] <- paste(etiquetas[i], nombre, sep = " · ")
  }
  list(ok = TRUE, ruta = ruta, capa = capa, bandas = n,
    rgb = identical(capa, "RGB") && n >= 3L,
    opciones = stats::setNames(as.character(seq_len(n)), etiquetas), etiquetas = etiquetas)
}

preparar_banda_entrada_cipat <- function(info, banda, parcelas = NULL, paleta = "grises", limite = 800L) {
  if (length(banda) != 1L || !is.numeric(banda) || !is.finite(banda) || banda != floor(banda) || banda < 1L || banda > info$bandas) stop("Selecciona una banda disponible en el archivo.")
  if (length(paleta) != 1L || !paleta %in% unname(paletas_banda_cipat())) stop("Selecciona una paleta disponible.")
  r <- terra::rast(info$ruta)[[as.integer(banda)]]
  if (!is.null(parcelas)) r <- terra::crop(r, terra::vect(sf::st_buffer(sf::st_transform(parcelas, terra::crs(r)), dist = 2)), snap = "out")
  reducir <- function(x) {
    factor <- max(1L, ceiling(max(nrow(x), ncol(x)) / limite))
    if (factor > 1L) terra::aggregate(x, fact = factor, fun = "mean", na.rm = TRUE) else x
  }
  r <- reducir(r)
  r <- reducir(leaflet::projectRasterForLeaflet(r, method = "bilinear"))
  v <- as.numeric(terra::values(r)); v <- v[is.finite(v)]
  dominio <- if (length(v)) range(v) else c(0, 1)
  if (diff(dominio) == 0) dominio <- dominio + c(-1, 1) * max(1e-6, abs(dominio[1]) * .001)
  colores <- switch(paleta, grises = grDevices::gray.colors(80, start = 0, end = 1),
    viridis = grDevices::hcl.colors(80, "Viridis"), terreno = grDevices::terrain.colors(80))
  list(ok = TRUE, imagen = r, paleta = leaflet::colorNumeric(colores, domain = dominio, na.color = "#00000000"),
    rgb = FALSE, capa = info$capa, bandas = info$bandas, banda = as.integer(banda),
    titulo = info$etiquetas[banda], sin_datos = !length(v), dominio = dominio, nombre_paleta = paleta)
}

registrar_bandas_entrada_cipat <- function(input, output, session, capas, campo, prefijo, recorte = shiny::reactive(NULL), mapas_bandas = shiny::reactive(list())) {
  informacion <- shiny::reactive({
    capa <- input[[campo]]
    if (identical(capa, "__ninguna__")) return(list(ok = TRUE, sin_imagen = TRUE))
    archivos <- capas()
    if (is.null(capa) || !capa %in% names(archivos)) return(list(ok = FALSE, mensaje = "Importa y selecciona una imagen para visualizarla."))
    clave <- if (identical(capa, "RGB")) clave_rgb_cipat(list(tipo = input$tipo, origen_sentera = input$origen_sentera)) else if (grepl("RE/NIR", capa, fixed = TRUE)) "ms" else NULL
    mapa <- if (!is.null(clave)) mapas_bandas()[[clave]] else NULL
    tryCatch({
      info <- informacion_bandas_cipat(archivos[[capa]], capa, mapa)
      if (!is.null(clave)) info$mapa <- validar_asignacion_bandas_cipat(mapa, info$bandas, clave)
      info
    }, error = function(e) list(ok = FALSE, mensaje = conditionMessage(e)))
  })
  vista_ui<-new.env(parent=emptyenv());vista_ui$ruta<-NULL
  output[[paste0(prefijo, "_bandas")]] <- shiny::renderUI({
    info <- informacion()
    if (!isTRUE(info$ok) || isTRUE(info$sin_imagen)) return(NULL)
    nuevo_archivo<-!identical(vista_ui$ruta,info$ruta)
    vista_ui$ruta<-info$ruta
    modo <- if(nuevo_archivo) NULL else shiny::isolate(input[[paste0(prefijo, "_modo")]])
    if (is.null(modo) || !modo %in% c("rgb", "banda")) modo <- if (info$rgb) "rgb" else "banda"
    if (!info$rgb) modo <- "banda"
    banda <- shiny::isolate(input[[paste0(prefijo, "_banda")]])
    if (is.null(banda) || !banda %in% unname(info$opciones)) banda <- "1"
    paleta <- shiny::isolate(input[[paste0(prefijo, "_paleta")]])
    if (is.null(paleta) || !paleta %in% unname(paletas_banda_cipat())) paleta <- "grises"
    shiny::div(class = "band-view-controls",
      shiny::tabsetPanel(id = paste0(prefijo, "_modo"), selected = modo,
        if (info$rgb) shiny::tabPanel("Composición RGB", value = "rgb",
          shiny::tags$p(class = "ib-note", "Composición de las bandas rojo, verde y azul.")),
        shiny::tabPanel("Bandas individuales", value = "banda",
          shiny::selectInput(paste0(prefijo, "_banda"), "Banda a visualizar", choices = info$opciones, selected = banda),
          shiny::selectInput(paste0(prefijo, "_paleta"), "Paleta de visualización", choices = paletas_banda_cipat(), selected = paleta),
          shiny::tags$p(class = "ib-note", paste(info$bandas, "bandas disponibles. La escala utiliza los valores de la banda seleccionada.")))))
  })
  imagen <- shiny::reactive({
    info <- informacion()
    if (!isTRUE(info$ok)) return(info)
    if (isTRUE(info$sin_imagen)) return(list(ok = TRUE, imagen = NULL))
    modo <- input[[paste0(prefijo, "_modo")]]
    if (is.null(modo)) modo <- if (info$rgb) "rgb" else "banda"
    tryCatch({
      if (info$rgb && identical(modo, "rgb")) {
        preparar_imagen_cipat(list(capas = capas()), info$capa, recorte(), bandas_rgb = unname(info$mapa[c("R", "G", "B")]))
      } else {
        banda <- suppressWarnings(as.numeric(input[[paste0(prefijo, "_banda")]]))
        if (length(banda) != 1L || !is.finite(banda) || banda != floor(banda) || !as.character(banda) %in% unname(info$opciones)) banda <- 1L
        paleta <- input[[paste0(prefijo, "_paleta")]]
        if (is.null(paleta) || !paleta %in% unname(paletas_banda_cipat())) paleta <- "grises"
        preparar_banda_entrada_cipat(info, banda, recorte(), paleta)
      }
    }, error = function(e) list(ok = FALSE, mensaje = conditionMessage(e)))
  })
  list(informacion = informacion, imagen = imagen)
}
