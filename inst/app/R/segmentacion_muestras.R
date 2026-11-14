# E28. Clasificador de distancia mínima a prototipos RGB por zona de entrenamiento.
# Muestras a resolución original; vista reducida exclusivamente para visualizar.
firma_muestras_cipat <- function(ruta, bandas) {
  f <- file.info(ruta)
  paste(normalizePath(ruta, winslash = "/", mustWork = TRUE), f$size,
    as.numeric(f$mtime), paste(bandas, collapse = ","), sep = "|")
}

# Códigos estables: 0=suelo, 1=verde, 2=material vegetal seco.
entrenar_muestras_cipat <- function(d, incluir_seco = !is.null(d) && any(d$clase == 2L)) {
  if (!all(c("R", "G", "B", "clase", "zona") %in% names(d))) stop("Muestras incompletas.")
  requeridas <- if (isTRUE(incluir_seco)) c(0L, 1L, 2L) else c(0L, 1L)
  z <- as.matrix(d[c("R", "G", "B")])
  if (!nrow(z) || any(!is.finite(z)) || anyNA(d$clase) || anyNA(d$zona) || any(!d$clase %in% requeridas)) stop("Muestras inválidas para las clases seleccionadas.")
  for (k in requeridas) if (length(unique(d$zona[d$clase == k])) < 2L)
    stop("Marca al menos dos zonas de cada clase seleccionada: verde y suelo", if (isTRUE(incluir_seco)) ", y material vegetal seco." else ". No se requieren muestras secas.")
  centro <- colMeans(z); escala <- apply(z, 2, stats::sd)
  escala <- pmax(escala, max(abs(z), 1) * 1e-6)
  grupos <- split(seq_len(nrow(d)), d$zona)
  if (any(vapply(grupos, function(i) length(unique(d$clase[i])) != 1L, logical(1)))) stop("Una zona tiene clases contradictorias.")
  prototipos <- t(vapply(grupos, function(i) colMeans(z[i, , drop = FALSE]), numeric(3)))
  clases <- vapply(grupos, function(i) as.integer(d$clase[i[1]]), integer(1))
  for (i in seq_along(clases)) for (j in seq_along(clases))
    if (clases[i] != clases[j] && sum(((prototipos[i, ] - prototipos[j, ]) / escala)^2) < 1e-10)
      stop("Hay muestras de clases diferentes con el mismo color. Revisa esas zonas.")
  list(centro = centro, escala = escala,
    prototipos = sweep(sweep(prototipos, 2, centro), 2, escala, "/"), clases = clases,
    incluir_seco = isTRUE(incluir_seco),
    metodo = "Distancia mínima a prototipos de zonas; RGB estandarizado; prioridad en empate: suelo, seco, verde", version = 2L)
}

predecir_muestras_cipat <- function(v, modelo) {
  if (is.null(dim(v))) v <- matrix(v, ncol = 3L, byrow = TRUE)
  if (ncol(v) != 3L) stop("La clasificación requiere tres canales RGB.")
  salida <- rep(NA_real_, nrow(v)); ok <- rowSums(is.finite(v)) == 3L
  if (!any(ok)) return(salida)
  z <- sweep(sweep(v[ok, , drop = FALSE], 2, modelo$centro), 2, modelo$escala, "/")
  orden <- c(0L, 2L, 1L); orden <- orden[orden %in% modelo$clases]
  distancias <- vapply(orden, function(k) {
    mejor <- rep(Inf, nrow(z))
    for (i in which(modelo$clases == k)) {
      delta <- sweep(z, 2, modelo$prototipos[i, ])
      mejor <- pmin(mejor, rowSums(delta * delta))
    }
    mejor
  }, numeric(nrow(z)))
  distancias <- matrix(distancias, nrow = nrow(z), ncol = length(orden))
  salida[ok] <- orden[max.col(-distancias, ties.method = "first")]
  salida
}

# Un único denominador válido, ponderado por fracción de píxel.
resumir_clases_cobertura_cipat <- function(values, coverage_fraction, incluir_seco, area_pixel, area_parcela) {
  v <- if (is.matrix(values) || is.data.frame(values)) values[, 1] else values
  ok <- is.finite(v) & is.finite(coverage_fraction) & coverage_fraction > 0
  v <- v[ok]; w <- coverage_fraction[ok]
  if (any(!v %in% (if (incluir_seco) 0:2 else 0:1))) stop("Códigos de clase inválidos.")
  total <- sum(w); verde <- sum(w[v == 1]); suelo <- sum(w[v == 0])
  seco <- if (incluir_seco) sum(w[v == 2]) else NA_real_
  cubierto <- verde + if (incluir_seco) seco else 0
  porcentaje <- function(n) if (total > 0) 100 * n / total else NA_real_
  q <- data.frame(Green_pct = porcentaje(verde), Dry_pct = porcentaje(seco),
    Bare_soil_pct = porcentaje(suelo), Total_pct = porcentaje(cubierto),
    Green_veg_area_m2 = verde * area_pixel, Dry_veg_area_m2 = seco * area_pixel,
    Bare_soil_area_m2 = suelo * area_pixel, Total_veg_area_m2 = cubierto * area_pixel,
    polygon_area_m2 = area_parcela,
    valid_area_pct = if (area_parcela > 0) 100 * total * area_pixel / area_parcela else NA_real_)
  names(q) <- paste0("Plant_ground_cover_image_", names(q)); q
}

resumen_clases_cobertura_cipat <- function(clases, poligonos, incluir_seco) {
  do.call(rbind, lapply(seq_len(nrow(poligonos)), function(j) {
    area <- as.numeric(sf::st_area(poligonos[j, ]))
    exactextractr::exact_extract(clases, poligonos[j, ], fun = function(values, coverage_fraction)
      resumir_clases_cobertura_cipat(values, coverage_fraction, incluir_seco, prod(terra::res(clases)), area), progress = FALSE)
  }))
}

validar_modelo_muestras_cipat <- function(p) {
  m <- p$muestras
  if (is.null(m)) return(invisible(TRUE))
  r <- terra::rast(p$rgb)
  bandas <- unname(mapa_bandas_cipat(p, clave_rgb_cipat(p), terra::nlyr(r))[c("R", "G", "B")])
  if (!identical(m$firma, firma_muestras_cipat(p$rgb, bandas)))
    stop("La imagen o sus bandas cambiaron. Prepara y aplica de nuevo la segmentación por muestras.")
  invisible(TRUE)
}

agregar_cobertura_muestras_cipat <- function(p, tabla, poligonos, meta) {
  if (is.null(p$muestras)) return(list(tabla = tabla, meta = meta, ruta = NULL))
  validar_modelo_muestras_cipat(p)
  m <- p$muestras; rgb <- terra::rast(p$rgb)[[m$bandas]]
  ruta <- archivo_salida_cipat(p, "Mascara_Muestras", "tif")
  fun <- local({ modelo <- m$modelo; function(v) predecir_muestras_cipat(v, modelo) })
  mask <- escribir_bloques_sentera(rgb, ruta, fun, nombres = "Vegetacion_muestras",
    max_celdas = 200000L)
  polys <- sf::st_transform(poligonos, terra::crs(mask))
  df <- resumen_cobertura(mask, polys)
  df$veg_area_m2 <- df$veg_pix * prod(terra::res(mask))
  df$polygon_area_m2 <- as.numeric(sf::st_area(polys))
  df$valid_area_pct <- 100 * df$total_pix * prod(terra::res(mask)) / df$polygon_area_m2
  df <- df[c("pct", "veg_area_m2", "polygon_area_m2", "valid_area_pct")]
  names(df) <- paste0("Plant_ground_cover_image_Muestras_", if (p$tipo == "phantom") "RGB_" else "Sentera_", names(df))
  utils::write.csv(m$datos, archivo_salida_cipat(p, "Muestras_RGB", "csv"), row.names = FALSE)
  saveRDS(m, archivo_salida_cipat(p, "Modelo_Muestras", "rds"))
  meta <- rbind(meta, data.frame(Parametro = c("Segmentacion_muestras", "Muestras_ambito", "Muestras_bandas", "Muestras_zonas", "Muestras_pixeles", "Muestras_fuente", "Muestras_NoData"),
    Valor = c(m$modelo$metodo, "Cobertura adicional; no cambia máscaras NDVI/ExG ni estadísticas de otros rasgos",
      paste(m$bandas, collapse = ","), length(unique(m$datos$zona)), nrow(m$datos), m$firma,
      "RGB no finito se conserva como NoData; denominador: área RGB válida")))
  list(tabla = cbind(tabla, df), meta = meta, ruta = ruta)
}

registrar_muestras_cipat <- function(input, output, session, mapas_bandas) {
  estado <- shiny::reactiveValues()
  activo <- shiny::reactiveVal(NULL)
  fuente <- shiny::reactive({
    tipo <- input$tipo
    ruta <- if (identical(tipo, "phantom")) input$rgb_phantom else if (identical(input$origen_sentera, "multibanda")) input$multibanda else input$rgb
    if (length(ruta) != 1L || is.na(ruta) || !nzchar(ruta) || !file.exists(ruta)) stop("Primero carga un ortomosaico válido en el módulo seleccionado.")
    r <- terra::rast(ruta)
    p <- list(tipo = tipo, origen_sentera = input$origen_sentera, mapas_bandas = mapas_bandas())
    bandas <- unname(mapa_bandas_cipat(p, clave_rgb_cipat(p), terra::nlyr(r))[c("R", "G", "B")])
    list(ruta = ruta, bandas = bandas, firma = firma_muestras_cipat(ruta, bandas), tipo = tipo)
  })
  vacio <- function() list(datos = NULL, modelo = NULL, aplicado = FALSE, revision = 0L, incluir_seco = FALSE)
  actual <- function() { a <- activo(); shiny::req(a); s <- estado[[a$tipo]]; if (is.null(s) || !identical(s$firma, a$firma)) vacio() else s }
  guardar <- function(s) { a <- activo(); s$firma <- a$firma; estado[[a$tipo]] <- s }
  vista <- shiny::reactiveVal(NULL)
  output$estado_muestras <- shiny::renderText({
    f <- tryCatch(fuente(), error = function(e) NULL)
    if (is.null(f)) return("Carga una imagen para preparar muestras.")
    s <- estado[[f$tipo]]
    if (is.null(s) || !identical(s$firma, f$firma) || !isTRUE(s$aplicado)) return("Máscara por muestras: sin aplicar.")
    paste("Máscara por muestras preparada:", length(unique(s$datos$zona)), "zonas. Se utilizará si eliges Por muestras.")
  })
  shiny::observeEvent(input$preparar_muestras, {
    tryCatch({
      f <- fuente(); activo(f); vista(NULL)
      shiny::showModal(shiny::modalDialog(title = "Segmentación por muestras", size = "l", easyClose = FALSE,
        shiny::p("Arrastra un rectángulo sobre una zona homogénea. Añade al menos dos zonas de cada clase, incluyendo distintas condiciones de iluminación."),
        shiny::fluidRow(shiny::column(8,
          shiny::plotOutput("muestras_imagen", height = "440px", brush = shiny::brushOpts(id = "muestras_brush", resetOnNew = TRUE)),
          shiny::actionButton("muestras_zoom", "Acercar a selección"), shiny::actionButton("muestras_total", "Ver imagen completa")),
          shiny::column(4,
            shiny::checkboxInput("muestras_incluir_seco", "Incluir material vegetal seco (tres clases)", isTRUE(actual()$incluir_seco)),
            shiny::p("Desactivado: verde y suelo. Al desactivarlo se retiran las muestras secas."),
            shiny::radioButtons("muestras_clase", "La zona seleccionada es", if (isTRUE(actual()$incluir_seco)) c("Vegetación verde" = "1", "Suelo" = "0", "Material vegetal seco" = "2") else c("Vegetación verde" = "1", "Suelo" = "0")),
            shiny::actionButton("muestras_agregar", "Añadir zona", class = "btn-primary"),
            shiny::actionButton("muestras_deshacer", "Deshacer última zona"),
            shiny::actionButton("muestras_limpiar", "Borrar muestras"),
            shiny::textOutput("muestras_resumen"),
            shiny::actionButton("muestras_generar", "Generar vista previa"),
            shiny::checkboxInput("muestras_ver", "Mostrar máscara", FALSE),
            shiny::downloadButton("muestras_descargar", "Guardar muestras CSV"),
            shiny::fileInput("muestras_importar", "Recuperar muestras de esta imagen", accept = ".csv"))),
        shiny::p("Verde: vegetación verde; amarillo: material seco (si se incluye); café: suelo. La vista previa es reducida; la máscara final se calcula a resolución original. Esta alternativa utiliza RGB, también en Sentera."),
        footer = shiny::tagList(shiny::actionButton("muestras_no_usar", "Desactivar muestras"),
          shiny::modalButton("Cerrar"), shiny::actionButton("muestras_aplicar", "Aplicar máscara", class = "btn-primary"))))
    }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
  })
  imagen <- shiny::reactive({
    f <- activo(); shiny::req(f)
    r <- terra::rast(f$ruta)[[f$bandas]]
    v <- vista(); if (!is.null(v)) r <- terra::crop(r, terra::ext(v), snap = "out")
    fact <- max(1L, ceiling(max(nrow(r), ncol(r)) / 850L))
    if (fact > 1L) r <- terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
    r
  })
  output$muestras_imagen <- shiny::renderPlot({
    r <- imagen(); s <- actual()
    v <- terra::values(r, mat = TRUE)
    finitos <- v[is.finite(v)]; shiny::validate(shiny::need(length(finitos), "No hay píxeles válidos en esta vista."))
    limites <- range(finitos); if (diff(limites) == 0) limites[2] <- limites[1] + 1
    display <- terra::clamp((r - limites[1]) / diff(limites) * 255, 0, 255)
    terra::plotRGB(display, r = 1, g = 2, b = 3, scale = 255, axes = TRUE, maxcell = terra::ncell(r), asp = 1)
    if (isTRUE(input$muestras_ver) && !is.null(s$modelo)) {
      pred <- predecir_muestras_cipat(v, s$modelo)
      colores <- c("#a87543", "#00d65a", "#e8bd37")
      for (k in sort(unique(s$modelo$clases))) {
        m <- r[[1]]; terra::values(m) <- ifelse(!is.na(pred) & pred == k, 1, NA_real_)
        if (any(pred == k, na.rm = TRUE)) terra::plot(m, add = TRUE, legend = FALSE, axes = FALSE,
          col = grDevices::adjustcolor(colores[k + 1L], alpha.f = .5), maxcell = terra::ncell(r))
      }
    }
    if (!is.null(s$datos)) {
      zonas <- s$datos[!duplicated(s$datos$zona), ]
      graphics::rect(zonas$xmin, zonas$ymin, zonas$xmax, zonas$ymax,
        border = c("#a87543", "#00de74", "#e8bd37")[zonas$clase + 1L], lwd = 2)
    }
  })
  brush <- function() {
    b <- input$muestras_brush
    if (is.null(b) || any(!is.finite(unlist(b[c("xmin", "xmax", "ymin", "ymax")])))) stop("Primero arrastra un rectángulo sobre la imagen.")
    c(b$xmin, b$xmax, b$ymin, b$ymax)
  }
  manejar <- function(expr) tryCatch(expr, error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8))
  shiny::observeEvent(input$muestras_zoom, manejar(vista(brush())))
  shiny::observeEvent(input$muestras_total, vista(NULL))
  shiny::observeEvent(input$muestras_incluir_seco, {
    s <- actual(); incluir <- isTRUE(input$muestras_incluir_seco)
    if (!identical(isTRUE(s$incluir_seco), incluir)) {
      if (!incluir && !is.null(s$datos)) {
        s$datos <- s$datos[s$datos$clase != 2L, , drop = FALSE]
        if (!nrow(s$datos)) s$datos <- NULL
      }
      s$incluir_seco <- incluir; s$modelo <- NULL; s$aplicado <- FALSE; guardar(s)
    }
    shiny::updateRadioButtons(session, "muestras_clase", choices = if (incluir)
      c("Vegetación verde" = "1", "Suelo" = "0", "Material vegetal seco" = "2") else c("Vegetación verde" = "1", "Suelo" = "0"), selected = "1")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$muestras_agregar, manejar({
    s <- actual(); f <- activo(); b <- brush()
    if (!input$muestras_clase %in% (if (isTRUE(s$incluir_seco)) c("0", "1", "2") else c("0", "1"))) stop("Selecciona una clase disponible.")
    if (!is.null(s$datos) && length(unique(s$datos$zona)) >= 60L) stop("Máximo 60 zonas. Borra una antes de añadir otra.")
    r <- terra::crop(terra::rast(f$ruta)[[f$bandas]], terra::ext(b), snap = "in")
    if (terra::ncell(r) < 4L) stop("Selecciona una zona un poco mayor: mínimo cuatro píxeles.")
    d <- as.data.frame(terra::spatSample(r, size = min(500L, terra::ncell(r)), method = "regular", na.rm = TRUE, values = TRUE, xy = TRUE))
    names(d) <- c("x", "y", "R", "G", "B")
    d <- d[rowSums(is.finite(as.matrix(d))) == ncol(d), , drop = FALSE]
    if (nrow(d) < 4L) stop("La selección no contiene suficientes píxeles RGB válidos.")
    if (!is.null(s$datos)) {
      prev <- s$datos
      for (i in which(!duplicated(prev$zona))) if (prev$clase[i] != as.integer(input$muestras_clase) &&
        b[1] < prev$xmax[i] && b[2] > prev$xmin[i] && b[3] < prev$ymax[i] && b[4] > prev$ymin[i])
        stop("La selección se cruza con una zona de la clase contraria. Deshaz esa zona o selecciona otra.")
    }
    d$clase <- as.integer(input$muestras_clase)
    d$zona <- if (is.null(s$datos)) 1L else max(s$datos$zona) + 1L
    d$xmin <- b[1]; d$xmax <- b[2]; d$ymin <- b[3]; d$ymax <- b[4]; d$firma <- f$firma
    s$datos <- rbind(s$datos, d); s$modelo <- NULL; s$aplicado <- FALSE; guardar(s)
  }))
  shiny::observeEvent(input$muestras_deshacer, {
    s <- actual(); if (!is.null(s$datos)) s$datos <- s$datos[s$datos$zona != max(s$datos$zona), , drop = FALSE]
    if (!is.null(s$datos) && !nrow(s$datos)) s$datos <- NULL
    s$modelo <- NULL; s$aplicado <- FALSE; guardar(s)
  })
  shiny::observeEvent(input$muestras_limpiar, { s <- vacio(); s$incluir_seco <- isTRUE(input$muestras_incluir_seco); guardar(s) })
  shiny::observeEvent(input$muestras_generar, manejar({
    s <- actual(); s$modelo <- entrenar_muestras_cipat(s$datos, isTRUE(s$incluir_seco)); s$aplicado <- FALSE; guardar(s)
    shiny::updateCheckboxInput(session, "muestras_ver", value = TRUE)
  }))
  output$muestras_resumen <- shiny::renderText({
    s <- actual(); d <- s$datos
    if (is.null(d)) return("Sin muestras. Añade al menos dos zonas por clase.")
    paste("Verde:", length(unique(d$zona[d$clase == 1])), "zonas · Suelo:", length(unique(d$zona[d$clase == 0])), "zonas.",
      if (isTRUE(s$incluir_seco)) paste("Seco:", length(unique(d$zona[d$clase == 2])), "zonas.") else "Seco: no evaluado (dos clases).",
      if (is.null(s$modelo)) "Genera la vista previa." else "Vista previa preparada; revisa el resultado antes de aplicar.")
  })
  shiny::observeEvent(input$muestras_aplicar, manejar({
    s <- actual(); if (is.null(s$modelo)) stop("Genera y revisa primero la vista previa.")
    s$aplicado <- TRUE; guardar(s); shiny::updateRadioButtons(session, if (activo()$tipo == "phantom") "metodo_phantom" else "metodo", selected = "Muestras"); shiny::removeModal()
    shiny::showNotification("Máscara preparada. Elige Por muestras para usarla en el análisis.", type = "message")
  }))
  shiny::observeEvent(input$muestras_no_usar, { s <- actual(); s$aplicado <- FALSE; guardar(s); shiny::updateRadioButtons(session, if (activo()$tipo == "phantom") "metodo_phantom" else "metodo", selected = "Otsu"); shiny::removeModal() })
  output$muestras_descargar <- shiny::downloadHandler(filename = function() "PhenoImage_Muestras_RGB.csv",
    content = function(file) { s <- actual(); if (is.null(s$datos)) stop("Todavía no hay muestras."); d <- s$datos; d$incluir_seco <- isTRUE(s$incluir_seco); utils::write.csv(d, file, row.names = FALSE) })
  shiny::observeEvent(input$muestras_importar, manejar({
    d <- utils::read.csv(input$muestras_importar$datapath, stringsAsFactors = FALSE)
    incluir <- if ("incluir_seco" %in% names(d)) {
      if (!is.logical(d$incluir_seco) || anyNA(d$incluir_seco) || length(unique(d$incluir_seco)) != 1L) stop("Modo de clases inválido en el CSV.")
      isTRUE(d$incluir_seco[1])
    } else any(d$clase == 2L, na.rm = TRUE)
    d$incluir_seco <- NULL
    campos <- c("x", "y", "R", "G", "B", "clase", "zona", "xmin", "xmax", "ymin", "ymax", "firma")
    if (!identical(names(d), campos) || !nrow(d) || nrow(d) > 30000L || length(unique(d$zona)) > 60L ||
      anyNA(d) || !all(d$firma == activo()$firma) || any(!vapply(d[setdiff(campos, "firma")], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(d[setdiff(campos, "firma")])))) stop("CSV inválido o de otra imagen / asignación de bandas.")
    # Releer los píxeles originales: el CSV no puede inyectar colores de entrenamiento.
    r <- terra::rast(activo()$ruta)[[activo()$bandas]]
    z <- terra::extract(r, as.matrix(d[c("x", "y")]))
    if (ncol(z) != 3L || any(!is.finite(as.matrix(z)))) stop("Muestras fuera de la imagen o sin datos.")
    d[c("R", "G", "B")] <- z
    s <- vacio(); s$datos <- d; s$incluir_seco <- incluir; s$modelo <- entrenar_muestras_cipat(d, incluir); guardar(s)
    shiny::updateCheckboxInput(session, "muestras_incluir_seco", value = incluir)
    shiny::updateRadioButtons(session, "muestras_clase", choices = if (incluir) c("Vegetación verde" = "1", "Suelo" = "0", "Material vegetal seco" = "2") else c("Vegetación verde" = "1", "Suelo" = "0"), selected = "1")
    shiny::updateCheckboxInput(session, "muestras_ver", value = TRUE)
  }))
  # Se verifica de nuevo la identidad de imagen y bandas al iniciar cada ejecución.
  shiny::reactive({
    f <- tryCatch(fuente(), error = function(e) NULL); if (is.null(f)) return(NULL)
    s <- estado[[f$tipo]]
    if (is.null(s) || !identical(s$firma, f$firma) || !isTRUE(s$aplicado)) return(NULL)
    list(firma = f$firma, bandas = f$bandas, datos = s$datos, modelo = s$modelo, incluir_seco = isTRUE(s$incluir_seco))
  })
}
