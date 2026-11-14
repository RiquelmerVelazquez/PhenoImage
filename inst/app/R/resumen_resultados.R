# Portada basada únicamente en la tabla HTP; nunca reprocesa índices.
resumen_texto_cipat <- function(x) {
  if(is.null(x) || !length(x) || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) "No disponible" else as.character(x[1])
}
resumen_datos_cipat <- function(x) {
  d <- x$tabla; n <- nrow(d); rgb <- identical(x$tipo,"phantom")
  cobertura <- if ("Plant_ground_cover_image_Green_pct" %in% names(d)) "Plant_ground_cover_image_Green_pct" else if ("Plant_ground_cover_image_Vegetation_pct" %in% names(d)) "Plant_ground_cover_image_Vegetation_pct" else if(rgb) "Plant_ground_cover_image_RGB_pct" else "Plant_ground_cover_image_NDVI_pct"
  preferencias <- c(if(rgb) "Canopy_height_image_p90" else "NDVI_mean", grep("_mean$", names(d), value = TRUE), cobertura)
  candidatas <- preferencias[preferencias %in% names(d)]
  rasgo <- if (length(candidatas)) candidatas[1] else "No solicitado"
  valor <- function(k) {
    if(!k %in% names(d) || !is.numeric(d[[k]])) return(rep(NA_real_,n))
    v <- as.numeric(d[[k]]); v[!is.finite(v)] <- NA_real_; v
  }
  cv <- valor(cobertura); rv <- valor(rasgo)
  promedio <- function(v) if(any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
  numericas <- names(d)[vapply(d, is.numeric, logical(1))]
  numericas <- setdiff(numericas[!startsWith(numericas, "Parcela_")], c("id", "Lote", "Fecha"))
  ok <- if (length(numericas)) rowSums(is.finite(as.matrix(d[numericas]))) > 0 else rep(FALSE, n)
  revision <- data.frame(Parcela = as.character(d$id), Estado = ifelse(ok, "Con datos", "Por revisar"),
    Cobertura_pct = cv, Rasgo = rv, Detalle = ifelse(ok, "Métricas solicitadas disponibles", "Sin valores finitos en los análisis solicitados"), stringsAsFactors = FALSE)
  list(n=n,ok=sum(ok),revisar=sum(!ok),cobertura=promedio(cv),rasgo=promedio(rv),
    n_cobertura=sum(is.finite(cv)),n_rasgo=sum(is.finite(rv)),revision=revision,
    campo_cobertura=cobertura,campo_rasgo=rasgo,rgb=rgb)
}
resumen_numero_cipat <- function(x, dec=1L, unidad="") {
  if(!length(x) || !is.finite(x)) return("Sin datos")
  paste0(formatC(x,format="f",digits=dec,big.mark=" ",decimal.mark="."),unidad)
}
resumen_tarjeta_cipat <- function(titulo,valor,nota,simbolo) {
  shiny::div(class="results-stat",shiny::div(class="results-stat-icon",shiny::icon(simbolo)),
    shiny::div(shiny::div(class="results-stat-label",titulo),shiny::div(class="results-stat-number",valor),shiny::div(class="results-stat-note",nota)))
}
registrar_resumen_resultados_cipat <- function(input,output,session,actual) {
  datos <- shiny::reactive({shiny::req(actual());resumen_datos_cipat(actual())})
  output$resumen_descarga <- shiny::renderUI({
    if(is.null(actual())) return(NULL)
    shiny::downloadButton("descargar","Descargar Excel",class="btn-primary")
  })
  output$resumen_portada <- shiny::renderUI({
    x <- actual();shiny::req(x);s <- datos()
    shiny::div(class="results-overview",
      shiny::div(class="results-stats",
        resumen_tarjeta_cipat("Parcelas evaluadas",as.character(s$n),"Registros de la tabla HTP","table-cells"),
        resumen_tarjeta_cipat("Cobertura verde media",resumen_numero_cipat(s$cobertura,1," %"),paste(s$n_cobertura,"parcelas con valor"),"leaf"),
        resumen_tarjeta_cipat(etiqueta_eda(s$campo_rasgo),resumen_numero_cipat(s$rasgo,if(startsWith(s$campo_rasgo, "Canopy_height")) 1 else 3,if(startsWith(s$campo_rasgo, "Canopy_height")) " cm" else ""),
          paste(s$n_rasgo,"parcelas con valor"),if(s$rgb) "ruler-vertical" else "chart-column"),
        resumen_tarjeta_cipat("Datos principales válidos",paste(s$ok,"/",s$n),"Al menos una métrica solicitada disponible","circle-check")),
      shiny::div(class="results-main-grid",
        shiny::div(class="results-preview-card",
          shiny::div(class="results-section-title",shiny::h3(shiny::icon("map"),"Vista del ensayo"),shiny::actionButton("resumen_ir_mapa","Abrir mapa",icon=shiny::icon("arrow-up-right-from-square"))),
          shiny::plotOutput("resumen_orto",height="320px"),
          shiny::p(class="results-small","Vista reducida de la imagen o modelo disponible. El visor conserva su configuración.")),
        shiny::div(class="results-quality-card",
          shiny::h3(shiny::icon("database"),"Calidad de los datos"),
          shiny::div(class="results-quality-counts",
            shiny::div(shiny::strong(s$ok),shiny::span("con datos")),
            shiny::div(class="results-warning",shiny::strong(s$revisar),shiny::span("por revisar"))),
          shiny::div(class="results-quality-bar",role="img",`aria-label`=paste(s$ok,"con datos y",s$revisar,"por revisar"),
            shiny::span(style=paste0("width:",if(s$n) 100*s$ok/s$n else 0,"%;"))),
          shiny::p(class="results-small",if(s$revisar) "Hay parcelas sin valores en las métricas solicitadas." else "Todas las parcelas tienen al menos una métrica solicitada disponible."),
          shiny::p(class="results-small","Se comprueba disponibilidad de datos; esto no certifica su precisión ni la calidad del vuelo."),
          shiny::actionButton("resumen_ir_calidad","Revisar parcelas",icon=shiny::icon("magnifying-glass"))),
        shiny::div(class="results-average-note","Promedios simples entre parcelas con valores disponibles; se excluyen NA e infinitos. ",
          paste("Variable mostrada:", etiqueta_eda(s$campo_rasgo)))),
      shiny::div(class="results-shortcuts",
        shiny::actionButton("resumen_ir_tabla",shiny::tagList(shiny::strong("Tabla de resultados"),shiny::span("Consulta las métricas por parcela")),icon=shiny::icon("file-lines")),
        shiny::actionButton("resumen_ir_eda",shiny::tagList(shiny::strong("Análisis exploratorio"),shiny::span("Compara genotipos y variables")),icon=shiny::icon("chart-simple")),
        shiny::actionButton("resumen_ir_archivos",shiny::tagList(shiny::strong("Archivos de la evaluación"),shiny::span("Excel, productos y parámetros")),icon=shiny::icon("folder-open"))))
  })
  output$resumen_orto <- shiny::renderPlot({
    x <- actual();shiny::req(x)
    ruta <- x$capas$RGB
    if (is.null(ruta) && length(x$capas)) ruta <- x$capas[[1]]
    disponible <- !is.null(ruta) && length(ruta)==1L && !is.na(ruta) && file.exists(ruta)
    graphics::par(mar=c(0,0,0,0),bg="#f2f5ef")
    vacio <- function(msg) {graphics::plot.new();graphics::text(.5,.5,msg,col="#65746a",cex=.95)}
    if(!disponible) {vacio("Ortomosaico no disponible\nPuedes consultar las tablas y el análisis exploratorio.");return(invisible(NULL))}
    tryCatch({
      r <- terra::rast(ruta)
      if(terra::nlyr(r)<3L) { r <- terra::spatSample(r,size=360000,method="regular",as.raster=TRUE);terra::plot(r,axes=FALSE);return(invisible(NULL)) }
      r <- r[[bandas_rgb_resultado_cipat(x,terra::nlyr(r))]]
      # Lectura limitada para no cargar el mosaico completo en memoria.
      r <- terra::spatSample(r,size=360000,method="regular",as.raster=TRUE)
      terra::plotRGB(r,r=1,g=2,b=3,stretch="lin",axes=FALSE)
    },error=function(e) vacio("No fue posible generar la vista previa.\nConsulta la disponibilidad del archivo RGB."))
  },res=96)
  output$resumen_criterio <- shiny::renderUI({
    s <- datos()
    shiny::div(class="results-quality-explanation",shiny::h3("Disponibilidad por parcela"),
      shiny::p("Se revisan únicamente las métricas solicitadas. No pedir cobertura o altura no se considera un error. Cero es válido; los valores no finitos se consideran ausentes."),
      shiny::p(paste("Variable principal mostrada:",s$campo_rasgo)),
      shiny::p("Esta revisión no sustituye el control de georreferenciación, escala radiométrica, segmentación ni área válida. La tabla incluye todas las parcelas y permite filtrar por estado."))
  })
  output$resumen_revision_tabla <- DT::renderDT({
    d <- datos()$revision
    DT::datatable(d,rownames=FALSE,filter="top",options=list(scrollX=TRUE,pageLength=10,
      language=list(search="Buscar:",lengthMenu="Mostrar _MENU_ parcelas",info="_START_ a _END_ de _TOTAL_ parcelas",paginate=list(previous="Anterior",`next`="Siguiente"))))
  })
  output$resumen_archivos <- shiny::renderUI({
    x <- actual();shiny::req(x)
    rutas <- c(list("Excel HTP"=x$excel),x$capas)
    d <- data.frame(Producto=names(rutas),Archivo=vapply(rutas,function(p) if(is.null(p)||!length(p)) "No registrado" else basename(p[1]),character(1)),
      Disponible=vapply(rutas,function(p) if(!is.null(p)&&length(p)==1L&&!is.na(p)&&file.exists(p)) "Sí" else "No",character(1)),stringsAsFactors=FALSE)
    shiny::div(class="results-files",shiny::h3("Productos de la evaluación"),
      shiny::p("Consulta la disponibilidad de los archivos registrados. Los GeoTIFF permanecen en la carpeta de salida; no se incluyen en la descarga de Excel."),
      shiny::tags$table(class="results-files-table",shiny::tags$thead(shiny::tags$tr(lapply(names(d),shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(d)),function(i) shiny::tags$tr(lapply(d[i,],shiny::tags$td))))),
      shiny::div(class="bottom-actions",shiny::actionButton("resumen_ir_productos","Ver productos ráster",icon=shiny::icon("layer-group"))),
      shiny::p(class="results-small",paste("Carpeta:",basename(x$salida))))
  })
  destinos <- c(resumen_ir_mapa="Mapas",resumen_ir_calidad="Calidad de datos",resumen_ir_tabla="Tabla HTP",resumen_ir_archivos="Archivos",resumen_ir_productos="Productos ráster")
  for(id in names(destinos)) local({origen<-id;destino<-destinos[[id]]
    shiny::observeEvent(input[[origen]],shiny::updateTabsetPanel(session,"vista_resultados",selected=destino))
  })
  shiny::observeEvent(input$resumen_ir_eda,{
    shiny::updateSelectInput(session,"eda_origen",selected="activo")
    shiny::updateTabsetPanel(session,"secciones",selected="exploracion")
  })
  invisible(datos)
}
