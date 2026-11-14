# Análisis descriptivo de valores por parcela. No modifica los procesamientos.
etiqueta_eda <- function(nombre) {
  especiales <- c(Plant_ground_cover_image_Green_pct = "Cobertura verde (%)", Plant_ground_cover_image_Dry_pct = "Material vegetal seco (%)", Plant_ground_cover_image_Bare_soil_pct = "Suelo descubierto (%)", Plant_ground_cover_image_Total_pct = "Cobertura vegetal total (%)", Plant_ground_cover_image_Vegetation_pct = "Cobertura vegetal · máscara elegida (%)", Plant_ground_cover_image_Muestras_RGB_pct = "Cobertura por muestras · RGB (%)", Plant_ground_cover_image_Muestras_Sentera_pct = "Cobertura por muestras · Sentera (%)", Plant_ground_cover_image_NDVI_pct = "Cobertura NDVI (%)",
    Plant_ground_cover_image_RGB_pct = "Cobertura RGB · ExG (%)",
    HSB_Hue_mean = "Tono HSB · promedio (grados)", HSB_Saturation_mean = "Saturación HSB · promedio (%)",
    HSB_Brightness_mean = "Brillo HSB · promedio (%)",
    RGB_Entropy_mean = "Textura RGB · entropía", RGB_Contrast_mean = "Textura RGB · contraste",
    RGB_Homogeneity_mean = "Textura RGB · homogeneidad")
  if (nombre %in% names(especiales)) return(unname(especiales[[nombre]]))
  nombre <- sub("^Parcela_", "", nombre)
  sufijos <- c(mean = "promedio", median = "mediana", sd = "desviación estándar", cv = "CV (fracción)",
    min = "mínimo", max = "máximo", p10 = "P10", p25 = "P25", p75 = "P75", p90 = "P90", p95 = "P95", IQR = "rango intercuartílico",
    valid_area_pct = "área válida (%)", polygon_area_m2 = "área de parcela (m²)",
    total_area_m2 = "área válida (m²)", veg_area_m2 = "área detectada (m²)", soil_area_m2 = "área no detectada (m²)",
    total_pix = "píxeles equivalentes válidos", veg_pix = "píxeles equivalentes detectados")
  prefijos <- c(Plant_ground_cover_image_Green = "Cobertura verde", Plant_ground_cover_image_Dry = "Material vegetal seco", Plant_ground_cover_image_Bare_soil = "Suelo descubierto", Plant_ground_cover_image_Total = "Cobertura vegetal total", Plant_ground_cover_image_Vegetation = "Cobertura vegetal", Canopy_height_image = "Altura del dosel", Plant_ground_cover_image_NDVI = "Cobertura NDVI",
    Plant_ground_cover_image_RGB = "Cobertura RGB", Plant_ground_cover_image_CHM = "Cobertura según altura")
  for (k in names(prefijos)) nombre <- sub(paste0("^", k), prefijos[[k]], nombre)
  for (k in names(sufijos)[order(nchar(names(sufijos)), decreasing = TRUE)]) {
    if (endsWith(nombre, paste0("_", k))) {
      nombre <- paste(sub(paste0("_", k, "$"), "", nombre), sufijos[[k]], sep = " · ")
      break
    }
  }
  if (startsWith(nombre, "Altura del dosel") && !grepl("CV", nombre, fixed = TRUE)) nombre <- paste0(nombre, " (cm)")
  gsub("_", " ", nombre, fixed = TRUE)
}

variables_eda <- function(df) {
  n <- names(df)[vapply(df, is.numeric, logical(1))]
  n <- n[!n %in% c("id", "Lote", "Fecha") & !startsWith(n, "Parcela_")]
  stats::setNames(n, vapply(n, etiqueta_eda, character(1)))
}

# Los perfiles solo filtran opciones de Exploración; nunca cambian la tabla HTP.
variables_basicas_eda <- function(df) {
  todas <- variables_eda(df); n <- unname(todas)
  principal <- grepl("_mean$", n) | n == "Canopy_height_image_p90" |
    grepl("^Plant_ground_cover_image_(Green|Dry|Bare_soil|Total|Vegetation|NDVI|RGB|Muestras_RGB|Muestras_Sentera)_pct$", n)
  evaluada <- vapply(df[n], function(v) any(is.finite(v)), logical(1))
  todas[principal & evaluada]
}
variables_visibles_eda <- function(df, perfil = "basica", personalizadas = character()) {
  todas <- variables_eda(df)
  if (is.null(perfil) || !perfil %in% c("basica","personalizada","avanzada")) perfil <- "basica"
  if (perfil == "avanzada") return(todas)
  if (perfil == "personalizada") return(todas[unname(todas) %in% personalizadas])
  variables_basicas_eda(df)
}
elegir_variable_eda <- function(opciones, actual = NULL, preferidas = character(), excluir = NULL) {
  n <- unname(opciones)
  if (!length(n)) return(character())
  if (length(actual)==1L && actual %in% n) return(actual)
  candidatas <- unique(c(preferidas,n))
  candidatas <- candidatas[candidatas %in% n]
  distintas <- setdiff(candidatas,excluir)
  if (length(distintas)) distintas[1] else candidatas[1]
}

etiqueta_grupo_eda <- function(campo, grupos) {
  if (is.null(campo) || identical(campo,"__ninguno__")) return("Evaluación")
  etiqueta <- names(grupos)[match(campo,unname(grupos))]
  if (!length(etiqueta) || is.na(etiqueta)) etiqueta <- sub("^Parcela_","",campo)
  clave <- tolower(iconv(etiqueta,to="ASCII//TRANSLIT"))
  if (clave %in% c("rep","repeticion","repetition","replicate")) "Repetición"
  else if (clave %in% c("bloque","block")) "Bloque"
  else if (clave %in% c("genotipo","genotype")) "Genotipo"
  else paste("Agrupación:",etiqueta)
}

resumen_eda <- function(v, grupo = NULL) {
  if (is.null(grupo)) grupo <- rep("Evaluación completa", length(v))
  grupo <- as.character(grupo)
  grupo[is.na(grupo) | !nzchar(trimws(grupo))] <- "Sin grupo informado"
  niveles <- unique(grupo)
  if (!length(niveles)) return(data.frame())
  do.call(rbind, lapply(niveles, function(g) {
    vv <- v[grupo == g]; validos <- vv[is.finite(vv)]; n <- length(validos)
    data.frame(Grupo = g, Parcelas = length(vv), N = n, Sin_datos = length(vv) - n,
      Media = if (n) mean(validos) else NA_real_,
      Mediana = if (n) stats::median(validos) else NA_real_,
      DE = if (n > 1) stats::sd(validos) else NA_real_,
      Minimo = if (n) min(validos) else NA_real_, Maximo = if (n) max(validos) else NA_real_,
      IQR = if (n) stats::IQR(validos) else NA_real_, stringsAsFactors = FALSE)
  }))
}

preparar_eda <- function(x) {
  d <- x$tabla
  # Los atributos ya incluidos en Phantom se reutilizan. Sentera se enriquece
  # solo para la vista EDA mediante una unión explícita por ID.
  aviso <- NULL
  if (!any(startsWith(names(d), "Parcela_"))) {
    attrs <- tryCatch(sf::st_drop_geometry(sf::st_read(x$fuentes$polys, quiet = TRUE)),
      error = function(e) { aviso <<- "No fue posible recuperar los atributos de parcelas. La exploración por parcela sigue disponible."; NULL })
    if (!is.null(attrs) && x$fuentes$id %in% names(attrs)) {
      ids <- as.character(attrs[[x$fuentes$id]])
      if (!anyNA(ids) && !anyDuplicated(ids)) {
        pos <- match(as.character(d$id), ids)
        for (k in setdiff(names(attrs), x$fuentes$id)) {
          if (is.atomic(attrs[[k]]) && !is.matrix(attrs[[k]])) d[[paste0("Parcela_", k)]] <- attrs[[k]][pos]
        }
      } else aviso <- "Los atributos no se incorporaron porque el ID de parcelas no es único."
    }
  }
  grupos <- names(d)[startsWith(names(d), "Parcela_")]
  grupos <- grupos[vapply(d[grupos], function(v) is.atomic(v) && !is.matrix(v), logical(1))]
  list(tabla = d, grupos = stats::setNames(grupos, sub("^Parcela_", "", grupos)), aviso = aviso)
}

estilo_plot_eda <- function(mar = c(4.5, 4.8, 2.8, 1.4)) {
  graphics::par(mar=mar,bg="white",col.axis="#64766d",col.lab="#274b39",col.main="#173c2a",family="sans",las=1,cex.main=1.05,bty="n",tcl=-.2)
}

datos_plot_comparacion <- function(v,ids,grupo,marcas=NULL,meta=NULL,limite=25L,orden="desc") {
  if(is.null(marcas)) marcas<-list(testigo=rep(FALSE,length(v)),color="#D18A20")
  if(is.null(meta)) meta<-list(genotipo=as.character(ids),repeticion=rep(NA_character_,length(v)))
  agrupado<-!is.null(grupo)
  if(agrupado) {
    grupo<-as.character(grupo);grupo[is.na(grupo)|!nzchar(trimws(grupo))]<-"Sin grupo informado"
    t<-resumen_eda(v,grupo);names(t)[names(t)=="Grupo"]<-"Nombre"
  } else {
    t<-data.frame(Nombre=as.character(ids),Parcelas=1L,N=as.integer(is.finite(v)),Sin_datos=as.integer(!is.finite(v)),Media=v,Mediana=v,DE=NA_real_,Minimo=v,Maximo=v,IQR=NA_real_,stringsAsFactors=FALSE)
  }
  juntar<-function(z) {z<-unique(as.character(z));z<-z[!is.na(z)&nzchar(trimws(z))];if(!length(z))return("—");paste(c(head(z,8),if(length(z)>8)paste0("… (+",length(z)-8,")")),collapse=", ")}
  if(!nrow(t))return(data.frame())
  t$Tipo<-t$Genotipo<-t$IDs<-t$Repeticiones<-NA_character_
  for(i in seq_len(nrow(t))) {
    pos<-if(agrupado) which(grupo==t$Nombre[i]) else i
    t$Tipo[i]<-eda_tipo_grupo(marcas$testigo[pos]);t$Genotipo[i]<-juntar(meta$genotipo[pos]);t$IDs[i]<-juntar(ids[pos]);t$Repeticiones[i]<-juntar(meta$repeticion[pos])
  }
  t<-t[is.finite(t$Media),,drop=FALSE]
  if(!nrow(t))return(t)
  o<-switch(orden,asc=order(t$Media),nombre=order(t$Nombre),order(t$Media,decreasing=TRUE))
  # Incluye los testigos elegidos aunque queden fuera de los primeros puestos.
  checks<-o[t$Tipo[o]!="Experimental"]
  keep<-unique(c(checks,head(setdiff(o,checks),max(0L,as.integer(limite)-length(checks)))))
  o<-rev(o[o %in% keep]);t<-t[o,,drop=FALSE];t$x<-t$Media;t$y<-seq_len(nrow(t))
  t$Color<-eda_colores(t$Tipo,marcas$color);t$Simbolo<-eda_simbolos(t$Tipo)
  attr(t,"total")<-sum(is.finite(v))
  t
}

plot_comparacion_eda <- function(v,ids,grupo,etiqueta,limite=25L,orden="desc",marcas=NULL,meta=NULL,interactivo=TRUE) {
  t<-datos_plot_comparacion(v,ids,grupo,marcas,meta,limite,orden)
  estilo_plot_eda(c(5,10,3.8,1.5))
  if(!nrow(t)){graphics::plot.new();graphics::text(.5,.5,"No hay valores disponibles.");return(invisible(t))}
  delta<-ifelse(is.finite(t$DE),t$DE,0);media<-mean(v[is.finite(v)])
  rango<-range(c(t$x-delta,t$x+delta,media),finite=TRUE);amplitud<-diff(rango)
  if(amplitud==0)amplitud<-max(1,abs(rango[1])*.1)
  rango<-rango+c(-.1,.12)*amplitud
  graphics::plot(t$x,t$y,type="n",xlim=rango,ylim=c(.5,nrow(t)+.5),yaxt="n",xaxt="n",xlab=etiqueta,ylab="",main="")
  bandas<-which(seq_len(nrow(t))%%2==0)
  for(i in bandas) graphics::rect(rango[1],i-.5,rango[2],i+.5,col="#f6f9f6",border=NA)
  ticks<-pretty(rango);graphics::abline(v=ticks,col="#e7eee8",lwd=.8)
  graphics::axis(1,at=ticks,col="#d9e3da",col.ticks="#d9e3da",cex.axis=.85)
  graphics::abline(v=media,col="#718079",lty=2,lwd=1.2)
  valid<-is.finite(t$DE)&t$DE>0
  if(any(valid)) {
    graphics::segments(t$x[valid]-delta[valid],t$y[valid],t$x[valid]+delta[valid],t$y[valid],col=grDevices::adjustcolor(t$Color[valid],alpha.f=.7),lwd=1.8)
    graphics::segments(t$x[valid]-delta[valid],t$y[valid]-.13,t$x[valid]-delta[valid],t$y[valid]+.13,col=t$Color[valid])
    graphics::segments(t$x[valid]+delta[valid],t$y[valid]-.13,t$x[valid]+delta[valid],t$y[valid]+.13,col=t$Color[valid])
  }
  graphics::points(t$x,t$y,pch=t$Simbolo,bg=t$Color,col="white",lwd=.8,cex=1.4)
  labels<-t$Nombre;labels<-ifelse(nchar(labels)>28,paste0(substr(labels,1,25),"…"),labels)
  graphics::axis(2,at=t$y,labels=labels,tick=FALSE,las=1,cex.axis=.82,gap.axis=0)
  graphics::mtext(if(is.null(grupo))"Valores por parcela" else "Medias por grupo",side=3,line=2,adj=0,font=2,col="#173c2a",cex=1)
  eda_leyenda(t$Tipo,if(is.null(marcas))"#D18A20" else marcas$color)
  graphics::mtext(paste(nrow(t),if(is.null(grupo))"parcelas" else "grupos",if(!is.null(grupo))"· intervalos: ±1 DE" else "",if(interactivo)"· pasa el cursor por un punto" else ""),side=1,line=3.8,cex=.75,col="#718079")
  t$Variable<-etiqueta
  invisible(t)
}

registrar_exploracion <- function(input, output, session, ultima, datos, agrupacion, testigos = shiny::reactive(NULL)) {
  output$eda_contenido <- shiny::renderUI({
    x <- ultima()
    if (is.null(x)) return(div(class = "card empty-state", icon("chart-line"), h3("Análisis exploratorio disponible al finalizar"),
      p("Procesa una evaluación y selecciona este apartado para explorar sus variables.")))
    if (!is.null(x$error)) return(div(class="card empty-state",icon("circle-info"),h3("Revisa el origen de los datos"),p(x$error)))
    todas <- variables_eda(datos()$tabla)
    if (!length(todas)) return(div(class = "card empty-state", h3("No hay variables numéricas para explorar.")))
    basicas <- variables_basicas_eda(datos()$tabla)
    perfil <- shiny::isolate(input$eda_perfil)
    if (is.null(perfil) || !perfil %in% c("basica","personalizada","avanzada")) perfil <- "basica"
    personalizadas <- shiny::isolate(input$eda_personalizadas)
    if (is.null(personalizadas)) personalizadas <- unname(basicas)
    personalizadas <- intersect(personalizadas,unname(todas))
    opciones <- variables_visibles_eda(datos()$tabla,perfil,personalizadas)
    preferida <- elegir_variable_eda(opciones,shiny::isolate(input$eda_variable),
      if (x$tipo == "phantom") c("Canopy_height_image_mean","Canopy_height_image_p90") else "NDVI_mean")
    alternativa <- elegir_variable_eda(opciones,shiny::isolate(input$eda_variable_y),
      c("Plant_ground_cover_image_Green_pct","Canopy_height_image_p90","NDVI_mean"),excluir=preferida)
    grupo_actual <- shiny::isolate(agrupacion())
    if (!grupo_actual %in% unname(datos()$grupos)) grupo_actual <- "__ninguno__"
    tagList(
      div(class = "evaluation-banner", icon("clipboard-check"), div(strong(x$fuentes$lote),
        span(paste(if(x$tipo=="integrado")"Sentera + RGB · base integrada" else nombre_modulo_cipat(x$tipo), "·", x$fuentes$fecha, "·", nrow(x$tabla), "parcelas")))),
      div(class = "analysis-layout",
        tags$aside(class = "analysis-tools",
          tarjeta("Controles", "Selecciona variables y agrupación. Orden y elementos visibles se aplican a la comparación.",
            selectInput("eda_perfil", "Vista de variables", c("Básica · principales"="basica", "Personalizada · elegir variables"="personalizada", "Avanzada · todos los estadísticos"="avanzada"), selected=perfil),
            conditionalPanel("input.eda_perfil === 'personalizada'",
              selectizeInput("eda_personalizadas", "Variables que quieres mostrar", choices=todas, selected=personalizadas, multiple=TRUE,
                options=list(placeholder="Busca y selecciona variables",plugins=list("remove_button"),maxOptions=1000))),
            uiOutput("eda_perfil_nota"),
            selectInput("eda_variable", "Variable", opciones, selected = preferida),
            selectInput("eda_grupo", "Agrupación", c("Por parcela" = "__ninguno__", datos()$grupos), selected = grupo_actual),
            conditionalPanel("input.eda_vista === 'relacion'",
              selectInput("eda_variable_y", "Variable Y", opciones, selected = alternativa),
              selectInput("eda_relacion_nivel", "Puntos de la relación X–Y", c("Parcelas con ambos valores"="parcela", "Medias por grupo"="medias"))),
            uiOutput("tg_eda_ui"),
            conditionalPanel("!input.eda_vista || input.eda_vista === 'comparacion'",
              div(class = "control-section", div(class = "tool-heading", icon("chart-column"), "Comparación"),
                selectInput("eda_orden", "Orden", c("Mayor a menor" = "desc", "Menor a mayor" = "asc", "Nombre del grupo / ID" = "nombre")),
                sliderInput("eda_limite", "Elementos visibles", min = 5, max = 60, value = 25, step = 5))),
            if (!length(datos()$grupos)) div(class = "tip", icon("circle-info"), "Tus parcelas tienen únicamente ID. Para agrupar por genotipo o tratamiento, incorpora esos campos al SHP/GPKG."),
            if (!is.null(datos()$aviso)) div(class = "tip", datos()$aviso),
            div(class = "control-section export-tools", div(class = "tool-heading", icon("download"), "Descargas"),
              downloadButton("eda_csv", "Resumen CSV"), conditionalPanel("!input.eda_vista || input.eda_vista === 'comparacion'", downloadButton("eda_png", "Comparación PNG"))),
            ayuda_contextual("Interpretación del análisis", "Las estadísticas se calculan sobre valores por parcela. La selección de grupos y variables no modifica el Excel HTP ni los GeoTIFF."))),
        div(class = "analysis-content", uiOutput("eda_indicadores"),
          tabsetPanel(id = "eda_vista",
            tabPanel("Comparación", value = "comparacion",
              tarjeta("Comparación", "La agrupación utiliza únicamente atributos presentes en tus parcelas.",
                uiOutput("eda_comparacion_ui"),
                ayuda_breve("Cada parcela aporta un valor. Al agrupar se calcula una media simple por grupo y su desviación estándar (DE), disponible desde n=2. Las barras son descriptivas; no representan intervalos de confianza ni pruebas de significancia."))),
            tabPanel("Distribución", value = "distribucion", div(class = "grid-two",
              tarjeta("Frecuencias", "Valores disponibles de la variable principal.", plotOutput("eda_histograma", height = "440px")),
              tarjeta("Distribución por grupo", "Mediana, cuartiles y puntos por parcela.", eda_plot_interactivo("eda_cajas","eda_hover_cajas","eda_ficha_cajas",height="440px")))),
            tabPanel("Relación X–Y", value = "relacion", tarjeta("Relación entre rasgos", "Variable principal en X; segunda variable en Y.",
              eda_plot_interactivo("eda_relacion","eda_hover_relacion","eda_ficha_relacion",height="570px"), uiOutput("eda_correlacion"),
              ayuda_breve("La correlación utiliza pares de parcelas con valores finitos. Describe asociación y puede verse afectada por valores extremos."))),
            tabPanel("Resumen", value = "resumen", tarjeta("Resumen estadístico", "Una fila por grupo, o una fila para la evaluación completa si no hay agrupación.",
              DT::DTOutput("eda_resumen")))))))
  })
  opciones_visibles <- shiny::reactive({
    variables_visibles_eda(datos()$tabla,input$eda_perfil,input$eda_personalizadas)
  })
  shiny::observeEvent(opciones_visibles(), {
    opciones <- opciones_visibles(); x <- ultima()
    principal <- elegir_variable_eda(opciones,shiny::isolate(input$eda_variable),
      if (identical(x$tipo,"phantom")) c("Canopy_height_image_mean","Canopy_height_image_p90") else "NDVI_mean")
    segunda <- elegir_variable_eda(opciones,shiny::isolate(input$eda_variable_y),
      c("Plant_ground_cover_image_Green_pct","Canopy_height_image_p90","NDVI_mean"),excluir=principal)
    shiny::updateSelectInput(session,"eda_variable",choices=opciones,selected=principal)
    shiny::updateSelectInput(session,"eda_variable_y",choices=opciones,selected=segunda)
  },ignoreNULL=FALSE)
  output$eda_perfil_nota <- shiny::renderUI({
    n <- length(opciones_visibles())
    if (!n) return(div(class="tip",icon("circle-info"),
      if (identical(input$eda_perfil,"personalizada")) "Selecciona al menos una variable para explorar." else "No hay métricas principales evaluadas. Usa la vista Avanzada para consultar los estadísticos disponibles."))
    tags$p(class="ib-note",paste(n,"variables visibles. La vista no cambia los resultados ni el Excel."))
  })
  # Cambiar solo la etiqueta conserva las variables elegidas al cambiar de pestaña.
  shiny::observeEvent(input$eda_vista, {
    shiny::updateSelectInput(session, "eda_variable", label = if (identical(input$eda_vista, "relacion")) "Variable X" else "Variable")
  }, ignoreNULL = FALSE)
  seleccion <- shiny::reactive({
    d <- datos()$tabla; shiny::req(input$eda_variable, input$eda_variable %in% unname(opciones_visibles()), input$eda_variable %in% names(d), is.numeric(d[[input$eda_variable]]))
    campo <- agrupacion()
    grupo <- if (campo %in% unname(datos()$grupos)) as.character(d[[campo]]) else NULL
    if (!is.null(grupo)) grupo[is.na(grupo) | !nzchar(trimws(grupo))] <- "Sin grupo informado"
    list(tabla = d, v = d[[input$eda_variable]], ids = as.character(d$id), grupo = grupo, etiqueta = etiqueta_eda(input$eda_variable), marcas=eda_marcas(d,testigos()), meta=eda_meta_puntos(d))
  })
  resumen <- shiny::reactive({
    s<-seleccion();t<-resumen_eda(s$v,s$grupo)
    grupos<-if(is.null(s$grupo)) rep("Evaluación completa",length(s$v)) else s$grupo
    t$Tipo<-vapply(t$Grupo,function(g) eda_tipo_grupo(s$marcas$testigo[grupos==g]),character(1))
    t
  })
  puntos_comparacion<-shiny::reactiveVal(NULL);puntos_cajas<-shiny::reactiveVal(NULL)
  output$eda_indicadores <- shiny::renderUI({
    s <- seleccion(); t <- resumen_eda(s$v); fmt <- function(v) if (is.finite(v)) formatC(v, digits = 3, format = "fg") else "—"
    dato <- function(titulo, valor, nota) div(class = "metric", title = nota, div(class = "metric-label", titulo), div(class = "metric-value", valor))
    div(class = "kpi-grid eda-kpis", dato("Parcelas con datos", paste0(t$N, " / ", t$Parcelas), paste(t$Sin_datos, "sin valor finito")),
      dato("Media general", fmt(t$Media), s$etiqueta), dato("Mediana", fmt(t$Mediana), "Centro de la distribución"),
      dato("Desviación estándar", fmt(t$DE), "Variación entre parcelas"))
  })
  output$eda_comparacion_ui <- shiny::renderUI({
    s<-seleccion();n<-nrow(datos_plot_comparacion(s$v,s$ids,s$grupo,s$marcas,s$meta,if(is.null(input$eda_limite))25L else input$eda_limite,if(is.null(input$eda_orden))"desc" else input$eda_orden))
    eda_plot_interactivo("eda_comparacion","eda_hover_comparacion","eda_ficha_comparacion",height=paste0(max(430,210+n*19),"px"))
  })
  dibujar_comparacion <- function(interactivo=TRUE) {
    s <- seleccion(); shiny::validate(shiny::need(any(is.finite(s$v)), "No hay valores disponibles para esta variable."))
    plot_comparacion_eda(s$v, s$ids, s$grupo, s$etiqueta, if (is.null(input$eda_limite)) 25L else input$eda_limite,
      if (is.null(input$eda_orden)) "desc" else input$eda_orden,s$marcas,s$meta,interactivo)
  }
  output$eda_comparacion <- shiny::renderPlot({ puntos_comparacion(dibujar_comparacion()) }, res = 96)
  eda_registrar_hover(input,output,"eda_hover_comparacion","eda_ficha_comparacion",puntos_comparacion,c("Variable"="Variable","Grupo / parcela"="Nombre","Genotipo"="Genotipo","Media / valor"="Media","Desviación estándar"="DE","Valores válidos"="N","Sin datos"="Sin_datos","Parcelas"="IDs","Repeticiones"="Repeticiones","Identificación"="Tipo"),"Comparación del ensayo")
  output$eda_histograma <- shiny::renderPlot({
    s<-seleccion();ok<-is.finite(s$v);v<-s$v[ok];shiny::validate(shiny::need(length(v)>0,"No hay valores disponibles."))
    estilo_plot_eda(c(5.5,4.8,2.8,1.4));h<-graphics::hist(v,breaks="Sturges",plot=FALSE)
    graphics::plot(h,col="#d7e6db",border="white",main="",xlab=s$etiqueta,ylab="Número de parcelas",yaxt="n")
    ticks<-pretty(c(0,max(h$counts)));graphics::abline(h=ticks,col="#edf1ed",lwd=.7);graphics::plot(h,add=TRUE,col="#c7ddce",border="white")
    graphics::axis(2,at=ticks,tick=FALSE);graphics::abline(v=mean(v),col="#24704A",lwd=2);graphics::abline(v=stats::median(v),col="#718079",lty=2,lwd=1.5)
    if(any(s$marcas$testigo[ok])) graphics::rug(v[s$marcas$testigo[ok]],col=s$marcas$color,lwd=2,ticksize=.06)
    graphics::legend("topright",legend=c("Media","Mediana",if(any(s$marcas$testigo[ok]))"Testigos: valores individuales"),col=c("#24704A","#718079",if(any(s$marcas$testigo[ok]))s$marcas$color),lty=c(1,2,if(any(s$marcas$testigo[ok]))1),bty="n",cex=.78)
    if(any(s$marcas$testigo[ok])) graphics::mtext("Marcas inferiores: valores de las parcelas testigo",side=1,line=4.3,cex=.7,col=s$marcas$color)
  },res=96)
  output$eda_cajas <- shiny::renderPlot({
    s<-seleccion();ok<-is.finite(s$v);shiny::validate(shiny::need(any(ok),"No hay valores disponibles."))
    g<-if(is.null(s$grupo))rep("Evaluación",sum(ok)) else s$grupo[ok]
    conteos<-sort(table(g),decreasing=TRUE);checks<-unique(g[s$marcas$testigo[ok]])
    niveles<-unique(c(checks,head(setdiff(names(conteos),checks),max(0L,15L-length(checks)))))
    keep<-g %in% niveles;f<-factor(g[keep],levels=niveles);valores<-s$v[ok][keep]
    flags<-s$marcas$testigo[ok][keep];tipos<-vapply(niveles,function(n) eda_tipo_grupo(flags[as.character(f)==n]),character(1))
    colores<-eda_colores(tipos,s$marcas$color)
    cortas <- max(nchar(niveles)) <= 8L
    estilo_plot_eda(c(if(cortas)5.5 else 9,4.8,2.8,1.3))
    graphics::boxplot(valores~f,col=grDevices::adjustcolor(colores,alpha.f=.15),border=colores,outline=FALSE,las=if(cortas)1 else 2,cex.axis=.76,ylab=s$etiqueta,xlab="",medlwd=2,boxwex=.55)
    graphics::mtext(etiqueta_grupo_eda(agrupacion(),datos()$grupos),side=1,line=if(cortas)3 else 7.5,cex=.9,col="#365f48")
    graphics::abline(h=pretty(range(valores)),col="#edf1ed",lwd=.7)
    graphics::boxplot(valores~f,col=grDevices::adjustcolor(colores,alpha.f=.15),border=colores,outline=FALSE,axes=FALSE,add=TRUE,medlwd=2,boxwex=.55)
    jitter<-((seq_along(valores)*.61803398875)%%1-.5)*.24
    p<-data.frame(x=as.integer(f)+jitter,y=valores,ID=s$ids[ok][keep],Genotipo=s$meta$genotipo[ok][keep],Repeticion=s$meta$repeticion[ok][keep],Grupo=as.character(f),Tipo=ifelse(flags,"Testigo","Experimental"),Variable=s$etiqueta)
    graphics::points(p$x,p$y,pch=eda_simbolos(p$Tipo),bg=eda_colores(p$Tipo,s$marcas$color),col="white",lwd=.5,cex=.9)
    puntos_cajas(p)
    if(length(conteos)>length(niveles))graphics::mtext(paste("Vista de",length(niveles),"grupos; el resumen incluye todos."),side=3,cex=.72,col="#718079")
  },res=96)
  eda_registrar_hover(input,output,"eda_hover_cajas","eda_ficha_cajas",puntos_cajas,c("Variable"="Variable","Parcela"="ID","Genotipo"="Genotipo","Repetición"="Repeticion","Grupo"="Grupo","Valor"="y","Identificación"="Tipo"),"Valor por parcela")
  pares <- shiny::reactive({
    s<-seleccion();shiny::req(input$eda_variable_y,input$eda_variable_y %in% unname(opciones_visibles()),input$eda_variable_y %in% names(s$tabla),is.numeric(s$tabla[[input$eda_variable_y]]))
    y<-s$tabla[[input$eda_variable_y]];ok<-is.finite(s$v)&is.finite(y)
    p<-data.frame(x=s$v[ok],y=y[ok],ID=s$ids[ok],Genotipo=s$meta$genotipo[ok],Repeticion=s$meta$repeticion[ok],N=rep.int(1L,sum(ok)),Tipo=ifelse(s$marcas$testigo[ok],"Testigo","Experimental"),stringsAsFactors=FALSE)
    nivel<-if(is.null(input$eda_relacion_nivel))"parcela" else input$eda_relacion_nivel
    if(nivel=="medias") {
      shiny::validate(shiny::need(!is.null(s$grupo),"Elige una agrupación para relacionar medias por grupo."))
      g<-s$grupo[ok]
      p<-if(!nrow(p))p else do.call(rbind,lapply(unique(g),function(k){i<-which(g==k);data.frame(x=mean(p$x[i]),y=mean(p$y[i]),ID=paste(p$ID[i],collapse=", "),Genotipo=paste(unique(p$Genotipo[i]),collapse=", "),Repeticion=paste(unique(p$Repeticion[i][!is.na(p$Repeticion[i])]),collapse=", "),N=length(i),Tipo=eda_tipo_grupo(s$marcas$testigo[ok][i]),Grupo=k,stringsAsFactors=FALSE)}))
    }
    p$Variable_X<-rep(s$etiqueta,nrow(p));p$Variable_Y<-rep(etiqueta_eda(input$eda_variable_y),nrow(p))
    list(x=p$x,y=p$y,tabla=p,xlabel=s$etiqueta,ylabel=etiqueta_eda(input$eda_variable_y),nivel=nivel,color=s$marcas$color)
  })
  output$eda_relacion <- shiny::renderPlot({
    p<-pares();shiny::validate(shiny::need(length(p$x)>0,"No hay pares de valores disponibles."))
    estilo_plot_eda(c(4.8,5.1,3.5,1.4));graphics::plot(p$x,p$y,type="n",xlab=p$xlabel,ylab=p$ylabel,main="")
    graphics::abline(h=pretty(range(p$y)),v=pretty(range(p$x)),col="#edf1ed",lwd=.8)
    if(length(p$x)>=3L&&stats::sd(p$x)>0&&stats::sd(p$y)>0){ajuste<-stats::lm(y~x,data=data.frame(x=p$x,y=p$y));graphics::abline(ajuste,col="#718079",lwd=1.4,lty=2)}
    graphics::points(p$x,p$y,pch=eda_simbolos(p$tabla$Tipo),bg=eda_colores(p$tabla$Tipo,p$color),col="white",lwd=.8,cex=1.25)
    graphics::mtext(if(p$nivel=="medias")"Relación entre medias de grupo" else "Relación entre valores por parcela",side=3,line=2,adj=0,font=2,col="#173c2a",cex=1)
    clases<-intersect(c("Experimental","Testigo","Grupo mixto"),unique(p$tabla$Tipo))
    graphics::legend("topright",inset=c(0,-.13),legend=clases,pch=eda_simbolos(clases),pt.bg=eda_colores(clases,p$color),col="white",bty="n",xpd=TRUE,horiz=TRUE,cex=.78)
  },res=96)
  puntos_relacion<-shiny::reactive({pares()$tabla})
  eda_registrar_hover(input,output,"eda_hover_relacion","eda_ficha_relacion",puntos_relacion,c("Variable X"="Variable_X","Variable Y"="Variable_Y","Genotipo"="Genotipo","Parcelas"="ID","Repeticiones"="Repeticion","Valor X"="x","Valor Y"="y","Pares utilizados"="N","Identificación"="Tipo"),"Relación X–Y")
  output$eda_correlacion <- shiny::renderUI({
    p <- pares()
    if (length(p$x) < 3L || stats::sd(p$x) == 0 || stats::sd(p$y) == 0) return(div(class = "tip", "Correlación no disponible: se necesitan al menos tres pares y variación en ambas variables."))
    div(class = "tip", icon("chart-line"), paste0(if(p$nivel=="medias")"Grupos con medias pareadas: " else "Parcelas con pares válidos: ", length(p$x), " · Pearson r = ", round(stats::cor(p$x, p$y), 3),
      " · Spearman ρ = ", round(stats::cor(p$x, p$y, method = "spearman"), 3),if(p$nivel=="medias")" · Ambas medias usan las mismas parcelas con X e Y." else ""))
  })
  output$eda_resumen <- DT::renderDT({
    t <- resumen(); DT::formatRound(DT::datatable(t, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10,
      language = list(search = "Buscar grupo:", lengthMenu = "Mostrar _MENU_ grupos", info = "_START_ a _END_ de _TOTAL_ grupos", zeroRecords = "Sin coincidencias", paginate = list(previous = "Anterior", `next` = "Siguiente")))),
      columns = c("Media", "Mediana", "DE", "Minimo", "Maximo", "IQR"), digits = 4)
  })
  output$eda_csv <- shiny::downloadHandler(filename = function() paste0("PhenoImage_EDA_", ultima()$tipo, "_", input$eda_variable, ".csv"), content = function(file) {
    t <- resumen(); x <- ultima(); t$Variable <- input$eda_variable
    t$Agrupacion <- if (is.null(seleccion()$grupo)) "Sin agrupar" else sub("^Parcela_", "", agrupacion())
    t$Evaluacion <- x$fuentes$lote; t$Fecha <- as.character(x$fuentes$fecha); t$Modulo <- if(x$tipo=="integrado")"Sentera + RGB" else nombre_modulo_cipat(x$tipo)
    utils::write.csv(t, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  })
  output$eda_png <- shiny::downloadHandler(filename = function() paste0("PhenoImage_Comparacion_", input$eda_variable, ".png"), content = function(file) {
    grDevices::png(file, width = 2400, height = 1800, res = 220)
    on.exit(grDevices::dev.off(), add = TRUE); dibujar_comparacion(interactivo=FALSE)
  }, contentType = "image/png")
  invisible(list(seleccion=seleccion,resumen=resumen,pares=pares,puntos_comparacion=puntos_comparacion,puntos_cajas=puntos_cajas))
}
