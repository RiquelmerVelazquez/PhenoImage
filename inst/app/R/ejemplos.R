# Ejemplos opcionales; su descarga se instala expresamente, nunca al abrir la app.
campos_ejemplos_cipat <- function() list(
  texto=c("rgb","ms","multibanda","rgb_phantom","chm","dsm","dtm","salida","lote","id_sentera","id_phantom"),
  numeros=c("factor","umbral","umbral_phantom","altura_min","altura_max","escala_rgb","ventana"),
  radios=c("metodo","metodo_phantom","fuente_altura"),
  selectores=c("origen_sentera","unidad_altura"),
  checks=c("confirmar_altura","texturas","crear_visores","raster_entrada_parcelas"), grupos=c("indices","indices_rgb"))

configuracion_ejemplo_cipat <- function(modulo, raiz=getwd()) {
  if (!modulo %in% c("sentera","phantom")) stop("Selecciona Sentera o RGB.")
  carpeta <- phenoImage::example_path(if (modulo == "sentera") "sentera" else "rgb")
  archivos <- if (modulo=="sentera") c(multibanda="Multibanda_Sentera_BH16_2026-09-11.tif",parcelas="Sentera_FTRBH25STG2CH001_2026-09-11.shp")
    else c(rgb_phantom="Ortomosaico_RGB_BH16_2026-09-11.tif",chm="CHM_BH16_2026-09-11_cm.tif",parcelas="RGB_FTRBH25STG2CH001_2026-09-11.shp")
  rutas <- file.path(carpeta,unname(archivos));names(rutas)<-names(archivos)
  if (!all(file.exists(rutas))) stop("Faltan archivos del ejemplo opcional. Instálalo con phenoImage::install_examples().")
  rutas <- vapply(rutas,normalizePath,character(1),winslash="/",mustWork=TRUE)
  salida <- file.path(tools::R_user_dir("CIPAT_Fenotipado_UAV","data"),"Resultados_ejemplos_completos")
  texto <- c(as.list(rutas[names(rutas)!="parcelas"]),list(salida=salida,
    lote=if(modulo=="sentera") "Ejemplo_Sentera_BH16_Completo" else "Ejemplo_RGB_BH16_Completo"))
  texto[[paste0("id_",modulo)]] <- "id"
  list(modulo=modulo,parcelas=unname(rutas[["parcelas"]]),
    texto=texto,fecha=as.Date("2026-09-11"),
    numeros=if(modulo=="sentera") list(factor=1,umbral=.3) else
      list(escala_rgb=255,altura_min=2.5,altura_max=100,umbral_phantom=0,ventana=5),
    radios=if(modulo=="sentera") list(metodo="Otsu") else list(metodo_phantom="Otsu",fuente_altura="CHM"),
    selectores=if(modulo=="sentera") list(origen_sentera="multibanda") else list(unidad_altura="cm"),
    checks=if(modulo=="sentera") list(crear_visores=FALSE,raster_entrada_parcelas=TRUE) else list(confirmar_altura=TRUE,texturas=FALSE,crear_visores=FALSE,raster_entrada_parcelas=TRUE),
    grupos=if(modulo=="sentera") list(indices=c("NDVI","NDRE","GNDVI")) else
      list(indices_rgb=names(indices_originales_cipat("phantom"))),
    clave_formulas=if(modulo=="sentera") "sentera_multibanda" else "phantom",
    formulas=if(modulo=="sentera") indices_originales_cipat("sentera")[c("NDVI","NDRE","GNDVI")] else indices_originales_cipat("phantom"))
}

selector_ejemplos_cipat <- function() {
  shiny::div(class="example-source-card",
    shiny::div(class="example-source-heading",shiny::icon("flask"),shiny::tags$strong("Datos para el análisis")),
    shiny::conditionalPanel("input.tipo == 'sentera'",
      shiny::radioButtons("modo_datos_sentera",NULL,c("Mis archivos"="propios","Usar ejemplo"="ejemplo"),selected="propios",inline=TRUE)),
    shiny::conditionalPanel("input.tipo == 'phantom'",
      shiny::radioButtons("modo_datos_phantom",NULL,c("Mis archivos"="propios","Usar ejemplo"="ejemplo"),selected="propios",inline=TRUE)),
    shiny::tags$small(class = "example-details", "Los ejemplos son opcionales. Para instalarlos consulta la Guía; puedes usar tus archivos sin descargarlos."),
    shiny::uiOutput("informacion_ejemplo"),
    shiny::conditionalPanel("(input.tipo == 'sentera' && input.modo_datos_sentera == 'ejemplo') || (input.tipo == 'phantom' && input.modo_datos_phantom == 'ejemplo')",
      shiny::actionButton("recargar_ejemplo","Restablecer ejemplo",icon=shiny::icon("rotate-left"),class="example-reset")))
}

# Este bloque es verdadero mientras se pueden importar archivos personales.
condicion_archivos_propios_cipat <- function() {
  "(input.tipo == 'sentera' && input.modo_datos_sentera != 'ejemplo') || (input.tipo == 'phantom' && input.modo_datos_phantom != 'ejemplo')"
}

aplicar_controles_ejemplo_cipat <- function(config, session) {
  for (k in names(config$texto)) shiny::updateTextInput(session,k,value=config$texto[[k]])
  for (k in names(config$numeros)) if (!is.null(config$numeros[[k]])) shiny::updateNumericInput(session,k,value=config$numeros[[k]])
  for (k in setdiff(names(config$radios), c("metodo", "metodo_phantom"))) if (!is.null(config$radios[[k]])) shiny::updateRadioButtons(session,k,selected=config$radios[[k]])
  for (k in names(config$selectores)) if (!is.null(config$selectores[[k]])) shiny::updateSelectInput(session,k,selected=config$selectores[[k]])
  for (k in names(config$checks)) shiny::updateCheckboxInput(session,k,value=isTRUE(config$checks[[k]]))
  for (k in names(config$grupos)) shiny::updateCheckboxGroupInput(session,k,selected=if(is.null(config$grupos[[k]])) character() else config$grupos[[k]])
  if (!is.null(config$fecha)) shiny::updateDateInput(session,"fecha",value=config$fecha)
}

registrar_ejemplos_cipat <- function(input,output,session,vectoriales,editor,ortomosaicos,raiz=getwd()) {
  activo <- shiny::reactiveVal(NULL)
  memoria <- new.env(parent=emptyenv());memoria$personal<-NULL
  capturar <- function() {
    campos<-campos_ejemplos_cipat()
    datos<-lapply(campos,function(ks) stats::setNames(lapply(ks,function(k) input[[k]]),ks))
    for(k in names(datos$texto)) if(is.null(datos$texto[[k]])) datos$texto[[k]]<-""
    datos$fecha<-input$fecha
    list(controles=datos,seleccion=vectoriales$seleccion(),
      formulas=stats::setNames(lapply(c("sentera","sentera_multibanda","phantom"),function(k) editor[[k]]),c("sentera","sentera_multibanda","phantom")),
      bandas=ortomosaicos$guardar())
  }
  restaurar <- function() {
    p<-memoria$personal
    if (is.null(p)) return(invisible(NULL))
    for(k in names(p$formulas)) editor[[k]]<-p$formulas[[k]]
    ortomosaicos$restaurar(p$bandas)
    for(k in names(p$seleccion)) vectoriales$seleccionar(p$seleccion[[k]],k)
    aplicar_controles_ejemplo_cipat(p$controles,session)
    clave_sentera<-if(identical(p$controles$selectores$origen_sentera,"multibanda")) "sentera_multibanda" else "sentera"
    shiny::updateCheckboxGroupInput(session,"indices",choices=names(p$formulas[[clave_sentera]]),
      selected=if(is.null(p$controles$grupos$indices)) character() else p$controles$grupos$indices)
    shiny::updateCheckboxGroupInput(session,"indices_rgb",choices=names(p$formulas$phantom),
      selected=if(is.null(p$controles$grupos$indices_rgb)) character() else p$controles$grupos$indices_rgb)
    memoria$personal<-NULL
  }
  cargar <- function(modulo) {
    config<-configuracion_ejemplo_cipat(modulo,raiz)
    # Preparar y comprobar las entradas antes de modificar controles.
    if (!dir.exists(config$texto$salida) && !dir.create(config$texto$salida,recursive=TRUE)) stop("No se pudo crear la carpeta de resultados de prueba.")
    if (is.null(memoria$personal)) memoria$personal<-capturar()
    vectoriales$importar(config$parcelas,modulo)
    editor[[config$clave_formulas]]<-config$formulas
    ortomosaicos$restaurar(list()) # los ejemplos usan el orden de bandas documentado
    aplicar_controles_ejemplo_cipat(config,session)
    # El catálogo puede recibir también la actualización de origen_sentera.
    shiny::updateCheckboxGroupInput(session,if(modulo=="sentera") "indices" else "indices_rgb",
      choices=names(config$formulas),selected=unname(config$grupos[[1]]))
    activo(modulo)
    invisible(config)
  }
  shiny::observeEvent(list(input$tipo,input$modo_datos_sentera,input$modo_datos_phantom), {
    mod<-input$tipo
    if(length(mod)!=1L || !mod %in% c("sentera","phantom")) return(NULL)
    modo<-input[[paste0("modo_datos_",mod)]]
    tryCatch({
      if (identical(modo,"ejemplo")) {
        if (!identical(activo(),mod)) cargar(mod)
      } else if (!is.null(activo())) {restaurar();activo(NULL)}
    },error=function(e){
      shiny::updateRadioButtons(session,paste0("modo_datos_",mod),selected="propios")
      shiny::showNotification(conditionMessage(e),type="error",duration=NULL)
    })
  },ignoreInit=FALSE)
  shiny::observeEvent(input$recargar_ejemplo, {
    mod<-input$tipo
    if(identical(input[[paste0("modo_datos_",mod)]],"ejemplo"))
      tryCatch(cargar(mod),error=function(e) shiny::showNotification(conditionMessage(e),type="error",duration=NULL))
  })
  output$informacion_ejemplo<-shiny::renderUI({
    mod<-activo()
    if(is.null(mod)) return(NULL)
    shiny::div(class="example-details",
      shiny::div(class="example-dataset-row",
        shiny::tags$strong(if(mod=="sentera") "Sentera multiespectral · BH16" else "RGB y altura del dosel · BH16"),
        ayuda_contextual("Acerca del conjunto de ejemplo",
          shiny::tags$p("Evaluación del 11 de septiembre de 2026. Incluye 48 parcelas con sus imágenes y delimitaciones."),
          shiny::tags$p(if(mod=="sentera") "Conjunto multiespectral para explorar índices de vegetación y cobertura." else "Conjunto RGB con modelo de altura del dosel para explorar color, cobertura y estructura de la vegetación."),
          compacta=TRUE)))
  })
  list(activo=activo,es_ejemplo=shiny::reactive(!is.null(activo()) && identical(activo(),input$tipo)))
}
