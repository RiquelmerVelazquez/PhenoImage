# Editor restringido: las fórmulas se interpretan por árbol; nunca se usa eval.
indices_originales_cipat <- function(tipo) {
  if (tipo == "phantom") return(c(ExG="2 * G - R - B",
    NGRDI="(G - R) / (G + R + 1e-6)", VARI="(G - R) / (G + R - B + 1e-6)",
    GLI="(2 * G - R - B) / (2 * G + R + B + 1e-6)",
    RGBVI="(G^2 - R * B) / (G^2 + R * B + 1e-6)"))
  c(NDVI="(NIR - R) / (NIR + R)", NDRE="(NIR - RE) / (NIR + RE)",
    GNDVI="(NIR - G) / (NIR + G)", CIRE="NIR / RE - 1", CVI="NIR * R / (G * G)",
    NDWI="(G - NIR) / (G + NIR)", OSAVI="1.16 * (NIR - R) / (NIR + R + 0.16)",
    SAVI="1.5 * (NIR - R) / (NIR + R + 0.5)", EVI="2.5 * (NIR - R) / (NIR + 6 * R - 7.5 * B + 1)")
}

# Compatibilidad de lectura: las evaluaciones históricas pueden llamarlo NDRE1.
normalizar_columnas_ndre_cipat <- function(tabla) {
  nombres <- names(tabla)
  nuevos <- sub("^NDRE1_", "NDRE_", nombres)
  cambiar <- startsWith(nombres, "NDRE1_") & !nuevos %in% nombres
  names(tabla)[cambiar] <- nuevos[cambiar]
  tabla
}

arbol_formula_cipat <- function(txt, variables) {
  if (length(txt) != 1L || is.na(txt) || nchar(txt) > 1000) stop("Fórmula vacía o demasiado larga.")
  expr <- parse(text=txt, keep.source=FALSE)
  if (length(expr) != 1L) stop("Escribe una sola expresión por índice.")
  revisar <- function(x, profundidad=0L) {
    if (profundidad > 40) stop("Fórmula demasiado compleja.")
    if (is.numeric(x) && length(x)==1L && is.finite(x)) return(invisible(NULL))
    if (is.symbol(x) && as.character(x) %in% variables) return(invisible(NULL))
    if (!is.call(x) || !is.symbol(x[[1]])) stop("Usa solamente bandas, números y operaciones permitidas.")
    op <- as.character(x[[1]]); n <- length(x)-1L
    valido <- (op %in% c("+", "-") && n %in% 1:2) ||
      (op %in% c("*", "/", "^") && n==2L) ||
      (op %in% c("(", "sqrt", "abs", "log", "exp") && n==1L)
    if (!valido) stop("Operación no permitida: ", op)
    if (!is.null(names(x)) && any(nzchar(names(x)))) stop("No uses argumentos nombrados.")
    for (j in seq.int(2L, length(x))) revisar(x[[j]], profundidad+1L)
  }
  revisar(expr[[1]]); expr[[1]]
}

ejecutar_formula_cipat <- function(txt, bandas) {
  arbol <- arbol_formula_cipat(txt, names(bandas))
  resolver <- function(x) {
    if (is.numeric(x)) return(x)
    if (is.symbol(x)) return(bandas[[as.character(x)]])
    op <- as.character(x[[1]]); a <- resolver(x[[2]])
    if (length(x)==2L) return(switch(op, `+`=a, `-`=-a, `(`=a,
      sqrt=sqrt(a), abs=abs(a), log=log(a), exp=exp(a)))
    b <- resolver(x[[3]])
    switch(op, `+`=a+b, `-`=a-b, `*`=a*b, `^`=a^b,
      `/`=if (inherits(b,"SpatRaster")) division_segura(a,b) else if (abs(b)<1e-8) a*NA_real_ else a/b)
  }
  r <- resolver(arbol)
  if (!inherits(r,"SpatRaster")) r <- bandas[[1]]*0+r
  terra::ifel(is.finite(r), r, NA)
}

leer_formulas_cipat <- function(txt, tipo) {
  lineas <- trimws(strsplit(txt, "\n", fixed=TRUE)[[1]])
  lineas <- lineas[nzchar(lineas)]
  if (!length(lineas) || length(lineas)>50) stop("Usa entre 1 y 50 índices.")
  formulas <- character()
  reservados <- c("CON","PRN","AUX","NUL",paste0("COM",1:9),paste0("LPT",1:9),"Red","Green","Blue","RE","NIR","R","G","B","Hue","Saturation","Value",
    "Multibanda_corregida","NDVI_Mascara_Binaria","NDVI_Mascara_Vegetacion","Mascara_ExG_RGB",
    "CHM_cm","Mascara_CHM","CHM_filtrado_cm","Green_vegetacion","Entropy_RGB","Contrast_RGB","Homogeneity_RGB",
    "HSB_Hue","HSB_Saturation","HSB_Brightness","DGCI","Lab_L","Lab_a","Lab_b")
  for (linea in lineas) {
    pos <- regexpr("=",linea,fixed=TRUE)[1]
    if (pos<2) stop("Formato esperado: Nombre = fórmula")
    nm <- trimws(substr(linea,1,pos-1)); f <- trimws(substring(linea,pos+1))
    if (tipo != "phantom" && identical(nm, "NDRE1")) nm <- "NDRE"
    if (!grepl("^[A-Za-z][A-Za-z0-9_]{0,39}$",nm) || tolower(nm) %in% tolower(reservados)) stop("Nombre no permitido o reservado: ",nm)
    if (tolower(nm) %in% tolower(names(formulas))) stop("Nombre duplicado: ",nm)
    arbol_formula_cipat(f, if(tipo=="phantom") c("R","G","B","rn","gn","bn") else c("R","G","B","RE","NIR"))
    formulas[nm] <- f
  }
  obligatorio <- if(tipo=="phantom") "ExG" else "NDVI"
  if (!obligatorio %in% names(formulas)) stop("Conserva ",obligatorio," porque genera la máscara.")
  formulas
}

registrar_formulas_cipat <- function(wb,p) {
  filtros <- vapply(names(p$formulas),function(nm) {
    if(p$tipo=="phantom") return("Sin recorte adicional")
    if(nm %in% c("NDVI","NDRE","GNDVI")) return("Recorte [-1, 1]")
    if(nm=="CIRE") return("Recorte [0, 3]")
    if(nm=="CVI") return("NA si G <= 1 o índice fuera de [0, 100]")
    "Sin recorte adicional"
  },character(1))
  df <- data.frame(Indice=names(p$formulas), Formula=unname(p$formulas), Filtro=filtros,
    Bandas=if(p$tipo=="phantom") "R/G/B originales; rn/gn/bn = banda / Escala_RGB" else "RGB corregido; RE y NIR corregidos y multiplicados por Factor",
    Factor=if(p$tipo=="phantom") NA_real_ else p$factor,
    Division="Denominador absoluto < 1e-8 e infinitos/NaN: NA", stringsAsFactors=FALSE)
  if (p$tipo != "phantom") {
    df$Interpretacion <- ifelse(df$Indice %in% c("OSAVI", "SAVI", "EVI", "MSAVI2", "GOSAVI", "GSAVI", "ARI", "RDVI"),
      "Exploratorio: verificar escala de reflectancia antes de interpretar o comparar fechas",
      "Revisar fórmula, filtros y consistencia radiométrica entre fechas")
    if (identical(p$origen_sentera, "multibanda")) {
      df$Bandas <- "Entrada multibanda R/G/B/RE/NIR; sin matrices ni factor adicional; unidades originales"
    }
  }
  openxlsx::addWorksheet(wb,"Formulas"); openxlsx::writeData(wb,"Formulas",df)
  openxlsx::setColWidths(wb,"Formulas",cols=1:6,widths=c(18,65,48,75,12,55))
  if ("Interpretacion" %in% names(df)) openxlsx::setColWidths(wb,"Formulas",cols=7,widths=85)
  utils::write.csv(df,archivo_salida_cipat(p,"Formulas","csv"),row.names=FALSE,fileEncoding="UTF-8")
}

registrar_editor_indices_cipat <- function(input,output,session) {
  originales_sentera <- indices_originales_cipat("sentera")
  estado <- reactiveValues(sentera=originales_sentera,
    sentera_multibanda=originales_sentera[c("NDVI", "NDRE", "GNDVI")],
    phantom=indices_originales_cipat("phantom"))
  clave_estado <- function(tipo) {
    if (tipo == "sentera" && identical(input$origen_sentera, "multibanda")) "sentera_multibanda" else tipo
  }
  tipo_editor <- reactiveVal("sentera")
  feedback <- reactiveVal(NULL)
  texto <- function(f) paste(names(f),f,sep=" = ",collapse="\n")
  datos <- reactive({
    filtrar_catalogo_cipat(tipo_editor(), input$catalogo_grupo, input$catalogo_buscar)
  })
  elegido <- reactive({
    d <- datos(); if(!nrow(d)) return(NULL)
    i <- input$catalogo_tabla_rows_selected
    if(length(i)!=1L || i<1L || i>nrow(d)) i <- 1L
    d[i,,drop=FALSE]
  })
  output$catalogo_tabla <- DT::renderDT({
    d <- datos()
    DT::datatable(d[,c("indice","nombre","bandas"),drop=FALSE],rownames=FALSE,
      colnames=c("Índice","Nombre","Bandas"),escape=TRUE,
      selection=list(mode="single",selected=if(nrow(d)) 1L else integer()),
      options=list(pageLength=6,lengthChange=FALSE,searching=FALSE,ordering=FALSE,dom="tip",
        language=list(emptyTable="No hay índices que coincidan con tu búsqueda.",
          info="_START_–_END_ de _TOTAL_ índices",infoEmpty="Sin resultados",
          paginate=list(previous="Anterior",`next`="Siguiente"))))
  },server=FALSE)
  output$catalogo_detalle <- renderUI({
    d <- elegido()
    if(is.null(d)) return(div(class="idx-empty",icon("magnifying-glass"),h4("Sin coincidencias"),p("Prueba con otro nombre o selecciona todos los grupos.")))
    nm <- d$indice[1]; actual <- estado[[clave_estado(tipo_editor())]]
    presente <- nm %in% names(actual)
    formula_actual <- if(presente) actual[[nm]] else NULL
    personalizado <- presente && !identical(formula_actual,d$formula[1])
    div(class="idx-detail",
      div(class="idx-badges",span(class="idx-pill",d$grupo[1]),span(class="idx-pill idx-soft",d$bandas[1])),
      div(class="idx-index",nm),h4(d$nombre[1]),p(class="idx-description",d$descripcion[1]),
      span(class="idx-label","FÓRMULA DE REFERENCIA"),tags$pre(class="idx-equation",tags$code(d$formula[1])),
      if(presente) div(class="idx-present",icon("circle-check"),if(personalizado) "Ya está en tus fórmulas, con una edición propia." else "Ya está incluido en tus fórmulas."),
      if(personalizado) tagList(span(class="idx-label","TU FÓRMULA ACTUAL"),tags$pre(class="idx-equation",tags$code(formula_actual))),
      div(class="idx-note",icon("circle-info"),span(d$nota[1])),
      if(nm %in% c("NDVI","ExG")) p(class="idx-mask-note","Este índice define la máscara de vegetación. Editarlo cambia la segmentación y las estadísticas."),
      if(tipo_editor()=="sentera" && nm %in% c("NDVI","NDRE","GNDVI","CIRE","CVI")) p(class="idx-mask-note",
        switch(nm,NDVI="Filtro conservado: recorte entre −1 y 1.",NDRE="Filtro conservado: recorte entre −1 y 1.",GNDVI="Filtro conservado: recorte entre −1 y 1.",CIRE="Filtro conservado: recorte entre 0 y 3.",CVI="Filtro conservado: NA si G ≤ 1 o resultado fuera de 0–100.")),
      p(class="idx-reference",d$referencia[1]),
      actionButton("catalogo_agregar",if(presente) "Ver en mis fórmulas" else "Añadir a mis fórmulas",icon=icon(if(presente) "pen-to-square" else "plus"),class="btn-primary idx-add"))
  })
  output$editor_feedback <- renderUI({
    f <- feedback(); if(is.null(f)) return(NULL)
    div(class=paste("idx-feedback",if(f$ok) "idx-success" else "idx-error"),icon(if(f$ok) "circle-check" else "circle-exclamation"),f$mensaje)
  })
  abrir <- function(tipo) {
    tipo_editor(tipo); feedback(NULL)
    showModal(modalDialog(title=NULL,size="l",easyClose=FALSE,
      div(class="idx-studio",
        div(class="idx-hero",
          div(class="idx-eyebrow","PhenoImage · HERRAMIENTAS DE ANÁLISIS"),
          div(class="idx-heading",h3("Biblioteca de índices"),span(class="idx-module",if(tipo=="phantom") "RGB convencional" else "Sentera Doble 4K")),
          p("Explora las fórmulas disponibles y prepara los índices de tu evaluación.")),
        div(class="idx-band-strip",icon("layer-group"),
          if(tipo=="phantom") "Bandas disponibles: R · G · B" else "Bandas disponibles: R · G · B · RE · NIR",
          span(paste(nrow(catalogo_indices_cipat(tipo)), "índices compatibles"))),
        tabsetPanel(id="indices_pestanas",type="tabs",
          tabPanel("Catálogo",value="catalogo",
            div(class="idx-catalogue",
              div(class="idx-list",
                textInput("catalogo_buscar",label=NULL,value="",placeholder="Buscar por nombre, banda o descripción…"),
                if(tipo=="sentera") radioButtons("catalogo_grupo",NULL,c("Todos","Multiespectral","RGB"),selected="Todos",inline=TRUE),
                DT::DTOutput("catalogo_tabla"),
                p(class="idx-footnote","Selecciona una fila para consultar su ficha.")),
              div(class="idx-detail-wrap",uiOutput("catalogo_detalle"))),
            div(class="idx-source",tags$a(href="https://nepem-ufsc.github.io/pliman/articles/indexes.html",target="_blank",rel="noopener noreferrer","Consultar documentación de índices"),
              span("Las variantes originales de la aplicación se conservan."))),
          tabPanel("Mis fórmulas",value="editor",
            div(class="idx-editor",
              h4("Índices de la evaluación"),p("Modifica una ecuación o agrega una línea con el formato Nombre = fórmula."),
              div(class="idx-note",icon("circle-info"),span(if(tipo=="phantom")
                "R, G y B son los canales originales. rn, gn y bn son los canales divididos por Escala RGB. No se aplica factor inter-cámara." else
                "R, G y B ya están corregidos. RE y NIR incluyen la corrección y el factor inter-cámara elegido. No vuelvas a multiplicarlos por 2.7.")),
              textAreaInput("formulas_editor",label=NULL,value=texto(estado[[clave_estado(tipo)]]),width="100%",height="285px"),
              uiOutput("editor_feedback"),
              tags$details(class="idx-help",tags$summary("Sintaxis y criterios de cálculo"),
                p("Operaciones: + − * / ^; funciones: sqrt(), abs(), log(), exp(). Usa bandas, no nombres de otros índices."),
                p("Conserva NDVI en Sentera y ExG en RGB: definen la máscara. Cambiarlos afecta coberturas y estadísticas. Los valores no finitos y las divisiones con denominador absoluto menor a 1e-8 se guardan como NA."),
                if(tipo=="sentera") p("Filtros históricos: NDVI, NDRE y GNDVI se recortan a [−1,1]; CIRE a [0,3]; CVI excluye G ≤ 1 y valores fuera de [0,100]. Usa un nombre nuevo para calcular sin esos filtros. OSAVI, SAVI y EVI requieren revisar la escala radiométrica.")),
              div(class="idx-editor-tools",actionButton("restaurar_indices","Cargar originales",icon=icon("rotate-left")),downloadButton("exportar_formulas","Guardar fórmulas")),
              p(class="idx-footnote","Los cambios se aplican al pulsar Aplicar. Se conservan durante esta sesión y se registran en cada Excel y Formulas.csv. Puedes guardar una copia de texto para reutilizarla."),
              p(class="idx-footnote","Marca en el panel los índices que deseas calcular. Los nuevos se seleccionan automáticamente; NDVI y ExG siguen disponibles para sus máscaras."))))),
      footer=div(class="idx-footer",span("Prepara tus fórmulas antes de procesar."),modalButton("Cancelar"),
        actionButton("validar_indices","Validar fórmulas",icon=icon("check")),
        actionButton("aplicar_indices","Aplicar",icon=icon("check-double"),class="btn-primary"))))
    # Los controles del modal se vuelven a crear: reiniciar filtros al terminar de montarlos.
    session$onFlushed(function() {
      updateTextInput(session, "catalogo_buscar", value = "")
      if (tipo == "sentera") updateRadioButtons(session, "catalogo_grupo", selected = "Todos")
    }, once = TRUE)
  }
  observeEvent(input$editar_indices,abrir(input$tipo))
  observeEvent(input$formulas_editor,feedback(NULL),ignoreInit=TRUE)
  observeEvent(input$catalogo_agregar,tryCatch({
    d <- elegido(); req(d)
    # Valida el borrador antes de añadir, preservando todas las ediciones propias.
    f <- leer_formulas_cipat(input$formulas_editor,tipo_editor()); nm <- d$indice[1]
    if(!tolower(nm) %in% tolower(names(f))) {
      updateTextAreaInput(session,"formulas_editor",value=paste(input$formulas_editor,paste(nm,d$formula[1],sep=" = "),sep="\n"))
      showNotification(paste(nm,"añadido al borrador. Pulsa Aplicar para conservarlo."),type="message")
    }
    updateTabsetPanel(session,"indices_pestanas",selected="editor")
  },error=function(e) {updateTabsetPanel(session,"indices_pestanas",selected="editor");feedback(list(ok=FALSE,mensaje=conditionMessage(e)))}))
  observeEvent(input$restaurar_indices,{
    updateTextAreaInput(session,"formulas_editor",value=texto(indices_originales_cipat(tipo_editor())))
    showNotification("Originales cargados en el borrador. Puedes cancelar para conservar tus fórmulas aplicadas.",type="message")
  })
  validar <- function() leer_formulas_cipat(input$formulas_editor,tipo_editor())
  output$exportar_formulas <- downloadHandler(filename=function() paste0("Formulas_",tipo_editor(),".txt"),content=function(file) {
    f <- validar();writeLines(texto(f),file,useBytes=TRUE)
  })
  observeEvent(input$validar_indices,{
    updateTabsetPanel(session,"indices_pestanas",selected="editor")
    tryCatch({f <- validar();feedback(list(ok=TRUE,mensaje=paste(length(f),"fórmulas con sintaxis y bandas válidas. Los píxeles se comprueban al procesar.")))},
      error=function(e) feedback(list(ok=FALSE,mensaje=conditionMessage(e))))
  })
  observeEvent(input$aplicar_indices,tryCatch({
    tipo <- tipo_editor(); f <- validar(); anterior <- estado[[clave_estado(tipo)]]; estado[[clave_estado(tipo)]] <- f
    if(tipo=="sentera") {
      nuevos <- setdiff(names(f),names(anterior))
      disponibles <- indices_entrada_sentera(names(f), input$origen_sentera)
      updateCheckboxGroupInput(session,"indices",choices=disponibles,selected=intersect(union(intersect(input$indices,names(f)),nuevos),disponibles))
    } else if (tipo == "phantom") {
      nuevos <- setdiff(names(f), names(anterior))
      seleccion <- intersect(union(as.character(input$indices_rgb), nuevos), names(f))
      updateCheckboxGroupInput(session, "indices_rgb", choices = names(f), selected = seleccion)
    }
    removeModal();showNotification("Fórmulas aplicadas para la siguiente ejecución.",type="message")
  },error=function(e) {updateTabsetPanel(session,"indices_pestanas",selected="editor");feedback(list(ok=FALSE,mensaje=conditionMessage(e)))}))
  estado
}
