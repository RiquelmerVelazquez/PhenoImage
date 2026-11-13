# Descarga opcional: unión por parcela de las dos evaluaciones del mismo lote/fecha.
# Reutiliza las métricas calculadas. No procesa ni remuestrea imágenes.
ib_fecha <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x <- trimws(as.character(x)); z <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (anyNA(z) || any(format(z, "%Y-%m-%d") != x)) stop("Indica la fecha en formato AAAA-MM-DD.")
  z
}
ib_leer <- function(path, nombre, hoja = 1) {
  csv <- grepl("\\.csv$", nombre, ignore.case = TRUE)
  d <- if (csv) utils::read.csv(path, check.names = FALSE, colClasses = "character", fileEncoding = "UTF-8-BOM")
    else openxlsx::read.xlsx(path, sheet = hoja, check.names = FALSE, detectDates = TRUE)
  if (!is.data.frame(d) || !nrow(d) || !ncol(d)) stop("La hoja seleccionada no contiene parcelas.")
  if (anyDuplicated(names(d))) stop("El archivo tiene columnas con nombres duplicados.")
  meta <- list()
  if (!csv && "Parametros" %in% openxlsx::getSheetNames(path)) {
    p <- openxlsx::read.xlsx(path, sheet = "Parametros", check.names = FALSE)
    if (all(c("Parametro", "Valor") %in% names(p))) meta <- as.list(stats::setNames(as.character(p$Valor), as.character(p$Parametro)))
  }
  lote <- if ("Lote" %in% names(d) && length(unique(d$Lote)) == 1) as.character(d$Lote[1]) else if (!is.null(meta$Lote)) meta$Lote else ""
  fecha <- if ("Fecha" %in% names(d) && length(unique(d$Fecha)) == 1) as.character(d$Fecha[1]) else if (!is.null(meta$Fecha)) meta$Fecha else ""
  list(tabla = d, nombre = nombre, hoja = as.character(hoja), lote = lote, fecha = fecha, meta = meta)
}
ib_actual <- function(x) {
  if (is.null(x)) return(NULL)
  d <- tryCatch(preparar_eda(x)$tabla, error = function(e) x$tabla)
  meta <- list()
  if (!is.null(x$excel) && file.exists(x$excel) && "Parametros" %in% openxlsx::getSheetNames(x$excel)) {
    p <- openxlsx::read.xlsx(x$excel, sheet = "Parametros", check.names = FALSE)
    if (all(c("Parametro", "Valor") %in% names(p))) meta <- as.list(stats::setNames(as.character(p$Valor), as.character(p$Parametro)))
  }
  list(tabla = d, nombre = basename(x$excel), hoja = if (x$tipo == "sentera") "HTP" else "HTP_Phantom",
    lote = as.character(x$fuentes$lote), fecha = as.character(x$fuentes$fecha), meta = meta)
}
ib_atributo <- function(n) {
  n <- sub("^Parcela_", "", n)
  alias <- c(n_line = "N_line", cod_local = "Cod_local", row = "Row", column = "Column", rep = "Rep", repeticion = "Rep", bloque = "Rep", original_i = "Original_ID", original_id = "Original_ID", genotipo = "Original_ID", genotype = "Original_ID")
  low <- tolower(n)
  if (low %in% names(alias)) unname(alias[[low]]) else n
}
ib_coalescer <- function(a, b, nombre) {
  # Los atributos se comparan como texto; se conservan sus valores tipados.
  av <- trimws(as.character(a)); bv <- trimws(as.character(b))
  oka <- !is.na(av) & nzchar(av); okb <- !is.na(bv) & nzchar(bv)
  if (any(oka & okb & av != bv)) stop("Conflicto de atributos en ", nombre, ". Revisa genotipos, repeticiones y parcelas; no se sustituyen automáticamente.")
  a[!oka & okb] <- b[!oka & okb]; a
}
ib_preparar <- function(x, sensor, campo, lote, fecha) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "ib_error")) stop(x$message)
  d <- normalizar_resultados_cipat(x$tabla, sensor); lote <- trimws(lote)
  if (!length(fecha) || !nzchar(trimws(as.character(fecha)))) stop("Completa la fecha de ", sensor, " (AAAA-MM-DD).")
  fecha <- ib_fecha(fecha)
  if (length(lote) != 1 || !nzchar(lote) || length(fecha) != 1 || is.na(fecha)) stop("Completa lote y fecha de ", sensor, ".")
  if (!campo %in% names(d)) stop("Selecciona la columna ID de ", sensor, ".")
  ids <- trimws(as.character(d[[campo]]))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) stop("Los ID de ", sensor, " deben ser únicos y no estar vacíos.")
  if ("Lote" %in% names(d) && any(is.na(d$Lote) | trimws(as.character(d$Lote)) != lote)) stop("El lote indicado difiere del registrado en ", sensor, ".")
  if ("Fecha" %in% names(d) && any(ib_fecha(d$Fecha) != fecha)) stop("La fecha indicada difiere de la registrada en ", sensor, ".")
  if (nzchar(x$lote) && trimws(x$lote) != lote) stop("El lote difiere de los parámetros de ", sensor, ".")
  if (nzchar(x$fecha) && any(ib_fecha(x$fecha) != fecha)) stop("La fecha difiere de los parámetros de ", sensor, ".")
  out <- data.frame(Plot = ids, Lote = lote, Fecha = rep(fecha, length(ids)), stringsAsFactors = FALSE)
  attrs <- character(); vars <- character()
  for (k in setdiff(names(d), c(campo, "id", "Plot", "Lote", "Fecha"))) {
    n <- ib_atributo(k)
    metrica_conocida <- grepl("^(Red|Green|Blue|RE|NIR|NDVI|NDRE|NDRE1|CVI|GNDVI|CIRE|OSAVI|SAVI|NDWI|EVI|ExG|NGRDI|VARI|GLI|RGBVI|Hue|Saturation|Value|Plant_ground_cover_image|Canopy_height_image|Canopy_volume_apparent|RGB_Entropy|RGB_Contrast|RGB_Homogeneity|RGB_Red|RGB_Green|RGB_Blue|Sentera_)(_|$)", k)
    atributo <- startsWith(k, "Parcela_") || n %in% c("N_line", "Cod_local", "Row", "Column", "Rep", "Original_ID", "Origen_datos") || (!metrica_conocida && !is.numeric(d[[k]]) && !is.logical(d[[k]]) && !grepl("_(sum|mean|median|sd|cv|min|max|p[0-9]+|pct|pix|m2|IQR)$", k))
    if (!atributo) {
      n <- k
      # CSV se lee como texto para proteger los ID; las métricas se convierten.
      v <- if (is.numeric(d[[k]])) as.numeric(d[[k]]) else suppressWarnings(as.numeric(as.character(d[[k]])))
      if (any(!is.na(d[[k]]) & nzchar(trimws(as.character(d[[k]]))) & is.na(v))) stop("La métrica ", k, " contiene valores no numéricos.")
      if (any(is.infinite(v))) stop("La métrica ", k, " contiene valores infinitos.")
      if (sensor == "RGB" && grepl("^(Red|Green|Blue)_", n)) n <- paste0("RGB_", n)
      out[[n]] <- v; vars <- c(vars, n)
    } else {
      if (n %in% names(out)) out[[n]] <- ib_coalescer(out[[n]], d[[k]], n) else out[[n]] <- d[[k]]
      attrs <- unique(c(attrs, n))
    }
  }
  list(tabla = out, atributos = attrs, variables = vars, sensor = sensor, nombre = x$nombre, hoja = x$hoja, meta = x$meta)
}
ib_integrar <- function(sentera, rgb, permitir_uno = FALSE) {
  if (is.null(sentera) && is.null(rgb)) stop("Procesa o importa al menos una evaluación.")
  if ((is.null(sentera) || is.null(rgb)) && !permitir_uno) stop("Carga Sentera y RGB, o marca la opción de exportar solo el sensor disponible.")
  fuentes <- Filter(Negate(is.null), list(sentera, rgb))
  if (length(fuentes) == 2 && (!identical(sentera$tabla$Lote[1], rgb$tabla$Lote[1]) || !identical(sentera$tabla$Fecha[1], rgb$tabla$Fecha[1]))) stop("Las evaluaciones tienen distinto lote o fecha. Selecciona el mismo ensayo y fecha; no se mezclan vuelos automáticamente.")
  ids <- unique(unlist(lapply(fuentes, function(x) x$tabla$Plot), use.names = FALSE))
  d <- data.frame(Plot = ids, Lote = fuentes[[1]]$tabla$Lote[1], Fecha = rep(fuentes[[1]]$tabla$Fecha[1], length(ids)), stringsAsFactors = FALSE)
  attrs <- unique(unlist(lapply(fuentes, `[[`, "atributos"))); dic <- list(); prov <- list()
  for (f in fuentes) {
    pos <- match(ids, f$tabla$Plot)
    for (k in f$atributos) {
      v <- f$tabla[[k]][pos]
      if (k %in% names(d)) d[[k]] <- ib_coalescer(d[[k]], v, k) else d[[k]] <- v
    }
    for (k in f$variables) {
      n <- k
      # Si un Excel ya contiene métricas RGB, conserva ambas fuentes sin borrarlas.
      if (n %in% names(d)) {
        anterior <- paste0("Sentera_", n)
        if (anterior %in% names(d)) stop("Colisión de columnas: ", n)
        names(d)[names(d) == n] <- anterior
        for (i in seq_along(dic)) if (dic[[i]]$Columna == n) dic[[i]]$Columna <- anterior
      }
      if (n %in% attrs) stop("Una métrica coincide con un atributo: ", n)
      d[[n]] <- f$tabla[[k]][pos]
      dic[[length(dic) + 1L]] <- data.frame(Columna = n, Sensor = f$sensor, Nombre_original = if (f$sensor == "RGB") sub("^RGB_(Red|Green|Blue)_", "\\1_", k) else k, stringsAsFactors = FALSE)
    }
    prov[[length(prov) + 1L]] <- data.frame(Sensor = f$sensor, Archivo = f$nombre, Hoja = f$hoja, Lote = d$Lote[1], Fecha = d$Fecha[1], Parcelas = nrow(f$tabla), stringsAsFactors = FALSE)
  }
  s <- if (is.null(sentera)) rep(FALSE, length(ids)) else ids %in% sentera$tabla$Plot
  r <- if (is.null(rgb)) rep(FALSE, length(ids)) else ids %in% rgb$tabla$Plot
  d$Origen_datos <- ifelse(s & r, "Sentera + RGB", ifelse(s, "Solo Sentera", "Solo RGB"))
  cabecera <- c("N_line", "Cod_local", "Plot", "Row", "Column", "Rep", "Original_ID", "Lote", "Fecha", "Origen_datos")
  for (k in setdiff(cabecera, names(d))) d[[k]] <- NA
  d <- d[c(cabecera, setdiff(names(d), cabecera))]
  dic <- if (length(dic)) do.call(rbind, dic) else data.frame(Columna=character(),Sensor=character(),Nombre_original=character())
  parametros <- do.call(rbind, lapply(fuentes, function(f) {
    m <- f$meta
    if (!length(m)) return(data.frame(Sensor=f$sensor,Parametro="Estadísticas y máscara",Valor="No declarado en el archivo importado"))
    data.frame(Sensor=f$sensor,Parametro=names(m),Valor=as.character(unlist(m)),stringsAsFactors=FALSE)
  }))
  d <- normalizar_resultados_cipat(d, "integrado")
  list(tabla=d, diccionario=dic, fuentes=do.call(rbind,prov), parametros=parametros,
    coincidencias=sum(s&r), solo_sentera=sum(s&!r), solo_rgb=sum(!s&r))
}
ib_escribir_excel <- function(x, file, seleccion) {
  wb <- openxlsx::createWorkbook(); d <- x$tabla
  identidad <- c("N_line", "Cod_local", "Plot", "Row", "Column", "Rep", "Original_ID", "Lote", "Fecha", "Origen_datos")
  seleccion <- intersect(seleccion, names(d))
  hojas <- list(HTP=d, `Variables a Evaluar`=d[unique(c(identidad, seleccion))], Fuentes=x$fuentes, Diccionario=x$diccionario, Parametros=x$parametros)
  for (s in names(hojas)) {
    dd <- hojas[[s]]; openxlsx::addWorksheet(wb,s)
    openxlsx::writeData(wb,s,dd,withFilter=TRUE,headerStyle=openxlsx::createStyle(fgFill="#1B552F",fontColour="#FFFFFF",textDecoration="bold"))
    openxlsx::freezePane(wb,s,firstRow=TRUE,firstCol=TRUE)
    openxlsx::setColWidths(wb,s,seq_len(ncol(dd)),widths=18)
    if (nrow(dd)) for (k in which(vapply(dd,inherits,logical(1),"Date"))) openxlsx::addStyle(wb,s,openxlsx::createStyle(numFmt="yyyy-mm-dd"),rows=seq_len(nrow(dd))+1,cols=k,gridExpand=TRUE)
  }
  openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
}
ib_escribir_csv <- function(x,file) {
  d <- x$tabla
  for (k in names(d)) if (is.character(d[[k]])) d[[k]] <- enc2utf8(d[[k]])
  con <- base::file(file,open="wb"); on.exit(close(con),add=TRUE)
  writeBin(as.raw(c(239,187,191)),con)
  utils::write.table(d,con,sep=",",row.names=FALSE,col.names=TRUE,na="",quote=TRUE,qmethod="double")
}

integracion_ui <- function() {
  fuente <- function(sensor, prefijo) {
    shiny::div(class = "ib-source",
      shiny::tags$h4(sensor),
      shiny::radioButtons(paste0(prefijo,"_modo"), "Origen de los datos", inline=TRUE,
        choices=c("Resultado de esta sesión"="sesion", "Importar Excel / CSV"="archivo")),
      shiny::conditionalPanel(paste0("input.",prefijo,"_modo == 'archivo'"),
        shiny::fileInput(paste0(prefijo,"_archivo"), NULL, accept=c(".xlsx", ".csv"), buttonLabel="Importar", placeholder="Ningún archivo seleccionado"),
        shiny::uiOutput(paste0(prefijo,"_hoja_ui"))),
      shiny::uiOutput(paste0(prefijo,"_info")),
      shiny::uiOutput(paste0(prefijo,"_id_ui")),
      shiny::div(class="ib-context",
        shiny::textInput(paste0(prefijo,"_lote"), "Lote / ensayo"),
        shiny::textInput(paste0(prefijo,"_fecha"), "Fecha del vuelo", placeholder="AAAA-MM-DD")))
  }
  shiny::tags$details(class="card ib-export",
    shiny::tags$summary(shiny::icon("file-export"), shiny::span("Exportar base integrada"),
      shiny::span(class="ib-optional", "Opcional")),
    shiny::div(class="ib-body",
      shiny::tags$p(class="ib-intro", "Reúne las estadísticas de Sentera y RGB de un mismo lote y fecha. Puedes usar los resultados de la sesión o importar tablas guardadas."),
      shiny::div(class="ib-sources", fuente("Sensor Sentera Double 4K", "ib_s"), fuente("Sensor RGB convencional", "ib_r")),
      shiny::checkboxInput("ib_uno", "Exportar únicamente el sensor disponible", FALSE),
      shiny::tags$p(class="ib-note", "La unión utiliza el valor del ID, no el orden de las filas. Revisa que cada ID corresponda a la misma parcela. Las bandas del RGB convencional se identifican como RGB_Red, RGB_Green y RGB_Blue."),
      shiny::actionButton("ib_preparar", "Preparar base", icon=shiny::icon("table"), class="btn-primary"),
      shiny::uiOutput("ib_estado"),
      shiny::conditionalPanel("output.ib_lista",
        shiny::selectizeInput("ib_variables", "Variables a Evaluar (hoja adicional de Excel)", choices=character(), multiple=TRUE),
        shiny::tags$p(class="ib-note", "La hoja HTP y el CSV incluyen todas las columnas. Esta selección solo modifica la hoja Variables a Evaluar."),
        shiny::div(class="bottom-actions", shiny::downloadButton("ib_excel", "Descargar Excel"), shiny::downloadButton("ib_csv", "Descargar CSV")),
        DT::DTOutput("ib_vista"))))
}

registrar_integracion <- function(input, output, session, actuales) {
  exportacion <- shiny::reactiveVal(NULL)
  fuente <- function(prefijo, tipo) {
    shiny::reactive(tryCatch({
      if (is.null(input[[paste0(prefijo,"_modo")]]) || input[[paste0(prefijo,"_modo")]] == "sesion") return(ib_actual(actuales()[[tipo]]))
      f <- input[[paste0(prefijo,"_archivo")]]
      if (is.null(f)) return(NULL)
      hoja <- input[[paste0(prefijo,"_hoja")]]
      if (!grepl("\\.csv$", f$name, ignore.case=TRUE)) {
        hs <- openxlsx::getSheetNames(f$datapath)
        if (is.null(hoja) || !hoja %in% hs) hoja <- if ("HTP" %in% hs) "HTP" else if ("HTP_Phantom" %in% hs) "HTP_Phantom" else hs[1]
      } else hoja <- 1
      ib_leer(f$datapath, f$name, hoja)
    }, error=function(e) structure(list(message=conditionMessage(e)),class="ib_error")))
  }
  s <- fuente("ib_s", "sentera"); r <- fuente("ib_r", "phantom")
  controles <- function(prefijo, raw) {
    output[[paste0(prefijo,"_hoja_ui")]] <- shiny::renderUI({
      f <- input[[paste0(prefijo,"_archivo")]]
      if (is.null(f) || grepl("\\.csv$", f$name, ignore.case=TRUE)) return(NULL)
      hs <- tryCatch(openxlsx::getSheetNames(f$datapath), error=function(e) character())
      if (!length(hs)) return(NULL)
      elegida <- if ("HTP" %in% hs) "HTP" else if ("HTP_Phantom" %in% hs) "HTP_Phantom" else hs[1]
      shiny::selectInput(paste0(prefijo,"_hoja"), "Hoja de resultados", choices=hs, selected=elegida)
    })
    output[[paste0(prefijo,"_info")]] <- shiny::renderUI({
      tryCatch({
        x <- raw()
        if (inherits(x,"ib_error")) stop(x$message)
        if (is.null(x)) return(shiny::tags$p(class="ib-note", "Sin datos. Procesa una evaluación o importa una tabla."))
        shiny::div(class="ib-file-info", shiny::tags$strong(x$nombre), shiny::span(paste(nrow(x$tabla), "parcelas ·", ncol(x$tabla), "columnas")))
      }, error=function(e) shiny::tags$p(class="ib-error", conditionMessage(e)))
    })
    # Recrear las opciones junto con el control, incluso tras reconstruir Resultados.
    output[[paste0(prefijo,"_id_ui")]] <- shiny::renderUI({
      x <- raw()
      ns <- if (is.null(x) || inherits(x, "ib_error")) character() else names(x$tabla)
      prioridad <- c("id", "Plot", "Parcela_Plot", "ID", "plot")
      previo <- shiny::isolate(input[[paste0(prefijo,"_id")]])
      elegido <- if (length(previo) == 1L && previo %in% ns) previo else {
        candidatos <- intersect(prioridad, ns)
        if (length(candidatos)) candidatos[1] else if (length(ns)) ns[1] else character()
      }
      shiny::selectInput(paste0(prefijo,"_id"), "Columna de parcela (ID)",
                         choices = ns, selected = elegido)
    })
    anterior <- shiny::reactiveVal(NULL)
    shiny::observeEvent(raw(), {
      x <- tryCatch(raw(),error=function(e) NULL)
      if (inherits(x,"ib_error")) x <- NULL
      if (identical(x, anterior())) return(NULL)
      anterior(x)
      ns <- if (is.null(x)) character() else names(x$tabla)
      prioridad <- c("id", "Plot", "Parcela_Plot", "ID", "plot")
      elegido <- intersect(prioridad,ns)
      shiny::updateTextInput(session,paste0(prefijo,"_lote"),value=if(is.null(x)) "" else x$lote)
      shiny::updateTextInput(session,paste0(prefijo,"_fecha"),value=if(is.null(x)) "" else x$fecha)
    }, ignoreNULL=FALSE)
  }
  controles("ib_s",s); controles("ib_r",r)
  shiny::observeEvent(list(s(),r(), input$ib_s_id,input$ib_r_id,input$ib_s_lote,input$ib_r_lote,
    input$ib_s_fecha,input$ib_r_fecha,input$ib_uno), { exportacion(NULL) }, ignoreInit=TRUE, ignoreNULL=FALSE, priority=10)
  shiny::observeEvent(input$ib_preparar, {
    exportacion(NULL)
    tryCatch({
      z <- ib_integrar(
        ib_preparar(s(),"Sentera",input$ib_s_id,input$ib_s_lote,input$ib_s_fecha),
        ib_preparar(r(),"RGB",input$ib_r_id,input$ib_r_lote,input$ib_r_fecha), isTRUE(input$ib_uno))
      exportacion(z)
      opciones <- z$diccionario$Columna
      preferidas <- c("NDVI_mean", "Plant_ground_cover_image_NDVI_pct", "Plant_ground_cover_image_RGB_pct", "Canopy_height_image_p90")
      shiny::updateSelectizeInput(session,"ib_variables",choices=opciones,selected=intersect(preferidas,opciones),server=TRUE)
      shiny::showNotification("Base integrada preparada. Revisa las coincidencias antes de descargar.",type="message",duration=5)
    }, error=function(e) shiny::showNotification(conditionMessage(e),type="error",duration=NULL))
  }, ignoreInit=TRUE)
  output$ib_lista <- shiny::reactive(!is.null(exportacion()))
  shiny::outputOptions(output,"ib_lista",suspendWhenHidden=FALSE)
  output$ib_estado <- shiny::renderUI({
    z <- exportacion()
    if (is.null(z)) return(NULL)
    shiny::div(class="ib-summary",
      shiny::tags$strong(paste(nrow(z$tabla),"parcelas ·",nrow(z$diccionario),"variables")),
      shiny::span(paste("Ambos sensores:",z$coincidencias,"· Solo Sentera:",z$solo_sentera,"· Solo RGB:",z$solo_rgb)),
      if(z$solo_sentera+z$solo_rgb>0) shiny::tags$p("Las parcelas sin coincidencia se conservan; las métricas del sensor faltante quedan vacías."))
  })
  output$ib_vista <- DT::renderDT({
    z <- exportacion(); shiny::req(!is.null(z))
    DT::datatable(z$tabla,rownames=FALSE,filter="top",options=list(pageLength=5,scrollX=TRUE,language=list(url=NULL,search="Buscar:",lengthMenu="Mostrar _MENU_ parcelas",info="_START_ a _END_ de _TOTAL_ parcelas",paginate=list(previous="Anterior",`next`="Siguiente"))))
  })
  nombre <- function(ext) {
    z <- exportacion(); shiny::req(!is.null(z))
    lote <- gsub("[^[:alnum:]_-]","_",z$tabla$Lote[1])
    paste0("HTP_Integrado_",lote,"_",format(z$tabla$Fecha[1],"%Y-%m-%d"),".",ext)
  }
  output$ib_excel <- shiny::downloadHandler(filename=function() nombre("xlsx"),content=function(file) {
    z <- exportacion(); shiny::req(!is.null(z)); ib_escribir_excel(z,file,input$ib_variables)
  })
  output$ib_csv <- shiny::downloadHandler(filename=function() nombre("csv"),content=function(file) {
    z <- exportacion(); shiny::req(!is.null(z)); ib_escribir_csv(z,file)
  })
  invisible(list(base=exportacion,sentera=s,rgb=r))
}
