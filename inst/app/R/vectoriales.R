# Catálogo de rutas locales durante la sesión. Cada módulo conserva su selección.
etiquetas_vectoriales_cipat <- function(catalogo) {
  if (!length(catalogo)) return(stats::setNames(character(), character()))
  nombres <- vapply(catalogo, function(x) basename(x$ruta), character(1))
  duplicados <- duplicated(tolower(nombres)) | duplicated(tolower(nombres), fromLast = TRUE)
  nombres[duplicados] <- paste0(nombres[duplicados], " · ", sprintf("%02d", which(duplicados)))
  stats::setNames(names(catalogo), nombres)
}

selector_vectoriales_cipat <- function() {
  shiny::div(class = "control-section vector-catalogue",
    shiny::conditionalPanel("input.tipo == 'sentera'",
      shiny::selectizeInput("parcelas_sentera", "Vectorial para Sentera", c("Sin vectorial seleccionado" = "__ninguno__"),
        options = list(placeholder = "Selecciona las parcelas de Sentera"))),
    shiny::conditionalPanel("input.tipo == 'phantom'",
      shiny::selectizeInput("parcelas_phantom", "Vectorial para RGB convencional", c("Sin vectorial seleccionado" = "__ninguno__"),
        options = list(placeholder = "Selecciona las parcelas de RGB"))),
    shiny::uiOutput("vectoriales_asignados"))
}

registrar_vectoriales_cipat <- function(input, output, session, elegir_archivo = file.choose) {
  catalogo <- shiny::reactiveVal(list())
  seleccion <- shiny::reactiveValues(sentera = "__ninguno__", phantom = "__ninguno__")
  importar <- function(ruta, modulo) {
    if (!is.character(modulo) || length(modulo) != 1L || is.na(modulo) || !modulo %in% c("sentera", "phantom")) stop("Selecciona un módulo antes de importar las parcelas.")
    if (!is.character(ruta) || length(ruta) != 1L || is.na(ruta) || !nzchar(trimws(ruta))) stop("No se seleccionó un archivo.")
    ruta <- normalizePath(normalizar_ruta(ruta), winslash = "/", mustWork = TRUE)
    if (!tolower(tools::file_ext(ruta)) %in% c("shp", "gpkg") || dir.exists(ruta)) {
      stop("Selecciona el archivo .shp o .gpkg de parcelas; no sus archivos auxiliares .dbf, .shx o .prj.")
    }
    c <- shiny::isolate(catalogo())
    rutas <- vapply(c, function(x) x$ruta, character(1))
    comparar <- function(x) if (.Platform$OS.type == "windows") tolower(x) else x
    coincidencia <- match(comparar(ruta), comparar(rutas))
    if (is.na(coincidencia)) {
      clave <- sprintf("vector_%04d", length(c) + 1L)
      c[[clave]] <- list(ruta = ruta)
      catalogo(c)
    } else clave <- names(c)[coincidencia]
    seleccion[[modulo]] <- clave
    invisible(clave)
  }
  shiny::observeEvent(input$elegir_polys, {
    modulo <- shiny::isolate(input$tipo)
    tryCatch(importar(elegir_archivo(), modulo),
      error = function(e) shiny::showNotification(conditionMessage(e), type = "error", duration = 8))
  })
  for (modulo in c("sentera", "phantom")) local({
    mod <- modulo; campo <- paste0("parcelas_", mod)
    shiny::observeEvent(input[[campo]], {
      clave <- input[[campo]]
      if (length(clave) != 1L || !clave %in% c("__ninguno__", names(catalogo()))) return(NULL)
      if (!identical(seleccion[[mod]], clave)) seleccion[[mod]] <- clave
    }, ignoreNULL = TRUE)
    shiny::observe({
      opciones <- c("Sin vectorial seleccionado" = "__ninguno__", etiquetas_vectoriales_cipat(catalogo()))
      actual <- seleccion[[mod]]
      shiny::freezeReactiveValue(input, campo)
      shiny::updateSelectizeInput(session, campo, choices = opciones, selected = actual, server = FALSE)
    })
  })
  parcelas <- shiny::reactive({
    mod <- input$tipo
    if (length(mod) != 1L || !mod %in% c("sentera", "phantom")) return(list(ruta = "", id = "id", clave = "__ninguno__"))
    clave <- seleccion[[mod]]; registro <- catalogo()[[clave]]
    id <- input[[paste0("id_", mod)]]
    if (is.null(id)) id <- "id"
    list(ruta = if (is.null(registro)) "" else registro$ruta, id = trimws(id), clave = clave)
  })
  output$nombre_polys <- shiny::renderText({
    ruta <- parcelas()$ruta
    if (!nzchar(ruta)) "Sin archivo seleccionado para este módulo" else basename(ruta)
  })
  output$vectoriales_asignados <- shiny::renderUI({
    c <- catalogo(); etiquetas <- etiquetas_vectoriales_cipat(c)
    texto <- function(mod) {
      clave <- seleccion[[mod]]
      if (clave == "__ninguno__") "Sin seleccionar" else names(etiquetas)[match(clave, etiquetas)]
    }
    shiny::tagList(
      shiny::tags$p(class = "ib-note", paste(length(c), "vectoriales importados en esta sesión. La selección se conserva por módulo.")),
      shiny::tags$div(class = "vector-assignments",
        shiny::tags$div(shiny::tags$strong("Sentera"), shiny::tags$span(texto("sentera"))),
        shiny::tags$div(shiny::tags$strong("RGB"), shiny::tags$span(texto("phantom")))))
  })
  seleccionar <- function(clave,modulo) {
    if(!modulo %in% c("sentera","phantom") || !clave %in% c("__ninguno__",names(shiny::isolate(catalogo())))) stop("La selección vectorial no está disponible.")
    seleccion[[modulo]]<-clave
    invisible(clave)
  }
  list(parcelas = parcelas, importar = importar, seleccionar = seleccionar, catalogo = shiny::reactive(catalogo()),
    seleccion = shiny::reactive(list(sentera = seleccion$sentera, phantom = seleccion$phantom)))
}
