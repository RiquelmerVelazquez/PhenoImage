# PhenoImage 4.8.0. Abre este archivo en RStudio y pulsa Source.
local({
  rutas <- Filter(function(x) is.character(x) && length(x) == 1L && file.exists(x),
    lapply(sys.frames(), function(e) e$ofile))
  raiz <- if (length(rutas)) dirname(normalizePath(tail(rutas, 1)[[1]], winslash = "/")) else getwd()
  if (!file.exists(file.path(raiz, "inst", "app", "app.R"))) {
    message("Selecciona DESCRIPTION de la carpeta PhenoImage 4.8.0 descomprimida.")
    raiz <- dirname(file.choose())
  }
  app <- file.path(raiz, "inst", "app")
  if (!file.exists(file.path(app, "app.R"))) stop("No se encontró inst/app/app.R. Descomprime el ZIP completo.")
  if (!requireNamespace("shiny", quietly = TRUE)) stop("Instala shiny antes de abrir la aplicación.")
  anterior <- getwd(); on.exit(setwd(anterior), add = TRUE); setwd(app)
  shiny::runApp(appDir = normalizePath(app, winslash = "/"), launch.browser = TRUE, host = "127.0.0.1")
})
