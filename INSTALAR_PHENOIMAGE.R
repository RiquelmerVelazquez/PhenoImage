# OPCIONAL: actualizar el paquete instalado para seguir usando phenoImage::run_app().
# Detén la app y usa Session > Restart R antes de ejecutar este archivo con Source.
local({
  rutas <- Filter(function(x) is.character(x) && length(x) == 1L && file.exists(x),
    lapply(sys.frames(), function(e) e$ofile))
  raiz <- if (length(rutas)) dirname(normalizePath(tail(rutas, 1)[[1]], winslash = "/")) else getwd()
  if (!file.exists(file.path(raiz, "DESCRIPTION"))) {
    message("Selecciona DESCRIPTION de PhenoImage 4.8.0."); raiz <- dirname(file.choose())
  }
  if (!file.exists(file.path(raiz, "inst", "app", "R", "segmentacion_muestras.R"))) stop("Selecciona la carpeta PhenoImage 4.8.0 completa.")
  if ("phenoImage" %in% loadedNamespaces()) stop("Primero reinicia R: Session > Restart R.")
  archivo <- tempfile("phenoImage_4.8.0_", fileext = ".tar.gz")
  anterior <- getwd(); on.exit({setwd(anterior); unlink(archivo)}, add = TRUE)
  setwd(dirname(raiz))
  utils::tar(archivo, files = basename(raiz), compression = "gzip", tar = "internal")
  setwd(anterior)
  utils::install.packages(archivo, repos = NULL, type = "source")
  if (!requireNamespace("phenoImage", quietly = TRUE) || as.character(utils::packageVersion("phenoImage")) != "4.8.0") stop("No se confirmó la actualización; revisa el mensaje de instalación.")
  message("PhenoImage 4.8.0 instalado. Ejecuta phenoImage::run_app(). Los ejemplos existentes se conservan.")
})
