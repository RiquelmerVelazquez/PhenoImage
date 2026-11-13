# La aplicación y todos sus recursos residen dentro del propio paquete.
.app_path <- function() {
  path <- system.file("app", package = "phenoImage")
  if (!nzchar(path) || !file.exists(file.path(path, "app.R"))) {
    stop("El paquete PhenoImage está incompleto. Vuelve a instalarlo desde el paquete completo.", call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
.app_distribution <- function(path) {
  e <- new.env(parent = baseenv())
  sys.source(file.path(path, "R", "distribucion.R"), envir = e)
  e$cipat_verificar_archivos(path)
  e
}
check_installation <- function() {
  e <- .app_distribution(.app_path())
  # glcm y raster son necesarios cuando se activa el cálculo de texturas.
  packages <- unique(c(e$cipat_paquetes, "raster", "glcm"))
  minimum <- e$cipat_minimos
  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  versions <- vapply(seq_along(packages), function(i) {
    if (installed[i]) as.character(utils::packageVersion(packages[i])) else NA_character_
  }, character(1))
  needed <- vapply(packages, function(p) if (p %in% names(minimum)) minimum[[p]] else NA_character_, character(1))
  good <- installed
  for (i in seq_along(packages)) {
    if (installed[i] && !is.na(needed[i])) good[i] <- utils::compareVersion(versions[i], needed[i]) >= 0
  }
  data.frame(Paquete = packages, Version = versions, Minimo = needed,
    Correcto = unname(good), Opcional_texturas = packages %in% c("raster", "glcm"), row.names = NULL)
}
install_dependencies <- function(textures = TRUE, repos = "https://cloud.r-project.org") {
  status <- check_installation()
  selected <- !status$Correcto & (isTRUE(textures) | !status$Opcional_texturas)
  if (any(selected)) utils::install.packages(status$Paquete[selected], repos = repos)
  status <- check_installation()
  bad <- !status$Correcto & (isTRUE(textures) | !status$Opcional_texturas)
  if (any(bad)) stop("No se completó la instalación de: ", paste(status$Paquete[bad], collapse = ", "), ". Revisa los mensajes de R.", call. = FALSE)
  message("Dependencias comprobadas. Abre PhenoImage con phenoImage::run_app().")
  invisible(status)
}
.launch <- function(path, launch.browser, port, runner) {
  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  setwd(path)
  runner(appDir = path, launch.browser = launch.browser, host = "127.0.0.1", port = port)
}
run_app <- function(launch.browser = TRUE, port = NULL) {
  status <- check_installation()
  bad <- !status$Correcto & !status$Opcional_texturas
  if (any(bad)) stop("Faltan dependencias: ", paste(status$Paquete[bad], collapse = ", "), ". Ejecuta phenoImage::install_dependencies().", call. = FALSE)
  .launch(.app_path(), launch.browser, port, shiny::runApp)
}
