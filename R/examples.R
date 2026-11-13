# Ejemplos opcionales en datos de usuario, fuera de la biblioteca del paquete.
.examples_root <- function() {
  configured <- getOption("phenoImage.examples_dir")
  if (is.null(configured)) configured <- file.path(tools::R_user_dir("PhenoImage", "data"), "ejemplos")
  if (length(configured) != 1L || is.na(configured) || !nzchar(configured)) stop("Carpeta de ejemplos inválida.", call. = FALSE)
  path.expand(configured)
}
.example_module <- function(module) {
  if (length(module) != 1L || is.na(module) || !module %in% c("rgb", "sentera", "phantom")) stop("Usa module = 'rgb' o 'sentera'.", call. = FALSE)
  if (module == "phantom") "rgb" else module
}
.example_files <- function(module) {
  module <- .example_module(module)
  base <- if (module == "rgb") "RGB_FTRBH25STG2CH001_2026-09-11" else "Sentera_FTRBH25STG2CH001_2026-09-11"
  images <- if (module == "rgb") c("Ortomosaico_RGB_BH16_2026-09-11.tif", "CHM_BH16_2026-09-11_cm.tif") else "Multibanda_Sentera_BH16_2026-09-11.tif"
  c(images, paste0(base, c(".shp", ".shx", ".dbf", ".prj", ".cpg")))
}
example_status <- function() {
  modules <- c("rgb", "sentera")
  paths <- file.path(.examples_root(), modules)
  ready <- vapply(seq_along(modules), function(i) all(file.exists(file.path(paths[i], .example_files(modules[i])))), logical(1))
  data.frame(Modulo = modules, Instalado = ready, Carpeta = paths, row.names = NULL)
}
example_path <- function(module) {
  module <- .example_module(module)
  path <- file.path(.examples_root(), module)
  if (!all(file.exists(file.path(path, .example_files(module))))) {
    stop("Este ejemplo es opcional y todavía no está instalado. Descarga su ZIP en Releases e instálalo con phenoImage::install_examples(). Puedes trabajar con 'Mis archivos' sin instalarlo.", call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
install_examples <- function(archive = NULL, overwrite = FALSE) {
  if (is.null(archive)) {
    if (!interactive()) stop("Indica la ruta del ZIP con archive = 'ruta/al/archivo.zip'.", call. = FALSE)
    message("Selecciona PhenoImage_Ejemplo_RGB.zip o PhenoImage_Ejemplo_Sentera.zip.")
    archive <- file.choose()
  }
  if (length(archive) != 1L || is.na(archive) || !file.exists(archive)) stop("No se encontró el archivo ZIP.", call. = FALSE)
  listing <- utils::unzip(archive, list = TRUE)
  names <- listing$Name
  if (!length(names) || anyDuplicated(names) || any(grepl('\\\\|^/|^[A-Za-z]:|(^|/)\\.\\.?(/|$)', names))) {
    stop("El ZIP tiene rutas inválidas o duplicadas.", call. = FALSE)
  }
  files <- names[!grepl('/$', names)]
  modules <- unique(sub('/.*$', '', files))
  if (length(modules) != 1L || !modules %in% c("rgb", "sentera")) stop("El ZIP debe contener un solo módulo: rgb/ o sentera/.", call. = FALSE)
  expected <- paste0(modules, '/', .example_files(modules))
  if (!setequal(files, expected)) stop("El ZIP no coincide con los archivos del ejemplo oficial de PhenoImage.", call. = FALSE)
  if (any(listing$Length[match(expected,names)] <= 0)) stop("El ZIP contiene archivos vacíos.", call. = FALSE)
  root <- .examples_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile("instalar-", tmpdir = root)
  dir.create(stage)
  target <- file.path(root, modules)
  backup <- NULL; completed <- FALSE
  on.exit({
    unlink(stage, recursive = TRUE)
    if (!completed && !is.null(backup) && dir.exists(backup)) file.rename(backup, target)
  }, add = TRUE)
  if (file.exists(target) && !isTRUE(overwrite)) stop("Ese ejemplo ya existe. Usa overwrite = TRUE si deseas reemplazarlo.", call. = FALSE)
  utils::unzip(archive, files = expected, exdir = stage)
  paths <- file.path(stage, expected)
  if (!all(file.exists(paths)) || any(file.info(paths)$size != listing$Length[match(expected,names)])) stop("No se pudo extraer el ejemplo completo.", call. = FALSE)
  if (file.exists(target)) {
    backup <- tempfile("anterior-", tmpdir = root)
    if (!file.rename(target, backup)) stop("No se pudo preparar el reemplazo del ejemplo anterior.", call. = FALSE)
  }
  if (!file.rename(file.path(stage, modules), target)) stop("No se pudo instalar el ejemplo. Revisa espacio y permisos.", call. = FALSE)
  completed <- TRUE
  if (!is.null(backup)) unlink(backup, recursive = TRUE)
  message("Ejemplo ", modules, " instalado. Puedes elegir 'Usar ejemplo' en PhenoImage.")
  invisible(example_status())
}
