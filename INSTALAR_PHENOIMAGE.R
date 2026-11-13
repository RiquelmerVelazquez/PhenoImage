# Para usuarios nuevos: no requiere el paquete cipat anterior.
# Abre este archivo en RStudio, pulsa Source y elige DESCRIPTION de PhenoImage.
local({
  if (getRversion() < "4.1.0") stop("Se requiere R 4.1 o posterior.")
  if ("phenoImage" %in% loadedNamespaces()) stop("Reinicia R con Session > Restart R y vuelve a ejecutar este instalador.")
  message("Selecciona DESCRIPTION dentro de la carpeta PhenoImage descomprimida.")
  chosen <- file.choose()
  if (basename(chosen) != "DESCRIPTION") stop("Selecciona el archivo DESCRIPTION del proyecto PhenoImage.")
  package_dir <- normalizePath(dirname(chosen), winslash = "/", mustWork = TRUE)
  description <- read.dcf(chosen)
  if (description[1,"Package"] != "phenoImage" || description[1,"Version"] != "4.7.0.22") stop("Selecciona el paquete phenoImage 4.7.0.22.")
  # Conservar ejemplos anteriores si existen. Nunca son un requisito de instalación.
  cache <- file.path(tools::R_user_dir("PhenoImage", "data"), "ejemplos")
  for (previous in c("phenoImage", "cipat")) {
    origin <- system.file("app", "ejemplos", package = previous)
    if (!nzchar(origin)) next
    for (module in c("rgb", "sentera")) {
      source <- file.path(origin,module); target <- file.path(cache,module)
      if (dir.exists(source) && !file.exists(target)) {
        dir.create(cache,recursive=TRUE,showWarnings=FALSE)
        if (!file.copy(source,cache,recursive=TRUE)) warning("No se pudo conservar el ejemplo ",module,"; es opcional y puedes instalarlo después.")
      }
    }
  }
  utils::install.packages(package_dir, repos = NULL, type = "source")
  if (!requireNamespace("phenoImage",quietly=TRUE) || utils::packageVersion("phenoImage") != "4.7.0.22") stop("No se instaló la versión esperada. Revisa los mensajes de R.")
  phenoImage::install_dependencies(textures = TRUE)
  message("PhenoImage instalado. Para abrirlo: phenoImage::run_app(). Los ejemplos son opcionales.")
})
