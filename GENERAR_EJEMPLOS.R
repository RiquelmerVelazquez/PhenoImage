# Solo para el responsable: crear archivos para adjuntar a una Release privada.
# Los datos NO se suben automáticamente a GitHub.
local({
  if (!requireNamespace("zip",quietly=TRUE)) stop("Instala zip: install.packages('zip').")
  message("Selecciona un archivo dentro de la carpeta de DESTINO de los ZIP. Puede ser un TXT vacío creado para este paso.")
  destination <- dirname(file.choose())
  candidates <- c(file.path(tools::R_user_dir("PhenoImage","data"),"ejemplos"),
    system.file("app","ejemplos",package="phenoImage"),system.file("app","ejemplos",package="cipat"))
  candidates <- candidates[nzchar(candidates)&dir.exists(candidates)]
  for(module in c("rgb","sentera")) {
    base <- if(module=="rgb")"RGB_FTRBH25STG2CH001_2026-09-11"else"Sentera_FTRBH25STG2CH001_2026-09-11"
    images <- if(module=="rgb")c("Ortomosaico_RGB_BH16_2026-09-11.tif","CHM_BH16_2026-09-11_cm.tif")else"Multibanda_Sentera_BH16_2026-09-11.tif"
    required <- c(images,paste0(base,c('.shp','.shx','.dbf','.prj','.cpg')))
    available <- candidates[vapply(candidates,function(p)all(file.exists(file.path(p,module,required))),logical(1))]
    if(!length(available)) {warning("No se encontraron datos completos del ejemplo ",module);next}
    output <- file.path(destination,if(module=='rgb')'PhenoImage_Ejemplo_RGB.zip'else'PhenoImage_Ejemplo_Sentera.zip')
    if(file.exists(output))stop("Ya existe ",output,". Mueve ese archivo o elige otra carpeta para no sobrescribirlo.")
    zip::zipr(output,files=paste0(module,'/',required),root=available[1],include_directories=FALSE,mode="mirror")
    message("Creado: ",output)
  }
})
