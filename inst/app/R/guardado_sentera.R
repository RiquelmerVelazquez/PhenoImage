asegurar_temporal_excel_sentera <- function() {
  carpeta <- tryCatch(tempdir(check = TRUE), error = function(e) "")
  if (!nzchar(carpeta)) stop("R no pudo crear su carpeta temporal. Revisa espacio libre y permisos en la unidad temporal de Windows.")
  if (!dir.exists(carpeta)) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  prueba <- tempfile("comprobar_excel_", tmpdir = carpeta)
  ok <- tryCatch({ writeBin(as.raw(1), prueba); TRUE }, error = function(e) FALSE)
  if (file.exists(prueba)) unlink(prueba)
  if (!ok) stop("No se puede escribir en la carpeta temporal de R: ", carpeta,
    ". Revisa espacio libre y permisos; reinicia R si la carpeta dejó de estar disponible.")
  invisible(carpeta)
}

guardar_resultados_sentera <- function(tabla, metadatos, p, umbral) {
  # Respaldar antes de construir el Excel. No depende del temporal de openxlsx.
  respaldo <- archivo_salida_cipat(p, "HTP_Respaldo", "rds")
  saveRDS(list(tabla = tabla, metadatos = metadatos, parametros = p, umbral = umbral), respaldo)
  utils::write.csv(tabla, archivo_salida_cipat(p, "HTP", "csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(metadatos, archivo_salida_cipat(p, "Parametros", "csv"), row.names = FALSE, fileEncoding = "UTF-8")
  exportar_excel_sentera(readRDS(respaldo), archivo_salida_cipat(p, "HTP", "xlsx"))
}

exportar_excel_sentera <- function(respaldo, archivo) {
  tryCatch({
    asegurar_temporal_excel_sentera()
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "HTP"); openxlsx::writeData(wb, "HTP", respaldo$tabla)
    openxlsx::freezePane(wb, "HTP", firstRow = TRUE, firstCol = TRUE)
    openxlsx::addWorksheet(wb, "Parametros"); openxlsx::writeData(wb, "Parametros", respaldo$metadatos)
    registrar_formulas_cipat(wb, respaldo$parametros)
    ok <- openxlsx::saveWorkbook(wb, archivo, overwrite = FALSE, returnValue = TRUE)
    if (!isTRUE(ok) || !file.exists(archivo) || is.na(file.info(archivo)$size) || file.info(archivo)$size <= 0) stop("No se pudo escribir el archivo Excel de salida.")
    archivo
  }, error = function(e) stop("Las estadísticas están respaldadas en HTP_Respaldo.rds y HTP_Sentera.csv. Falló únicamente el Excel: ",
    conditionMessage(e), ". Revisa el espacio disponible, los permisos de escritura y la carpeta de resultados.", call. = FALSE))
}
