# Catálogo curado. Las variantes originales de la aplicación tienen prioridad.
catalogo_indices_cipat <- function(tipo) {
  d <- data.frame(
    indice = c("NDVI", "NDRE", "GNDVI", "CIRE", "CVI", "NDWI", "OSAVI", "SAVI", "EVI", "ExG", "NGRDI", "VARI", "GLI", "RGBVI", "CIG", "RESR", "IPVI", "GCC", "NGBDI", "TGI"),
    nombre = c("Diferencia normalizada de vegetación", "Diferencia normalizada de borde rojo", "Diferencia normalizada con verde", "Relación de clorofila con borde rojo", "Índice de vegetación de clorofila", "Diferencia normalizada verde–NIR", "Vegetación ajustada al suelo", "Vegetación ajustada al suelo · L = 0.5", "Índice de vegetación mejorado", "Exceso de verde", "Diferencia normalizada verde–rojo", "Índice de vegetación visible", "Índice de hoja verde", "Índice de vegetación RGB", "Relación NIR–verde menos uno", "Relación simple de borde rojo", "Proporción de infrarrojo", "Fracción cromática verde", "Diferencia normalizada verde–azul", "Verdor triangular"),
    grupo = c("Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "RGB", "RGB", "RGB", "RGB", "RGB", "Multiespectral", "Multiespectral", "Multiespectral", "RGB", "RGB", "RGB"),
    bandas = c("NIR · R", "NIR · RE", "NIR · G", "NIR · RE", "NIR · R · G", "G · NIR", "NIR · R", "NIR · R", "NIR · R · B", "R · G · B", "G · R", "R · G · B", "R · G · B", "R · G · B", "NIR · G", "NIR · RE", "NIR · R", "R · G · B", "G · B", "R · G · B"),
    descripcion = c("Contraste entre infrarrojo cercano y rojo. Es el índice utilizado para la máscara Sentera.", "Contraste entre infrarrojo cercano y borde rojo. Índice NDRE de borde rojo.", "Compara el infrarrojo cercano con el canal verde.", "Relación NIR/RE desplazada en una unidad. Conserva el recorte histórico de la aplicación.", "Combina infrarrojo, rojo y verde. Conserva los filtros históricos de la aplicación.", "Variante verde–NIR de NDWI. No corresponde a la variante NIR–SWIR.", "Se conserva la variante con multiplicador 1.16 de la aplicación.", "Se conserva el ajuste al suelo con L = 0.5.", "Combina infrarrojo cercano, rojo y azul con los coeficientes originales.", "Destaca el canal verde frente a rojo y azul. Es el índice utilizado para la máscara RGB.", "Contraste normalizado entre los canales verde y rojo.", "Relaciona verde y rojo incorporando el canal azul en el denominador.", "Compara el doble del canal verde con la suma de rojo y azul.", "Contrasta el cuadrado del verde con el producto rojo por azul.", "Relación simple con verde, desplazada en una unidad.", "Cociente directo entre infrarrojo cercano y borde rojo.", "Fracción NIR respecto a la suma NIR y rojo.", "Proporción del verde dentro de la intensidad total RGB.", "Contraste normalizado entre verde y azul.", "Combinación lineal de los tres canales visibles."),
    referencia = c("Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Original de la aplicación", "Catálogo pliman · Gitelson et al. (2003)", "Catálogo pliman · Gitelson et al. (2002)", "Catálogo pliman · Crippen (1990)", "Catálogo pliman · Richardson et al. (2007)", "Catálogo pliman · Bannari et al. (1995)", "Catálogo pliman · Hunt et al. (2013)"),
    formula = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "NIR / G - 1", "NIR / RE", "NIR / (NIR + R)", "G / (R + G + B)", "(G - B) / (G + B)", "G - 0.39 * R - 0.61 * B"),
    stringsAsFactors = FALSE)
  originales <- c(indices_originales_cipat("sentera"), indices_originales_cipat("phantom"))
  for (i in seq_len(nrow(d))) if (!nzchar(d$formula[i])) d$formula[i] <- originales[[d$indice[i]]]
  extra <- data.frame(
    indice = c("BCC", "RCC", "BGI", "BRVI", "GR", "GB", "ERVI", "MVARI", "VEG", "MSR", "WDRVI", "BWDRVI", "GRVI", "VIN", "VIRE", "MSAVI2", "GOSAVI", "GSAVI", "PSRI", "ARI", "RDVI", "PNDVI"),
    nombre = c("Fracción cromática azul", "Fracción cromática roja", "Relación azul–verde", "Diferencia normalizada azul–rojo", "Relación verde–rojo", "Relación verde–azul", "Exceso de rojo", "Índice visible modificado", "Índice vegetativo RGB", "Relación simple modificada", "Vegetación de rango dinámico amplio", "Rango dinámico amplio con azul", "Relación NIR–verde", "Relación NIR–rojo", "Diferencia normalizada borde rojo–rojo", "Vegetación con ajuste de suelo modificado", "Ajuste de suelo optimizado con verde", "Ajuste de suelo con verde", "Índice de senescencia vegetal", "Índice de reflectancia de antocianinas", "Diferencia de vegetación renormalizada", "Diferencia normalizada NIR–visible"),
    grupo = c("RGB", "RGB", "RGB", "RGB", "RGB", "RGB", "RGB", "RGB", "RGB", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral", "Multiespectral"),
    bandas = c("R · G · B", "R · G · B", "B · G", "B · R", "G · R", "G · B", "R · G", "R · G · B", "R · G · B", "NIR · R", "NIR · R", "NIR · B", "NIR · G", "NIR · R", "RE · R", "NIR · R", "NIR · G", "NIR · G", "R · G · RE", "G · RE", "NIR · R", "NIR · R · G · B"),
    formula = c("B / (R + G + B)", "R / (R + G + B)", "B / G", "(B - R) / (B + R)", "G / R", "G / B", "1.4 * R - G", "(G - B) / (G + R - B)", "G / (R^0.667 * B^0.334)", "(NIR / R - 1) / (sqrt(NIR / R) + 1)", "(0.2 * NIR - R) / (0.2 * NIR + R)", "(0.1 * NIR - B) / (0.1 * NIR + B)", "NIR / G", "NIR / R", "(RE - R) / (RE + R)", "(2 * NIR + 1 - sqrt((2 * NIR + 1)^2 - 8 * (NIR - R))) / 2", "(NIR - G) / (NIR + G + 0.16)", "1.5 * (NIR - G) / (NIR + G + 0.5)", "(R - G) / RE", "1 / G - 1 / RE", "(NIR - R) / sqrt(NIR + R)", "(NIR - (G + R + B)) / (NIR + (G + R + B))"),
    referencia = c("De Swaef et al. (2021)", "De Swaef et al. (2021)", "Zarco-Tejada et al. (2005)", "De Swaef et al. (2021)", "Catálogo pliman", "Catálogo pliman", "Meyer y Neto (2008)", "Yang et al. (2008)", "Hague et al. (2006)", "Chen (1996)", "Gitelson (2004)", "Gitelson (2004)", "Sripada et al. (2006)", "Pearson y Miller (1972)", "Gitelson et al. (2002)", "Qi et al. (1994)", "Index Database; pliman", "Index Database; pliman", "Merzlyak et al. (1999)", "Gitelson et al. (2001)", "Roujean y Breon (1995)", "Index Database; pliman"), stringsAsFactors = FALSE)
  extra$descripcion <- paste0(extra$nombre, ". Disponible como opción; su pertinencia debe evaluarse con las repeticiones del ensayo.")
  extra <- extra[, names(d), drop = FALSE]
  d <- rbind(d, extra[!extra$indice %in% d$indice, , drop = FALSE])
  # RGB es compatible con ambos módulos; RE/NIR solo con Sentera.
  if (tipo == "phantom") d <- d[d$grupo == "RGB", , drop = FALSE]
  d$nota <- ifelse(d$indice %in% c("OSAVI", "SAVI", "EVI", "MSAVI2", "GOSAVI", "GSAVI", "ARI", "RDVI"),
    "Verifica la escala radiométrica: las constantes aditivas requieren una escala compatible con la fórmula.",
    ifelse(d$grupo == "Multiespectral", "En Sentera, RE y NIR ya incluyen la corrección y el factor inter-cámara.",
      "En Sentera se usan canales RGB corregidos; en RGB convencional, canales originales."))
  escalas <- d$indice %in% c("OSAVI", "SAVI", "EVI", "MSAVI2", "GOSAVI", "GSAVI", "ARI", "RDVI")
  d$nota[escalas] <- "Requiere reflectancia o escala validada. No aplicar directamente a valores Camera Only de cientos/miles ni reescalar por un factor arbitrario."
  d$nota[d$indice %in% c("ExG", "ERVI", "TGI")] <- "Depende de la escala de intensidad: mantener escala, iluminación y procesamiento consistentes."
  d$nota[d$indice == "NDWI"] <- "Variante verde–NIR: es el negativo de GNDVI; no usa SWIR ni mide humedad directamente."
  d$nota[d$indice == "GRVI"] <- "Nombre GRVI según catálogo pliman: NIR/G. No confundir con GRVI2/NGRDI, que usan verde y rojo."
  d$referencia <- paste0(d$referencia, " | https://nepem-ufsc.github.io/pliman/articles/indexes.html")
  d
}

# El filtro de grupo pertenece solo a Sentera. RGB ignora cualquier estado residual.
filtrar_catalogo_cipat <- function(tipo, grupo = NULL, consulta = NULL) {
  d <- catalogo_indices_cipat(tipo)
  if (identical(tipo, "sentera") && length(grupo) == 1L &&
      !is.na(grupo) && grupo %in% c("Multiespectral", "RGB")) {
    d <- d[d$grupo == grupo, , drop = FALSE]
  }
  consulta <- if (length(consulta) == 1L && !is.na(consulta)) trimws(consulta) else ""
  if (nzchar(consulta) && nrow(d)) {
    normalizar <- function(x) tolower(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
    hay <- apply(d, 1, paste, collapse = " ")
    d <- d[grepl(normalizar(consulta), normalizar(hay), fixed = TRUE), , drop = FALSE]
  }
  d
}
