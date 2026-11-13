# Selección RGB: ExG siempre está disponible para construir la máscara.
# Su casilla controla únicamente su inclusión en las estadísticas del análisis.
preparar_indices_rgb_cipat <- function(p) {
  if (is.null(p$formulas) || !"ExG" %in% names(p$formulas)) {
    stop("Conserva ExG en las fórmulas RGB porque genera la máscara.")
  }
  # Las llamadas antiguas sin este parámetro conservan todos sus índices.
  # Una selección vacía explícita se respeta, incluso si Shiny entrega NULL.
  if (!"indices" %in% names(p)) p$indices <- names(p$formulas)
  seleccion <- unique(as.character(p$indices))
  if (any(!seleccion %in% names(p$formulas))) stop("Revisa los índices RGB seleccionados en el editor.")
  p$indices <- seleccion
  p$formulas <- p$formulas[unique(c("ExG", seleccion))]
  p
}
