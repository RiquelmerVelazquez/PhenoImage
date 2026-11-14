# PhenoImage 4.8.0 — 14 de noviembre de 2026

Entrega unificada basada en E28, con los cambios acumulados de E24–E28.

## Análisis seleccionables (E24)

- Elegir análisis completo o personalizado antes de cargar archivos.
- Pedir fuentes según los grupos elegidos; altura sin RGB.
- Aplicar la máscara elegida a las estadísticas solicitadas.
- Mantener la elección Por muestras al cargar ejemplos.
- Adaptar resultados a los análisis solicitados, sin columnas duplicadas.

## Coberturas por clases (E25)

- Dos clases: verde y suelo. Tres clases: verde, seco y suelo.
- La clase seca es opcional. Con dos clases queda no evaluada (NA).
- Columnas `Plant_ground_cover_image_Green_pct`, `_Dry_pct`, `_Bare_soil_pct`
  y `_Total_pct`; mismo denominador válido y NoData excluido.
- Total = verde + seco en tres clases; total = verde en dos clases.
- Otsu y umbral manual mantienen cobertura verde; su fondo no equivale a suelo.
- Bandas, índices, color y texturas se filtran por verde. CHM independiente.
- CSV de muestras conserva el modo de clases; lectura de CSV históricos.

## Resultados y Exploración (E26)

- Comparación solo en Exploración; retirada de la pestaña duplicada.
- Variable X/Y y nivel de puntos visibles solo en Relación X–Y.
- Panel de controles con fondo verde suave y selecciones conservadas.

## Gráficas y testigos (E27)

- Eje de agrupación identificado; marcas inferiores del histograma explicadas.
- Opciones de testigos actualizadas solo cuando cambian campo o datos.
- Ficha del punto estable con margen de salida; menos revisiones del diseño.

## Vistas de variables (E28)

- Básica predeterminada: medias, altura media y P90 y porcentajes evaluados.
- Personalizada: elegir variables y estadísticos visibles.
- Avanzada: todas las métricas numéricas disponibles.
- Cambiar de vista conserva selecciones válidas y no modifica el Excel HTP.

## Distribución 4.8.0

- Una única versión del código y un paquete fuente instalable de R.
- Instaladores, cabecera y metadatos actualizados a 4.8.0.
- Documentación unificada y pruebas de instalación y funciones principales.
- Los datos de ejemplo continúan separados del código.
