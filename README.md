# PhenoImage 4.8.0

Plataforma local de caracterización fenotípica mediante imágenes UAV.
**Esta versión unifica las mejoras de E24 a E28. No requiere instalar las
ediciones anteriores.**

## Qué incluye

| Área | Funcionalidad integrada |
| --- | --- |
| Análisis | Completo o personalizado; entradas según los productos solicitados |
| RGB | Bandas, índices, cobertura, altura, color y texturas seleccionables |
| Sentera | Bandas, índices y cobertura; multibanda corregida o flujo de entrada existente |
| Altura | CHM sin exigir RGB; filtro independiente; salida en cm |
| Segmentación | Otsu, umbral manual o muestras; muestras con verde/suelo o verde/seco/suelo |
| Coberturas | Green, Dry, Bare_soil y Total; seco no evaluado como NA, no como cero |
| Estadísticas | Bandas, índices, color y texturas filtrados por la clase verde |
| Resultados | Resumen, mapas, productos ráster, tabla HTP, calidad y archivos |
| Exploración | Comparación, distribución, relación X–Y y resumen |
| Vistas | Básica, Personalizada y Avanzada; altura media y P90 en Básica |
| Testigos | Selección y color; listas sin reconstrucción innecesaria y fichas estables |
| Archivos | Excel, CSV, RDS y TIFF; nombres técnicos conservados |

## Actualizar desde una edición anterior

1. Detén la aplicación y reinicia R en RStudio: **Session → Restart R**.
2. Descomprime el paquete completo en una carpeta nueva.
3. Abre `INSTALAR_PHENOIMAGE.R` y pulsa **Source**.
4. Ejecuta:

```r
phenoImage::install_dependencies()
phenoImage::run_app()
```

La cabecera debe mostrar **4.8.0**. Los ejemplos instalados se conservan fuera de
la biblioteca del paquete. No necesitas volver a instalarlos ni recalcular tus
resultados para cambiar las vistas de Exploración.

También puedes abrir directamente `INICIAR_PHENOIMAGE.R` con Source.

## Instalar el paquete fuente

Si recibiste `phenoImage_4.8.0.tar.gz`, selecciónalo con:

```r
install.packages(file.choose(), repos = NULL, type = "source")
phenoImage::install_dependencies()
phenoImage::run_app()
```

Para instalar desde el ZIP de código de GitHub, descomprime todo y utiliza
`INSTALAR_PHENOIMAGE.R`; el ZIP del repositorio no se instala directamente como
un paquete fuente de R.

## Ejemplos opcionales

Los ortomosaicos, CHM y polígonos de ejemplo se distribuyen por separado por su
tamaño. Se pueden conservar los ejemplos ya instalados, trabajar con archivos
propios o instalar los ZIP oficiales con `phenoImage::install_examples()`.

## Documentación

- `LEEME.txt`: procedimiento y definiciones de las coberturas.
- `NEWS.md`: cambios integrados en esta versión.
- `VERIFICACION.txt`: comprobaciones realizadas y alcance.

Repositorio: https://github.com/RiquelmerVelazquez/PhenoImage
