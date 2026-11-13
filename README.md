# PhenoImage

### Fenotipado de forrajes mediante imágenes UAV

**CIPAT · Centro de Investigación de Pastos Tropicales · Grupo Papalotla**

![CIPAT](inst/app/www/cipat.png)

PhenoImage es una aplicación local de R/Shiny para organizar el análisis de imágenes de ensayos de forrajes. Integra módulos para Sentera Double 4K y RGB convencional, con extracción de resultados por parcela y exportación de productos.

**Versión 4.7.0.21 · E21 · Distribución privada de evaluación.**

## Capacidades

| Área | Productos y funciones |
|---|---|
| Multiespectral Sentera | Bandas, índices configurables y cobertura NDVI |
| RGB | Bandas, índices configurables y cobertura RGB |
| Estructura | Altura del dosel a partir de CHM o DSM/DTM |
| Color | HSB, DGCI y CIE-Lab |
| Texturas | Entropía, contraste y homogeneidad GLCM |
| Resultados | Tablas individuales e integrada, GeoTIFF y visores interactivos |

Se conservan la interfaz, logos, estilos y módulos de E20. E21 cambia la distribución de los ejemplos y la instalación; no cambia las fórmulas ni las funciones principales de procesamiento. El uso científico requiere validar las entradas, la calibración y los resultados para cada evaluación. El empaquetado no constituye esa validación.

## Requisitos

- R 4.1 o posterior; usa una versión compatible con los paquetes requeridos.
- RStudio para seguir el procedimiento sencillo de instalación.
- Internet durante la instalación de dependencias.
- Espacio en disco y memoria suficientes para los ortomosaicos y sus productos.

El instalador comprueba las dependencias, incluidas `terra`, `sf`, `exactextractr`, `pliman`, `raster` y `glcm`. Si una dependencia no dispone de binario compatible, sigue sus instrucciones de compilación; puede requerir Rtools en Windows o bibliotecas del sistema en Linux.

## Instalación para un usuario nuevo

**No necesitas instalar CIPAT antes.**

1. Solicita acceso al repositorio privado y acepta la invitación.
2. En GitHub selecciona **Code → Download ZIP** y descomprime todo el archivo.
3. En RStudio entra a **Session → Restart R** si ya tienes PhenoImage abierto.
4. Abre `INSTALAR_PHENOIMAGE.R` y presiona **Source**.
5. En el selector elige `DESCRIPTION` de la carpeta descomprimida de PhenoImage.
6. Espera a que termine la instalación de la aplicación y las dependencias.

El instalador conserva los ejemplos de instalaciones anteriores si los encuentra, pero no los exige. No copia ni sube archivos del ensayo a GitHub.

## Abrir la aplicación

```r
phenoImage::run_app()
```

La interfaz abre el navegador y los cálculos se ejecutan en tu computadora. Mantén R abierto y usa **Stop** para detener la aplicación. Los resultados se guardan en la carpeta que selecciones dentro de PhenoImage.

También puedes utilizar:

```r
library(phenoImage)
run_app()
```

Si usas otras aplicaciones con una función llamada `run_app`, prefiere `phenoImage::run_app()` para evitar confusiones.

## Instalación alternativa desde una versión publicada

Si el responsable adjuntó `phenoImage_4.7.0.21.tar.gz` a una **Release** privada, descarga ese archivo. En R:

```r
install.packages(file.choose(), repos = NULL, type = "source")
phenoImage::install_dependencies()
phenoImage::run_app()
```

Selecciona el archivo `.tar.gz` cuando aparezca el selector. La primera línea instala el paquete; la segunda instala sus dependencias necesarias para la aplicación.

## Ejemplos opcionales

Puedes analizar tus propios datos sin descargar ejemplos. El repositorio no incluye ortomosaicos de demostración en su historial de Git.

Si están publicados por el responsable en **Releases**, descarga el ejemplo que quieras:

- `PhenoImage_Ejemplo_RGB.zip`: RGB, CHM y polígonos.
- `PhenoImage_Ejemplo_Sentera.zip`: multibanda Sentera y polígonos.

Con la aplicación detenida, ejecuta:

```r
phenoImage::install_examples()
```

Selecciona uno de esos ZIP sin descomprimirlo. Repite para el otro módulo si lo deseas y abre nuevamente PhenoImage. Los ejemplos quedan en una carpeta de datos de tu usuario y sobreviven a la reinstalación del paquete.

```r
phenoImage::example_status()
```

No se descargan ni sustituyen datos automáticamente. Para reemplazar expresamente un ejemplo instalado utiliza `phenoImage::install_examples(overwrite = TRUE)`.

**Nota de publicación:** mientras el responsable no adjunte los ejemplos a una Release, esos ZIP no estarán disponibles en GitHub. La aplicación seguirá funcionando con archivos propios.

## Entradas

| Módulo | Entradas principales |
|---|---|
| Sentera | Multibanda con asignación correcta de bandas y polígonos identificados |
| RGB | Ortomosaico RGB, polígonos identificados y CHM o DSM/DTM cuando se analiza altura |

La calidad de los resultados depende de la alineación espacial, las unidades, el orden de bandas, la escala de datos y la calibración. Las correcciones y fórmulas deben elegirse según el origen de las imágenes. Consulta los controles de información de la aplicación.

## Actualizar

Descarga la nueva versión y repite la instalación con PhenoImage detenido y la sesión R reiniciada. Los ejemplos opcionales y las carpetas de resultados permanecen fuera de la instalación del paquete.

## Método y referencias

- HSB y Lab: llamadas directas a `pliman::rgb_to_hsb()` y `pliman::rgb_to_lab()`.
- DGCI: fórmula documentada por pliman, evaluada con HSB.
- Rásteres y extracción zonal: `terra`, `exactextractr` y código de PhenoImage.
- Texturas: `glcm`, independiente de las conversiones de color de pliman.

[pliman](https://nepem-ufsc.github.io/pliman/) · [Shiny](https://shiny.posit.co/)

## Autoría y distribución

Proyecto de CIPAT · Grupo Papalotla, preparado para revisión privada. El contacto del mantenedor sigue pendiente de completar antes de una distribución formal. La dirección `contacto-pendiente@example.invalid` del DESCRIPTION es un marcador técnico y no recibe correo. La licencia y los permisos de distribución deben definirse por el responsable; este repositorio no se declara automáticamente de código abierto.

## Ayuda

Para informar un problema usa **Issues** del repositorio privado e indica la versión, el módulo, el mensaje de error y los pasos para reproducirlo. Evita adjuntar datos del ensayo que no estén autorizados para compartir.

`VERIFICACION.txt` describe las comprobaciones técnicas realizadas y sus límites. Aún no se ha creado ni publicado este repositorio: esta es la documentación lista para cargar cuando exista la cuenta.
