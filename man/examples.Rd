\name{install_examples}
\alias{install_examples}
\alias{example_path}
\alias{example_status}
\title{Instalar y consultar los ejemplos opcionales de PhenoImage}
\description{Instala un archivo ZIP oficial por módulo en la carpeta de datos del usuario, sin necesitar CIPAT ni modificar la biblioteca del paquete.}
\usage{
install_examples(archive = NULL, overwrite = FALSE)
example_status()
example_path(module)
}
\arguments{
\item{archive}{Ruta de PhenoImage_Ejemplo_RGB.zip o PhenoImage_Ejemplo_Sentera.zip. NULL abre un selector en una sesión interactiva.}
\item{overwrite}{Reemplazar un ejemplo ya instalado. FALSE evita sobrescribirlo.}
\item{module}{rgb o sentera; phantom se acepta como alias de rgb.}
}
\value{install_examples devuelve el estado invisiblemente; example_status devuelve una tabla; example_path devuelve la ruta del módulo instalado o un error si falta.}
\details{La opción phenoImage.examples_dir permite elegir otra carpeta de almacenamiento. No se descargan archivos automáticamente. Los archivos de ejemplo deben obtenerse de una fuente autorizada. La extracción valida los nombres, tamaños, rutas y componentes requeridos.}
