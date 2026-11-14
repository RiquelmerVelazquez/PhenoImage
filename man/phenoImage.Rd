\name{run_app}
\alias{run_app}
\alias{check_installation}
\alias{install_dependencies}
\title{Abrir PhenoImage incluida en el paquete}
\description{Ejecuta la aplicación completa, con sus módulos, recursos y ejemplos opcionales. No requiere configurar una carpeta externa de la aplicación.}
\usage{
run_app(launch.browser = TRUE, port = NULL)
check_installation()
install_dependencies(textures = TRUE, repos = "https://cloud.r-project.org")
}
\arguments{
\item{launch.browser}{Abrir el navegador automáticamente.}
\item{port}{Puerto local opcional. NULL permite que Shiny elija uno.}
\item{textures}{Instalar también raster y glcm para las texturas.}
\item{repos}{Repositorio de paquetes R.}
}
\value{run_app devuelve el resultado de shiny::runApp al detenerla; check_installation devuelve una tabla de dependencias; install_dependencies devuelve esa tabla invisiblemente.}
\details{Los cálculos se realizan en el equipo local. Los resultados se guardan en la carpeta que selecciona el usuario. Las imágenes de ejemplo se instalan aparte en la carpeta de datos del usuario. Incluye análisis seleccionables, máscaras por muestras de dos o tres clases y vistas Básica, Personalizada y Avanzada de Exploración.}
