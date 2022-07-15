
comprueba <- function(file_name = NULL, doc_format = NULL) {
  proj_dir    <- getwd()
  proj_files  <- list.files(proj_dir)
  if (!any(grepl(".Rproj", proj_files)))
    stop("\nEl directorio de trabajo no contiene ning\u00fan proyecto de RStudio.",
         "\nCambia al directorio principal creado con",
         " la funci\u00f3n fisabio::nuevo_proyecto()")
  if (is.null(file_name))
    file_name <- "informe"
  if (!grepl("/$", proj_dir))
    proj_dir  <- paste0(proj_dir, "/")
  proj_opt    <- readLines(proj_files[grep(".Rproj", proj_files)])
  doc_format  <- tolower(doc_format)
  if (!any(grepl(doc_format, c("pdf", "docx", "odt", "beamer"))))
    stop("\nEl formato que has escogido no est\u00e1 disponible o es err\u00f3neo.",
         "\nLos posibles formatos son: pdf, docx, odt o beamer.")
  if (doc_format %in% c("pdf", "beamer")) {
    if (!any(grepl("knitr", proj_opt, ignore.case = TRUE))) {
      stop("\nknitr no es la opci\u00f3n por defecto para compilar archivos LaTeX",
           "\nVuelve a ejecutar la funci\u00f3n cuando lo hayas cambiado en:\n",
           "Tools/Project Options/Sweave/Program Defaults")
    }
  }
}



#' Función que devuelve el formato de salida apropiado para generar el informe o
#' la presentación en PDF.
#'
#' Esta función crea una lista con todas las especificaciones necesarias para
#' crear un PDF siguiendo el formato de la plantilla de `fisabio`. Por defecto se
#' usa XeLaTeX como motor de LaTeX (mejor trabajo con fuentes).
#'
#' @param motor Motor de LaTeX. XeLaTeX por defecto.
#' @param ... Otros argumentos a pasar a [rmarkdown::pdf_document].
#' @export
#'
#' @details La función solo debe utilizarse desde una llamada a
#'   rmarkdown::render() o dentro de un documento .Rmd
#'
#' Aunque la opción por defecto usa [tinytex::install_tinytex] para obtener una sesión
#' de LaTeX operativa, siempre se puede optar por usar una versión del SO, en cuyo caso
#' es necesario tenerla instalada.
#'
#' @return Objeto con clase "rmarkdown_output_format".
#'
informe_pdf <- function(motor = c("xelatex", "pdftex"), ...) {
  este_sitio <- here::here()
  motor      <- tolower(motor)
  motor      <- match.arg(motor)
  template   <- system.file(
    "rmarkdown/templates/pdf/resources/template.tex",
    package = "fisabio"
  )
  doc_format <- rmarkdown::pdf_document(
    template         = template,
    latex_engine     = motor,
    md_extensions    = "-autolink_bare_uris",
    ...
  )
  doc_format$inherits <- "pdf_document"

  return(doc_format)
}


#' Crea un informe estadístico en ODT -- Markdown
#'
#' Crea automáticamente un informe estadístico en ODT -- Markdown empleando una
#' plantilla base con detalles del grupo.
#'
#' @export
#' @param template Ruta hasta la plantilla del documento. Por defecto se usa la plantilla
#'   de FISABIO (opción: `default`).
#' @param template_xml Ruta hasta la plantilla XML de Pandoc. Por defecto se usa la
#'   plantilla de FISABIO (opción: `default`).
#' @param ... Otras opciones a usar en [rmarkdown::odt_document].
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{nuevo_proyecto}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   La bibliografía se gestiona a través de `pandoc-citeproc` y un archivo `csl`
#'   con el estilo (por defecto, estilo APA). Si quieres cambiar el estilo las
#'   referencias, elimina el archivo `csl` que se genera por defecto y añade en su
#'   lugar el que tu desees, cambiando la opción en el YAML del documento .Rmd.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{nuevo_proyecto}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo.
#'
informe_odt <- function(template = "default", template_xml = "default", ...) {

  if (template == "default") {
    template <- system.file(
      "rmarkdown/templates/odt/resources/template_fisabio.odt",
      package = "fisabio"
    )
  } else {
    if (!file.exists(template)) {
      stop("No se encuentra la plantilla ODT.")
    } else {
      if (!grepl("odt$", template)) {
        stop("El archivo proporcionado no parece una plantilla ODT.")
      }
    }
  }
  if (template_xml == "default") {
    template_xml <- system.file(
      "rmarkdown/templates/odt/resources/template_fisabio_odt.xml",
      package = "fisabio"
    )
  } else {
    if (!file.exists(template_xml)) {
      stop("No se encuentra la plantilla ODT.")
    } else {
      if (!grepl("xml$", template_xml)) {
        stop("El archivo proporcionado no parece una plantilla XML de ODT.")
      }
    }
  }

  doc_format <- rmarkdown::odt_document(
    reference_odt    = template,
    template         = template_xml,
    md_extensions    = "-autolink_bare_uris",
    ...
  )
  doc_format$inherits <- "odt_document"

  return(doc_format)
}


#' Crea un informe estadístico en DOCX -- Markdown
#'
#' Crea automáticamente un informe estadístico en DOCX -- Markdown empleando una
#' plantilla base con detalles del grupo.
#'
#' @export
#' @param template Ruta hasta la plantilla del documento. Por defecto se usa la plantilla
#'   de FISABIO (opción: `default`).
#' @param ... Otras opciones a usar en [rmarkdown::word_document].
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{nuevo_proyecto}}, esto no debería causar molestias, pues se crea
#'   un \emph{*.Rproj} por defecto.
#'
#'   La bibliografía se gestiona a través de pandoc-citeproc y un archivo csl
#'   con el estilo (por defecto, estilo APA). Si quieres cambiar el estilo las
#'   referencias, elimina el archivo csl que se genera por defecto y añade en su
#'   lugar el que tu desees, cambiando la opción en el YAML del documento .Rmd.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{nuevo_proyecto}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de informes/docx y otros en los
#'   directorios de data/cache o figuras/docx).
#'
informe_docx <- function(template = "default", ...) {
  if (template == "default") {
    template <- system.file(
      "rmarkdown/templates/docx/resources/template_fisabio.docx",
      package = "fisabio"
    )
  } else {
    if (!file.exists(template)) {
      stop("No se encuentra la plantilla DOCX.")
    } else {
      if (!grepl("docx$", template)) {
        stop("El archivo proporcionado no parece una plantilla DOCX.")
      }
    }
  }

  doc_format <- officedown::rdocx_document(
    reference_docx   = template,
    md_extensions    = "-autolink_bare_uris",
    ...
  )
  doc_format$inherits <- "word_document"

  return(doc_format)
}


#' Crea una presentación estadística en PDF -- R Markdown -- Beamer
#'
#' Crea automáticamente una presentación estadística en PDF -- R Markdown --
#' Beamer empleando una plantilla base con detalles del grupo. El documento
#' emplea formato R Markdown y tiene extensión .Rmd.
#'
#' @export
#' @param citation_package Paquete para gestionar las citas y referencias. Pandoc citeproc
#'   ha dado problemas con Beamer, de forma que se restringe a natbib y biblatex (opción
#'   por defecto).
#' @param ... Otras opciones a usar en [rmarkdown::beamer_presentation]. No funciona con xelatex...
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{nuevo_proyecto}}, esto no debería causar molestias, pues se crea un
#'   \emph{*.Rproj} por defecto.
#'
#'   Aunque la opción por defecto usa [tinytex::install_tinytex] para obtener una sesión
#'   de LaTeX operativa, siempre se puede optar por usar una versión del SO, en cuyo caso
#'   es necesario tenerla instalada.
#'
#'   El proceso genera un documento rmarkdown que produce un PDF tras ser
#'   compilado con knitr y pdflatex La bibliografía se gestiona a través de
#'   \code{biblatex}.
#'
#'   Con todo, en primer lugar la función comprueba que el proyecto tenga
#'   declaradas todas estas opciones (vienen por defecto al emplear
#'   \code{\link{nuevo_proyecto}}) y que el software apropiado esté instalado. Si
#'   no hubiera un acuerdo entre lo esperado y lo declarado en las opciones del
#'   proyecto, la función pregunta qué hacer para que, o bien se cambien las
#'   opciones del proyecto de forma automática o se detenga la ejecución y se
#'   cambien a mano.
#'
#' @return Crea un documento principal y un conjunto de documentos de respaldo
#'   (algunos directamente en el directorio de informes y otros en los
#'   directorios de cache o figuras).
#'
presentacion_beamer <- function(citation_package = c("biblatex", "natbib"), ...) {
  template   <- system.file(
    "rmarkdown/templates/beamer/resources/template.tex",
    package = "fisabio"
  )
  doc_format <- rmarkdown::beamer_presentation(
    template         = template,
    md_extensions    = "-autolink_bare_uris",
    citation_package = citation_package,
    dev              = "tikz",
    theme            = "metropolis",
    latex_engine     = "lualatex",
    ...
  )
  doc_format$inherits <- "beamer_presentation"

  return(doc_format)
}
