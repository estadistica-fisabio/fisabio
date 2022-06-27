
#' Comprobaciones para informes y presentaciones
#'
#' Esta función crea una lista con todas las especificaciones necesarias para
#' crear un informe o presentación siguiendo el formato de la plantilla de
#' `fisabio`.
#'
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán un nombre genérico
#'   \code{informe_form}, donde "form" varía en función del formato del informe:
#'   `pdf`, `odt` y `docx`.
#' @param doc_format Cadena de caracteres con el formato del documento. Los
#'   formatos admitidos son `pdf`, `docx`, `odt` y `beamer`
#'
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
#' @param ... Otros argumentos a pasar.
#' @export
#'
#' @details La función solo debe utilizarse desde una llamada a
#'   rmarkdown::render() o dentro de un documento .Rmd
#'
#' @return Objeto con clase "rmarkdown_output_format".
#'
informe_pdf <- function(motor = c("xelatex", "pdftex"), ...) {

  motor <- tolower(motor)
  motor <- match.arg(motor)
  template <- system.file(
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
#' @param nombre Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico
#'   \code{informe}.
#'
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
#' @examples
#' \dontrun{
#' library(fisabio)
#' nuevo_proyecto(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' informe_odt(nombre = "informe_proyecto_x")
#' }
informe_odt <- function(nombre = "informe") {
  comprueba(file_name = nombre, doc_format = "odt")
  report_path <- paste0("informes/", nombre, ".Rmd")

  copy_fisabio(
    from_ = "templates/",
    to_   = file.path(ruta_trabajo, "informes/extra/referencias.bib")
  )
  doc_format <- rmarkdown::odt_document(
    template         = template,
    latex_engine     = motor,
    md_extensions    = "-autolink_bare_uris",
    ...
  )
  doc_format$inherits <- "pdf_document"


  return(doc_format)

  rmarkdown::draft(file = report_path, create_dir = FALSE, template = "odt",
                   package = "fisabio", edit = FALSE)
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(report_path)
  } else {
    utils::file.edit(report_path)
  }
}



#' Crea un informe estadístico en DOCX -- Markdown
#'
#' Crea automáticamente un informe estadístico en DOCX -- Markdown empleando una
#' plantilla base con detalles del grupo.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán el nombre genérico
#'   \code{informe}.
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
#' @examples
#' \dontrun{
#' library(fisabio)
#' nuevo_proyecto(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' informe_docx(file_name = "informe_proyecto_x")
#' }
informe_docx <- function(file_name = "informe") {
  doc_format <- "docx"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/docx"))
    dir.create("informes/docx", recursive = T)
  report_path <- paste0("informes/docx/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE, template = "docx",
                   package = "fisabio", edit = FALSE)
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(report_path)
  } else {
    utils::file.edit(report_path)
  }
}


#' Crea una presentación estadística en PDF -- R Markdown -- Beamer
#'
#' Crea automáticamente una presentación estadística en PDF -- R Markdown --
#' Beamer empleando una plantilla base con detalles del grupo. El documento
#' emplea formato R Markdown y tiene extensión .Rmd.
#'
#' @export
#' @param file_name Cadena de caracteres con el nombre del archivo sin
#'   extensión. Por defecto, los archivos recibirán un nombre genérico
#'   \code{informe}.
#'
#' @details La función solo funciona si el directorio de trabajo se circunscribe
#'   a un proyecto de RStudio, es decir, si existe un archivo \emph{*.Rproj} en
#'   el directorio de trabajo. Si el proyecto se ha creado con
#'   \code{\link{nuevo_proyecto}}, esto no debería causar molestias, pues se crea un
#'   \emph{*.Rproj} por defecto.
#'
#'   Es necesario tener instalada una distribución LaTeX que incorpore (como
#'   mínimo) los siguientes paquetes: \code{adjustbox}, \code{amsmath},
#'   \code{amssymb}, \code{array}, \code{authblk}, \code{beamer},
#'   \code{biblatex}, \code{booktabs}, \code{caption}, \code{ccicons},
#'   \code{csquotes}, \code{float}, \code{fontenc}, \code{fontspec},
#'   \code{footmisc}, \code{graphicx}, \code{hyperref}, \code{letltxmacro},
#'   \code{longtable}, \code{lscape}, \code{mathtools}, \code{microtype},
#'   \code{multirow}, \code{parskip}, \code{polyglossia}, \code{rotating},
#'   \code{setspace}, \code{tabularx}, \code{textpos}, \code{tikz},
#'   \code{xltxtra}, \code{xunicode} y el tema \code{motropolis}.
#'
#'   El proceso genera un documento rmarkdown que produce un PDF tras ser
#'   compilado con knitr y XeLaTeX. La bibliografía se gestiona a través de
#'   \code{biblatex} empleando el gestor de referencias \code{biber}, de modo
#'   que este último debe estar instalado en el sistema.
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
#'   (algunos directamente en el directorio de presentacion/latex y otros en los
#'   directorios de data/cache o figuras/presentacion).
#'
#' @examples
#' \dontrun{
#' library(fisabio)
#' nuevo_proyecto(proj_nom = "proyecto_europeo_X",
#'           proj_dir = "~/proyectos",
#'           git      = TRUE)
#'
#' presentacion_beamer(file_name  = "presentacion_proyecto_x")
#' }
presentacion_beamer <- function(file_name = "presentacion") {
  doc_format <- "rmd_beamer"
  comprueba(file_name = file_name, doc_format = doc_format)
  if (!dir.exists("informes/presentacion"))
    dir.create("informes/presentacion", recursive = T)
  report_path <- paste0("informes/presentacion/", file_name, ".Rmd")
  rmarkdown::draft(file = report_path, create_dir = FALSE,
                   template = "beamer", package = "fisabio", edit = FALSE)
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(report_path)
  } else {
    utils::file.edit(report_path)
  }
}
