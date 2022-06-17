
#' Crea un nuevo proyecto con la estructura `fisabio`
#'
#' Crea automáticamente un proyecto siguiendo las directrices comunes de `fisabio`,
#' empleando una jerarquía de directorios unificada, cargando archivos y funciones comunes
#' del grupo de colaboradores. Opcionalmente, genera un repositorio `git` y un control de
#' versiones de paquetes de `R` con `renv`.
#'
#' @export
#' @param nombre_proyecto Cadena de caracteres con el nombre a asignar al proyecto. Por
#'   defecto se asigna el nombre "fisabio_se_yyXXXX", donde `yy` es el año y XXXX es el
#'   código correlativo de proyecto para ese año. La función espera que cada proyecto esté
#'   ubicado en un directorio raíz destinado a proyectos vigentes y, en caso de que este
#'   argumento se deje en blanco y no se detecte la estructura que este directorio debería
#'   tener, inicia una nueva cadena de proyecto (p. ej., para el primer proyecto del año
#'   2022: fisabio_se_220001).
#' @param directorio Cadena de caracteres indicando la ruta al directorio principal donde
#'   se creará el proyecto. Por defecto se escoge el directorio actual de trabajo,
#'   devuelto por la función [base::getwd()]. Si este argumento no esta vacío y el
#'   directorio indicado no existe, la función tratará de crearlo de forma recursiva.
#' @param git Valor lógico (TRUE -opción por defecto- o FALSE), indicando si se desea
#'   generar un repositorio `git` asociado al proyecto.
#' @param renv Valor lógico (TRUE -opción por defecto- o FALSE), indicando si se desea
#'   generar un control de versiones de paquetes con [renv::init()].
#' @details La función crea la estructura de directorios a partir del nombre del proyecto
#'   empleando minúsculas, con independencia de si el usuario ha introducido el argumento
#'   con mayúsculas.
#'
#'   Los informes se almacenan, junto con sus figuras y datos de caché (si los hubiera),
#'   en el directorio informes.
#' @return La función crea la siguiente estructura de directorios en directorio/nombre_proyecto/:
#'   * r
#'   * informes
#'   * datos
#'       - brutos
#'       - procesados
#'       - documentacion
#' @examples
#' \dontrun{
#' library(fisabio)
#' nuevo_proyecto(nombre_proyecto = "proyecto_europeo_X", directorio = "~/proyectos")
#' }
nuevo_proyecto <- function(nombre_proyecto = NULL, directorio = NULL, git = TRUE, renv = TRUE) {

  ############################################################################
  #                                                                          #
  # Comprobaciones iniciales                                                 #
  #                                                                          #
  ############################################################################

  stopifnot(is.logical(git))
  stopifnot(is.logical(renv))

  if (is.null(directorio)) {
    directorio <- getwd()
  } else {
    stopifnot(is.character(directorio))

    if (!dir.exists(directorio)) {
      dir.create(directorio, recursive = TRUE)
    } else {
      dir_info <- sapply(c(0, 2, 4), function(x)
        file.access(names = directorio, mode = x))
      if (dir_info[1] != 0)
        stop("Revisa el directorio de proyecto que has proporcionado. ",
             "Quiz\u00e1 el problema sea que no existe el directorio inmediatamente ",
             "superior o alguno intermediario...")
      if (any(dir_info[-1] != 0)) {
        stop("No tienes permisos de lectura o escritura en ese directorio: prueba otro.")
      }
    }
  }

  if (is.null(nombre_proyecto)) {
    tmp   <- list.dirs(directorio, full.names = TRUE, recursive = FALSE)
    tmp   <- basename(tmp)
    misel <- grep("\\d{6}$", tmp)

    if (length(misel) > 0) {
      orden           <- regmatches(tmp[misel], regexpr("\\d{4}$", tmp[misel]))
      orden           <- as.numeric(orden)
      orden           <- max(orden) + 1
      orden           <- formatC(orden, digits = 3, flag = "0")
      nombre_proyecto <- paste0("fisabio_se_", format(Sys.Date(), "%y"), orden)
    } else {
      nombre_proyecto <- paste0("fisabio_se_", format(Sys.Date(), "%y"), "0001")
    }
    dir.create(file.path(directorio, nombre_proyecto))
  } else {
    stopifnot(is.character(nombre_proyecto))
    nombre_proyecto <- tolower(nombre_proyecto)

    if (!dir.exists(file.path(directorio, nombre_proyecto))) {
      dir.create(file.path(directorio, nombre_proyecto))
    } else {
      stop(
        "\u00a1El directorio que iba a crearse ya existe!",
        " Usa un nombre de proyecto diferente."
      )
    }
  }
  cat("\nCreando estructura FISABIO en", file.path(directorio, nombre_proyecto), "...\n")
  ruta_trabajo <- file.path(directorio, nombre_proyecto)


  ############################################################################
  #                                                                          #
  # Crear subdirectorios                                                     #
  #                                                                          #
  ############################################################################

  sub_dirs <- file.path(
    ruta_trabajo,
    c(paste0("datos/", c("brutos", "procesados", "documentacion")),
      "informes", "r", "presupuestos")
  )
  invisible(sapply(sub_dirs, dir.create, recursive = TRUE))


  ############################################################################
  #                                                                          #
  # Copia de archivos                                                        #
  #                                                                          #
  ############################################################################

  copy_fisabio(
    from_ = "templates/proyecto.Rproj",
    to_   = file.path(ruta_trabajo, paste0(nombre_proyecto, ".Rproj"))
  )
  copy_fisabio(
    from_ = "templates/00_configuracion.R",
    to_   = file.path(ruta_trabajo, "r", "00_configuracion.R")
  )
  copy_fisabio(
    from_ = "templates/README.Rmd",
    to_   = file.path(ruta_trabajo, "README.Rmd")
  )
  copy_fisabio(
    from_ = "templates/apa.csl",
    to_   = file.path(ruta_trabajo, "informes/apa.csl")
  )
  copy_fisabio(
    from_ = "templates/vancouver.csl",
    to_   = file.path(ruta_trabajo, "informes/vancouver.csl")
  )
  copy_fisabio(
    from_ = "templates/referencias_prueba.bib",
    to_   = file.path(ruta_trabajo, "informes/referencias.bib")
  )
  knitr::knit(
    input    = file.path(ruta_trabajo, "README.Rmd"),
    output   = file.path(ruta_trabajo, "README.md"),
    quiet    = TRUE,
    encoding = "UTF-8"
  )

  sample_scripts <- paste0(
    "r/",
    c("00_configuracion.R", "01_importar_datos.R", "02_depurar_datos.R")
  )
  for (i in seq_along(sample_scripts)[-1]) {
    writeLines(
      text = paste0("\nsource(\"", sample_scripts[i - 1], "\")\n\n"),
      con  = file.path(ruta_trabajo, sample_scripts[i])
    )
  }


  ############################################################################
  #                                                                          #
  # Configuración de Git                                                     #
  #                                                                          #
  ############################################################################

  if (.Platform$OS.type == "windows") {
    git_exist <- file.exists(
      list.files(
        path        = paste0("c:/program files", c("", " (x86)"), "/Git/bin"),
        pattern     = "git",
        recursive   = TRUE,
        ignore.case = TRUE,
        full.names  = TRUE
      )
    )
  } else {
    git_exist <- file.exists(Sys.which("git"))
  }
  if (!git_exist && git == TRUE) {
    warning("No tienes instalado git, as\u00ed que no se puede llevar un control de versiones.")
  } else if (git == TRUE) {
    if (git2r::in_repository(ruta_trabajo)) {
      warning("Ya exite el repositorio git: no se hace nada.")
    } else {
      repo <- git2r::init(ruta_trabajo)
      copy_fisabio(
        from_ = "templates/.gitignore",
        to_   = file.path(ruta_trabajo, ".gitignore")
      )
      git2r::config(
        repo       = repo,
        user.name  = "Servicio de Estudios Estadísticos",
        user.email = "estadistica_fisabio@gva.es"
      )
      git2r::add(repo = repo, path = "*")
      invisible(
        git2r::commit(
          repo    = repo,
          message = "Primer commit: creo proyecto",
          all     = TRUE
        )
      )
    }
  } else message("Proyecto configurado sin git: puedes emplearlo m\u00e1s adelante.")


  ############################################################################
  #                                                                          #
  # Configuración de renv                                                    #
  #                                                                          #
  ############################################################################

  if (renv == TRUE) {
    renv::init(project = ruta_trabajo, restart = TRUE, bare = TRUE)
  }


  ############################################################################
  #                                                                          #
  # Aviso final                                                              #
  #                                                                          #
  ############################################################################

  message("No olvides editar el archivo README para describir el proyecto.")
}





