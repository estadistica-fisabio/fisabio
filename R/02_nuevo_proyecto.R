
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
#' @param git_nombre Cadena de caracteres indicando el usuario vinculado al repositorio
#'   `git`. Por defecto se usa el nombre del servicio de estadística.
#' @param git_correo Cadena de caracteres indicando el correo electrónico vinculado al
#'   repositorio `git`. Por defecto se indica el correo del servicio de estadística.
#' @param renv Valor lógico (TRUE -opción por defecto- o FALSE), indicando si se desea
#'   generar un control de versiones de paquetes con [renv::init()].
#' @param abrir_proyecto Valor lógico (TRUE -opción por defecto- o FALSE), indicando si se
#'   desea abrir el nuevo proyecto en una nueva sesión de R-RStudio.
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
nuevo_proyecto <- function(
    nombre_proyecto = NULL,
    directorio      = NULL,
    git             = TRUE,
    git_nombre      = NULL,
    git_correo      = NULL,
    renv            = TRUE,
    abrir_proyecto  = TRUE
  ) {

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
      dir.create(directorio, recursive = TRUE, mode = "0777")
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
    misel <- grep("_\\d{5}$", tmp)

    if (length(misel) > 0) {
      orden           <- regmatches(
        tmp[misel],
        regexpr("2.\\d{3}$", tmp[misel])
      )
      curr_year <- format(Sys.Date(), "%y")
      years     <- regmatches(
        orden,
        regexpr(".*(?=\\d{3}$)", orden, perl = TRUE)
      )

      if (curr_year %in% years) {
        orden           <- as.numeric(orden)
        orden           <- max(orden) + 1
        orden           <- formatC(orden, digits = 3, flag = "0", format = "d")
        nombre_proyecto <- paste0("fisabio_se_", orden)
      } else {
        nombre_proyecto <- paste0("fisabio_se_", format(Sys.Date(), "%y"), "001")
      }
    } else {
      nombre_proyecto <- paste0("fisabio_se_", format(Sys.Date(), "%y"), "001")
    }
    dir.create(file.path(directorio, nombre_proyecto), mode = "0777")
  } else {
    stopifnot(is.character(nombre_proyecto))
    nombre_proyecto <- tolower(nombre_proyecto)

    if (!dir.exists(file.path(directorio, nombre_proyecto))) {
      dir.create(file.path(directorio, nombre_proyecto), mode = "0777")
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
      paste0("informes/", c("extra", "figuras")), "r", "presupuestos")
  )
  invisible(sapply(sub_dirs, dir.create, recursive = TRUE, mode = "0777"))


  ############################################################################
  #                                                                          #
  # Copia de archivos                                                        #
  #                                                                          #
  ############################################################################

  copy_fisabio(
    from_ = "templates/proyecto.mipro",
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
    from_ = "templates/nota_cargo_interno.xlsx",
    to_   = file.path(
      ruta_trabajo,
      paste0("presupuestos/nota_cargo_interno", nombre_proyecto, ".xlsx")
    )
  )
  copy_fisabio(
    from_ = "templates/presupuesto.xlsx",
    to_   = file.path(
      ruta_trabajo,
      paste0("presupuestos/presupuesto", nombre_proyecto, ".xlsx")
    )
  )
  copy_fisabio(
    from_ = "templates/apa.csl",
    to_   = file.path(ruta_trabajo, "informes/extra/apa.csl")
  )
  copy_fisabio(
    from_ = "templates/vancouver.csl",
    to_   = file.path(ruta_trabajo, "informes/extra/vancouver.csl")
  )
  copy_fisabio(
    from_ = "templates/referencias_prueba.bib",
    to_   = file.path(ruta_trabajo, "informes/extra/referencias.bib")
  )
  copy_fisabio(
    from_ = "templates/se_fisabio.png",
    to_   = file.path(ruta_trabajo, "informes/extra/se_fisabio.png")
  )
  copy_fisabio(
    from_ = "templates/fisabio_gva.png",
    to_   = file.path(ruta_trabajo, "informes/extra/fisabio_gva.png")
  )
  file.create(
    file.path(ruta_trabajo, "informes/figuras/tikzMetrics"),
    showWarnings = FALSE
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
  # Configuración de renv                                                    #
  #                                                                          #
  ############################################################################

  if (renv == TRUE) {
    renv::scaffold(project = ruta_trabajo)
    pkgs <- c(
      "estadistica-fisabio/fisabio"
    )

    dirpkg <- list.dirs(
      path      = file.path(ruta_trabajo, "renv", "library"),
      recursive = FALSE
    )
    dirpkg <- list.dirs(
      path      = dirpkg,
      recursive = FALSE
    )

    renv::install(
      packages     = pkgs,
      project      = ruta_trabajo,
      library      = dirpkg,
      dependencies = c("Depends", "Imports")
    )
    renv::snapshot(
      project  = ruta_trabajo,
      library  = dirpkg,
      lockfile = file.path(ruta_trabajo, "renv.lock"),
      prompt   = FALSE
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
        from_ = "templates/gitignore_fisabio",
        to_   = file.path(ruta_trabajo, ".gitignore")
      )
      if (is.null(git_nombre)) {
        git_nombre <- "Servicio de Estudios Estad\u00edsticos"
      }
      if (is.null(git_correo)) {
        git_correo <- "estadistica_fisabio@gva.es"
      }
      git2r::config(
        repo       = repo,
        user.name  = git_nombre,
        user.email = git_correo
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
  # Aviso final                                                              #
  #                                                                          #
  ############################################################################

  message("No olvides editar el archivo README para describir el proyecto.")

  Sys.sleep(3)
  if (abrir_proyecto & rstudioapi::isAvailable()) {
    rstudioapi::openProject(
      path       = file.path(ruta_trabajo, paste0(nombre_proyecto, ".Rproj")),
      newSession = TRUE
    )
  }
}
