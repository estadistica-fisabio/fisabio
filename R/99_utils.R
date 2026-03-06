
copy_fisabio <- function(from_, to_) {
  invisible(
    file.copy(
      from      = system.file(from_, package = "fisabio", mustWork = TRUE),
      to        = to_
    )
  )
}


#' Formato de tres números como estimación e IC
#'
#' Formatea tres números como `EST (IC XX %: LOWER; UPPER)` fijando el número de decimales.
#'
#' @export
#' @param est Estimación.
#' @param lower Nivel inferior del IC.
#' @param upper Nivel superior del IC.
#' @param digitos Número de dígitos del redondeo. Por defecto 2.
#' @param texto Texto abreviado para intervalo de confianza. Por defecto `IC 95 %:`.
#'
#' @return La función devuelve una cadena de texto con la estructura deseada.
#'
#' @examples
#' printest(est = 0.95, lower = 0.8, upper = 1.13, texto = "IC 95:", digitos = 2)
printest <- function(est = NULL, lower = NULL, upper = NULL, texto = "IC 95 %:", digitos = 2) {

  if (is.null(est) || is.null(lower) || is.null(upper)) {
    stop("Debes aportar un valor para est, lower y upper.")
  }
  stopifnot(is.numeric(digitos))
  stopifnot(is.character(texto))

  miest <- paste0(
    gtsummary::style_number(est, digits = digitos),
    " (",
    texto,
    " ",
    gtsummary::style_number(lower, digits = digitos),
    "; ",
    gtsummary::style_number(upper, digits = digitos),
    ")"
  )


  return(miest)
}

