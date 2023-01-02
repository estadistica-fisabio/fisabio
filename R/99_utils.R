
copy_fisabio <- function(from_, to_) {
  invisible(
    file.copy(
      from      = system.file(from_, package = "fisabio", mustWork = TRUE),
      to        = to_
    )
  )
}


# #' @title Carga los datos privados de cada proyecto
# #'
# #' @description Todos los datos de cada proyecto se guardan en su estado más bruto
# #'   posible, quedando almacenados en modo local. Para lograr trazabilidad y mejorar la
# #'   reproducibilidad, resulta conveniente la gestión de todos los datos con Git, y esta
# #'   función lee los datos encriptados para que puedan ser utilizados.
# #'
# #' @details La contraseña no se almacena en el historial.
# #'
# #' @param key Cadena de carácteres con la contraseña.
# #' @param tipo Tipo de datos a cargar: población o censo.
# #'
# #' @return Un \code{data.frame} con los datos solicitados.
# #'
# #' @usage carga_datos(key, tipo = c("poblacion", "censo"))
# #'
# #' @seealso \code{\link{censo}}, \code{\link{poblacion}} y \code{\link{descarga_poblaciones}}.
# #'
# #' @keywords datasets
# #'
# #' @examples
# #'
# #' \dontrun{
# #'   # Carga de datos de población
# #'   carga_datos(key = "contraseña", tipo = "poblacion")
# #' }
# #'
# #' @encoding UTF-8
# #'
# #' @export
# carga_datos <- function(key, tipo = c("poblacion", "censo")) {
#
#   stopifnot(is.character(key))
#   tipo <- match.arg(tipo)
#   key  <- sodium::sha256(charToRaw(key))
#   if (tipo == "poblacion") {
#     cifrado <- system.file("data_encrypted", "poblacion.rds",
#                            package = "medear", mustWork = TRUE)
#     cifrado <- unserialize(
#       sodium::data_decrypt(readRDS(cifrado), key)
#     )
#     utils::data("poblacion", envir = environment(), package = "medear")
#     datos <- data.table::rbindlist(
#       list(poblacion, cifrado), fill = TRUE
#     )[order(year, sexo, seccion)]
#     attributes(datos)$fuente <- "Fuente: Sitio web del INE: www.ine.es"
#     class(datos) <- c(class(datos), "poblaciones_ine")
#   } else {
#     cifrado <- system.file("data_encrypted", "censo.rds",
#                            package = "medear", mustWork = TRUE)
#     datos <- unserialize(
#       sodium::data_decrypt(readRDS(cifrado), key)
#     )
#     class(datos) <- c(class(datos), "censos_ine")
#   }
#   return(datos)
#
#   on.exit(
#     try(expr = {
#       ruta <- list.files(getwd(), all.files = TRUE,
#                          pattern = "*\\.Rhistory$", full.names = TRUE)
#       if (length(ruta) > 0) {
#         historial <- readLines(ruta)
#         historial <- historial[!grepl("carga_datos|key\\s?=?", historial)]
#         unlink(ruta, force = TRUE)
#         writeLines(historial, ruta)
#         utils::loadhistory(file = ruta)
#         if (.Platform$OS.type == "unix") {
#           ruta <- list.files("~/.rstudio-desktop/", all.files = TRUE,
#                              pattern = "history_database", full.names = TRUE)
#           for (i in ruta) {
#             historial <- readLines(i)
#             historial <- historial[!grepl("carga_datos|key\\s?=?", historial)]
#             unlink(i, force = TRUE)
#             writeLines(historial, i)
#           }
#         } else {
#           ruta <- list.files(dirname(dirname(tempdir())),
#                              all.files = TRUE,
#                              pattern = "history_database",
#                              full.names = TRUE,
#                              recursive = TRUE)
#           for (i in ruta) {
#             historial <- readLines(i)
#             historial <- historial[!grepl("carga_datos|key\\s?=?", historial)]
#             unlink(i, force = TRUE)
#             writeLines(historial, i)
#           }
#         }
#       }
#     },
#     silent = TRUE)
#   )
# }
