
copy_fisabio <- function(from_, to_) {
  invisible(
    file.copy(
      from      = system.file(from_, package = "fisabio", mustWork = TRUE),
      to        = to_
    )
  )
}
