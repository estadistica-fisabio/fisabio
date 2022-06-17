
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.fisabio <- list(
    renv.consent = TRUE
  )
  toset <- !(names(op.fisabio) %in% names(op))
  if(any(toset)) options(op.fisabio[toset])

  invisible()
}
