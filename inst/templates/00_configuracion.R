
pkgs <- c(
  "remotes",
  "data.table",
  "readxl",
  "haven",
  "lubridate",
  "rmarkdown",
  "gtsummary",
  "flextable",
  "labelled",
  "kableExtra"
)

invisible(
  sapply(
    pkgs,
    function(x) {
      if (!x %in% installed.packages()[, 1])
        install.packages(x)
      suppressPackageStartupMessages(require(x, character.only = TRUE))
    }
  )
)

rm(pkgs)

remotes::install_github("estadistica-fisabio/fisabio")
library(fisabio)


# Opciones ----------------------------------------------------------------
colores <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7")



# Funciones ---------------------------------------------------------------


# No olvides iniciar renv al finalizar el proyecto para gestionar mejor las versiones
# renv::init()
