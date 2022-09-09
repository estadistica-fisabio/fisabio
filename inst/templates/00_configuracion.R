
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
