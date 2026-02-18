
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.fisabio <- list(
    renv.consent = TRUE
  )
  toset <- !(names(op.fisabio) %in% names(op))
  if(any(toset)) options(op.fisabio[toset])

  invisible()
}


mifoot <- function(text) {
  officer::fpar(
    fp_p = officer::fp_par(text.align = "center"),
    officer::ftext(
      text = text,
      prop = officer::fp_text(
        color = "red",
        font.size = 7,
        font.family = "Roboto Light"
      )
    )
  )
}


#' Función para cambiar a modo apaisado en MS Word
#' @export
word_start_land <- function() {
  officer::block_section(
    officer::prop_section(
      type = "continuous",
      page_margins = officer::page_mar(footer = 0.6, header = 0.1, right = 0.75, left = 0.75),
      header_default = officer::block_list(
        " ",
        officer::fpar(
          fp_p = officer::fp_par(text.align = "center"),
          officer::external_img("informes/extra/fisabio_gva.png", width = 8, height = 1.5, unit = "cm")
        )
      ),
      footer_default = officer::block_list(
        officer::fpar(
          fp_p = officer::fp_par(text.align = "left"),
          officer::run_word_field(
            field = "PAGE  \\* MERGEFORMAT",
            prop = officer::fp_text(font.size = 9, font.family = "Roboto Light")
          )
        ),
        mifoot(
          paste(
            "FUNDACI\u00d3 PER AL FOMENT DE LA INVESTIGACI\u00d3",
            "SANIT\u00c0RIA I BIOM\u00c9DICA DE LA COMUNITAT VALENCIANA (FISABIO)"
          )
        ),
        mifoot(
          paste(
            "C/ Misser Masc\u00f3 n\u00ba 31. 46010 Val\u00e8ncia. CIF.: G98073760",
            "Inscrita Registre fundacions: 501 V"
          )
        ),
        mifoot("www.fisabio.san.gva.es")
      )
    )
  )
}


#' Función auxiliar para concluir el modo apaisado en MS Word
#' @export
word_stop_land <- function() {
  officer::block_section(
    officer::prop_section(
      page_size = officer::page_size(orient = "landscape"),
      type = "continuous",
      page_margins = officer::page_mar(footer = 0.6, header = 0.1, right = 0.8, left = 0.8, top = 0.8),
      header_default = officer::block_list(
        " ",
        officer::fpar(
          fp_p = officer::fp_par(text.align = "center"),
          officer::external_img("informes/extra/fisabio_gva.png", width = 8, height = 1.5, unit = "cm")
        )
      ),
      footer_default = officer::block_list(
        officer::fpar(
          fp_p = officer::fp_par(text.align = "left"),
          officer::run_word_field(
            field = "PAGE  \\* MERGEFORMAT",
            prop = officer::fp_text(font.size = 9, font.family = "Roboto Light")
          )
        ),
        mifoot(
          paste(
            "FUNDACI\u00d3 PER AL FOMENT DE LA INVESTIGACI\u00d3",
            "SANIT\u00c0RIA I BIOM\u00c9DICA DE LA COMUNITAT VALENCIANA (FISABIO)"
          )
        ),
        mifoot(
          paste(
            "C/ Misser Masc\u00f3 n\u00ba 31. 46010 Val\u00e8ncia. CIF.: G98073760",
            "Inscrita Registre fundacions: 501 V"
          )
        ),
        mifoot("www.fisabio.san.gva.es")
      )
    )
  )
}
