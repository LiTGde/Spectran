
# UI ----------------------------------------------------------------------

#create a Date and R-Version object for the day of Package-Generation
Date <- Sys.Date()
R <- R.version$version.string

impressumUI <- function(
    id
    ) {
  
  ns <- shiny::NS(id)
  tr <- spectran_help_text
  #List all the used packages
  # packages <- utils::packageDescription("Spectran")$Imports %>% 
  #   stringr::str_split_1(pattern = ",\\n")
  
  htmltools::tagList(
    htmltools::h3(lang$ui(158)),
    #Project funder, author, project comittee
    htmltools::p(
      htmltools::img(
        width = "250px", 
        src = "extr/litg-logo.png", alt = "LiTG", loading = "lazy"
      ), htmltools::br(),
      htmltools::h4(lang$ui(162), htmltools::br(), htmltools::br(),
      lang$ui(163), htmltools::br(),
      htmltools::strong("Johannes Zauner"), htmltools::br(), htmltools::br(),
      tr("Projektausschuss Grundmodul (in alphabetischer Reihenfolge):", "Core application project committee (in alphabetical order):"), htmltools::br(),
      htmltools::strong("Meike Barfu\u00df, Nils Haferkemper, Sylvia Hubalek")), 
      htmltools::br(),
      #Details on the LiTG
      htmltools::strong(lang$ui(172)), htmltools::br(),
      lang$ui(173), htmltools::br(),
      lang$ui(174), htmltools::br(),
      lang$ui(175), htmltools::br(),
      "Email: ", htmltools::a(.noWS = "outside", 
                              "info@litg.com", 
                              href = "mailto:info@litg.com"), htmltools::br(),
      "Web: ", htmltools::a(.noWS = "outside", 
                              "www.litg.de", 
                              href = "https://www.litg.de",
                            target = "_blank"),
      htmltools::br(),
      htmltools::br(),
      #Information about the software context of creation
      lang$ui(159), 
      " ", as.character(utils::packageVersion("Spectran")), 
      " ",
      lang$ui(160),  
      R, 
      lang$ui(161)),
    htmltools::tags$section(class = "spectran-about-material",
      htmltools::p(htmltools::strong(tr("Zusatzmodul: Transmission und Reflexion", "Additional module: transmission and reflection"))),
      htmltools::p(tr(
        "Das LiTG-Modul erweitert Spectran um die spektrale Wirkung von Materialien. Materialkurven aus Bibliotheken oder CSV-Dateien lassen sich auf eine Lichtquelle anwenden. Gespeicherte Schritte k\u00f6nnen als Lichtpfad und in ihrer Gesamtwirkung verglichen und exportiert werden.",
        "The LiTG module extends Spectran with spectral material effects. Material curves from libraries or CSV files can be applied to a light source. Saved steps can be compared and exported as a light path and as a combined effect.")),
      htmltools::p(htmltools::strong(tr("Projektausschuss (in alphabetischer Reihenfolge):", "Project committee (in alphabetical order):")),
        htmltools::br(), "Karin Bieske, Kai Broszio, Nils Haferkemper")),
    #Information about points of contact
    htmltools::p(
      lang$ui(176), 
      htmltools::a(.noWS = "outside", 
                   " Github", 
                   href = "https://github.com/LiTGde/Spectran",
                   target = "_blank"),
      htmltools::br(),
      lang$ui(164), 
      htmltools::a(
        .noWS = "outside", lang$ui(165), href = "mailto:spectran@litg.com"
        )
      ),
    #Information about citation
    htmltools::p(
      lang$ui(166), 
      htmltools::br(),htmltools::br(),
      htmltools::strong("Source Code: "), htmltools::br(),
      htmltools::em("Zauner, J. (2024); German Society for Lighting Technology and Lighting Design; Spectran: Visual and Non-Visual Spectral Analysis with an Emphasis on Education and Presentation-Ready Diagrams. Available on https://litgde.github.io/Spectran/. doi: 10.5281/zenodo.11518043. RRID: SCR_025407"),
    )
  )
}

# Server ------------------------------------------------------------------

# App ---------------------------------------------------------------------
