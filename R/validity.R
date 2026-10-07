
# UI ----------------------------------------------------------------------

validityUI <- function(
    id
    ) {
  
  ns <- shiny::NS(id)
  tr <- spectran_help_text
  
  htmltools::tagList(
        htmltools::h3(lang$ui(153)),
        htmltools::h4(lang$ui(154)),
        htmltools::p(
                     lang$ui(157)
                     ),
        htmltools::tags$script(
          htmltools::HTML(
            "$(document).ready(function() {
            function setIframeHeight() {
            var windowHeight = $(window).height();
            $('#validation').height(windowHeight - 250); 
            // 70px for the navbar
            }
            setIframeHeight();
            $(window).resize(setIframeHeight);
            });
            ")),
        htmltools::tags$iframe(id = "validation", 
                               src = 'https://litgde.github.io/Spectran/articles/Validation.html',
                               allowfullscreen='true',
                               frameborder='0',
                               allowtransparency='true',
                               width = "100%"),
        htmltools::h4(tr("Validierung von Transmission und Reflexion",
                        "Transmission and reflection validation")),
        htmltools::p(tr(
          "Vergleich der photopischen und melanopischen Transmissions- und Reflexionsgrade mit DIN/TS 67600. Der Artikel zeigt die Referenzwerte, Berechnungen und Abweichungen (Englisch).",
          "Comparison of photopic and melanopic transmittance and reflectance with DIN/TS 67600. The article presents the reference values, calculations and differences.")),
        htmltools::tags$iframe(id = ns("material_validation"),
                               src = "https://litgde.github.io/Spectran/articles/Material_Validation.html",
                               title = tr("Materialvalidierung (Englisch)", "Material validation"),
                               loading = "lazy",
                               style = "height: calc(100vh - 250px);",
                               allowfullscreen = "true",
                               frameborder = "0",
                               allowtransparency = "true",
                               width = "100%"),
        htmltools::h4(lang$ui(179)),
        htmltools::p(lang$ui(155), 
                     htmltools::a(.noWS = "outside", 
                                  "DIN/TS 5031-100:2021-11", 
                                  href = URL_DIN, target="_blank"),
                     lang$ui(156),
                     htmltools::a(.noWS = "outside", 
                                  "CIE S026 (2018)", 
                                  href = URL_CIE, target="_blank"),
                     "."),
        htmltools::tags$script(
          htmltools::HTML(
            "$(document).ready(function() {
            function setIframeHeight() {
            var windowHeight = $(window).height();
            $('#values').height(windowHeight - 250); 
            // 70px for the navbar
            }
            setIframeHeight();
            $(window).resize(setIframeHeight);
            });
            ")),
        htmltools::tags$iframe(id = "values", 
                               src = 'https://litgde.github.io/Spectran/articles/Values.html',
                               allowfullscreen='true',
                               frameborder='0',
                               allowtransparency='true',
                               width = "100%")
  )
  }

# Server ------------------------------------------------------------------

# App ---------------------------------------------------------------------
