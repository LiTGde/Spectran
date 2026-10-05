spectran_introduction_dependency <- function() {
  htmltools::htmlDependency("spectran-introduction", "1.0.0",
    src = c(file = system.file("app/www", package = "Spectran")),
    stylesheet = "spectran-introduction.css", all_files = FALSE)
}

# Bootstrap's native dialog handles Escape, the backdrop and return focus.
# The same local image is used for the thumbnail and the enlarged view.
spectran_introduction_lightbox <- function(id, image_path, caption) {
  tr <- spectran_help_text
  htmltools::div(id = id, class = "modal spectran-intro-lightbox", tabindex = "-1",
    role = "dialog", `aria-modal` = "true", `aria-labelledby` = paste0(id, "-title"),
    htmltools::div(class = "modal-dialog modal-lg", role = "document",
      htmltools::div(class = "modal-content",
        htmltools::div(class = "modal-header",
          htmltools::tags$button(type = "button", class = "close", `data-dismiss` = "modal",
            `aria-label` = tr("Bildansicht schlie\u00dfen", "Close image viewer"),
            htmltools::span(`aria-hidden` = "true", "\u00d7")),
          htmltools::h2(id = paste0(id, "-title"), class = "modal-title", caption)),
        htmltools::tags$input(type = "checkbox", id = paste0(id, "-zoom"),
          class = "spectran-intro-zoom", `aria-controls` = paste0(id, "-image")),
        htmltools::tags$label(`for` = paste0(id, "-zoom"), class = "spectran-intro-zoom-label",
          tr("Originalgr\u00f6\u00dfe", "Original size")),
        htmltools::div(id = paste0(id, "-image"), class = "modal-body", tabindex = "0",
          role = "region", `aria-label` = tr("Bildbereich, mit den Pfeiltasten verschieben", "Image area, use arrow keys to pan"),
          htmltools::p(class = "spectran-intro-pan-hint",
            tr("Im Bild scrollen oder wischen, um Details anzusehen.", "Scroll or swipe within the image to see details.")),
          htmltools::img(src = image_path, alt = caption, loading = "lazy")),
        htmltools::div(class = "modal-footer",
          htmltools::tags$button(type = "button", class = "btn btn-default",
            `data-dismiss` = "modal", tr("Schlie\u00dfen", "Close"))))))
}

introductionUI <- function(id) {
  ns <- shiny::NS(id)
  tr <- spectran_help_text
  images <- image_gallery()
  examples <- function(key, indices, gallery = images) {
    gallery$images <- gsub("_", " ", gallery$images, fixed = TRUE)
    htmltools::div(class = "spectran-intro-examples",
      htmltools::h3(tr("Beispielansichten (Englisch)", "Example screens")),
      htmltools::div(class = "spectran-intro-gallery", lapply(indices, function(i) {
        htmltools::tags$figure(
          htmltools::tags$button(type = "button", class = "spectran-intro-thumbnail",
            id = ns(paste0(key, "-preview-", i)), `data-toggle` = "modal",
            `data-target` = paste0("#", ns(paste0(key, "-lightbox-", i))),
            `aria-haspopup` = "dialog",
            `aria-label` = paste(gallery$images[i], tr("vergr\u00f6\u00dfern", "enlarge image")),
            htmltools::img(src = gallery$image_path[i], alt = gallery$images[i], loading = "lazy")),
          htmltools::tags$figcaption(gallery$images[i]))
      })),
      lapply(indices, function(i) spectran_introduction_lightbox(
        ns(paste0(key, "-lightbox-", i)), gallery$image_path[i], gallery$images[i])))
  }
  feature <- function(key, icon, title, lead, items, detail) {
    htmltools::tags$section(id = ns(key), class = "spectran-intro-feature",
      htmltools::div(class = "spectran-intro-feature-heading",
        htmltools::span(class = "spectran-intro-feature-icon", shiny::icon(icon)), htmltools::h2(title)),
      htmltools::p(class = "spectran-intro-feature-lead", lead),
      htmltools::tags$ul(lapply(items, htmltools::tags$li)), detail)
  }
  ui <- htmltools::tags$main(class = "spectran-intro-page",
    htmltools::tags$header(class = "spectran-intro-hero",
      htmltools::div(class = "spectran-intro-hero-copy",
        htmltools::p(class = "spectran-intro-eyebrow", "LiTG \u00b7 SPECTRAN"),
        htmltools::h1(tr("Licht verstehen. Mit Spektren arbeiten.", "Understand light. Explore its spectrum.")),
        htmltools::p(class = "spectran-intro-lead", tr(
          "Spektraldaten f\u00fcr Lichtplanung, Lehre und Pr\u00e4sentation. Untersuchen Sie Lichtquellen und verfolgen Sie, wie Materialien ihr Licht ver\u00e4ndern.",
          "Spectral data for lighting design, teaching and presentations. Explore light sources and see how materials change their light.")),
        htmltools::div(class = "spectran-intro-actions",
          shiny::actionButton(ns("zu_Import1"), tr("Lichtquelle ausw\u00e4hlen", "Choose a light source"),
            icon = shiny::icon("arrow-right"), class = "btn-primary"),
          shiny::actionButton(ns("to_material"), htmltools::tagList(
            htmltools::span(class = "spectran-intro-material-icons", `aria-hidden` = "true",
              shiny::icon("filter"), htmltools::span("/"), shiny::icon("reply")),
            htmltools::span(tr("Materialwirkung untersuchen", "Explore material effects"))),
            class = "btn-default spectran-intro-material-button"))),
      htmltools::div(class = "spectran-intro-hero-visual", `aria-hidden` = "true",
        htmltools::img(src = "extr/Frontbild.png", alt = "", width = "1680", height = "729")),
      htmltools::p(class = "spectran-intro-background", tr(
        "Spectran ist eine Anwendung der LiTG f\u00fcr die Auswertung und anschauliche Aufbereitung von Spektralmessungen. Weitere Werkzeuge f\u00fcr die spektrale Bewertung sind die ",
        "Spectran is a LiTG application for analysing and presenting spectral measurements. Other tools for spectral assessment include the "),
        htmltools::a("CIE S 026 Toolbox", href = "https://files.cie.co.at/CIE%20S%20026%20alpha-opic%20Toolbox%20User%20Guide.pdf", target = "_blank", rel = "noopener"),
        tr(" und ", " and "), htmltools::a("luox", href = "https://luox.app", target = "_blank", rel = "noopener"), ".")),
    htmltools::div(class = "spectran-intro-features",
      feature("import", "file-import", "Import",
        tr("Mit einer Messung, einem Beispiel oder einer eigenen Lichtquelle starten.", "Start with a measurement, an example or a light source you create."),
        c(tr("CSV-Dateien importieren und die Spalten und Einheiten pr\u00fcfen.", "Import CSV files and check their columns and units."),
          tr("Beispiellichtquellen ausw\u00e4hlen, anpassen und herunterladen.", "Choose, adjust and download example light sources."),
          tr("Im Spektralbaukasten eigene Spektren zusammensetzen.", "Assemble your own spectra with the spectrum builder.")), examples("import", 1:3)),
      feature("analysis", "magnifying-glass-chart", lang$ui(23),
        tr("Spektren darstellen und ihre Kenngr\u00f6\u00dfen im Zusammenhang verstehen.", "Visualise spectra and understand their metrics in context."),
        c(tr("Bestrahlungsst\u00e4rke, Beleuchtungsst\u00e4rke, Lichtfarbe und Farbwiedergabe auswerten.", "Assess irradiance, illuminance, light colour and colour rendering."),
          tr("Melanopische und weitere \u03b1-opische Bewertungen vergleichen, einschlie\u00dflich EDI und DER.", "Compare melanopic and other \u03b1-opic assessments, including EDI and DER."),
          tr("Den modellierten Einfluss von Alter, Augenmedien und Pupille untersuchen.", "Explore the modelled effects of age, ocular media and pupil size.")), examples("analysis", 4:6)),
      feature("material", "layer-group", tr("Transmission und Reflexion", "Transmission and reflection"),
        tr("Untersuchen, wie ein Material das einfallende Licht ver\u00e4ndert.", "Explore how a material changes the incident light."),
        c(tr("Materialien mit Vorschau aus der Bibliothek w\u00e4hlen oder eigene CSV-Kurven verwenden.", "Preview materials in the library or use your own CSV curves."),
          tr("Das Lichtniveau als Beleuchtungsst\u00e4rke oder melanopische EDI vorgeben und Ein- und Ausgang vergleichen.", "Set illuminance or melanopic EDI and compare the incident and outgoing light."),
          tr("Ergebnisse im Lichtpfad speichern und einzelne Schritte oder die Gesamtwirkung vergleichen.", "Save results in the light path and compare individual steps or the combined effect.")),
        examples("material", 1:3, list(
          images = c(tr("Materialauswahl", "Material selection"),
            tr("Ergebnisvergleich", "Result comparison"), tr("Lichtpfad", "Light path")),
          image_path = paste0("extr/intro-examples/", c("material-library.jpg", "material-result.jpg", "material-light-path.jpg"))))),
      feature("export", "file-export", "Export",
        tr("Die passende Ausgabe f\u00fcr Bericht, Pr\u00e4sentation oder weitere Auswertung erstellen.", "Create the right output for a report, presentation or further analysis."),
        c(tr("Abbildungen und Tabellen als PNG oder PDF passend gestalten.", "Customise figures and tables as PNG or PDF files."),
          tr("Kenngr\u00f6\u00dfen in Excel und Spektren als CSV weiterverwenden.", "Reuse metrics in Excel and spectra as CSV files."),
          tr("Im Materialmodul aktuelle oder gespeicherte Ergebnisse sowie das Lichtpfad-Diagramm exportieren.", "Export current or saved material results and the light-path plot.")), examples("export", 7:9))),
    htmltools::tags$section(class = "spectran-intro-learning",
      htmltools::div(htmltools::h2(tr("Zusammenh\u00e4nge verstehen", "Understand the concepts")),
        htmltools::p(tr("Die Erl\u00e4uterungen verbinden kurze Texte mit Grafiken: vom Lesen eines Spektrums \u00fcber EDI und DER bis zum Material und zum Lichtpfad.",
          "The explanations combine short text and illustrations, from reading a spectrum and understanding EDI and DER to materials and light paths."))),
      shiny::actionButton(ns("to_explanations"), tr("Erl\u00e4uterungen \u00f6ffnen", "Open explanations"),
        icon = shiny::icon("book-open"), class = "btn-default")),
    htmltools::tags$section(class = "spectran-intro-tutorial",
      htmltools::h2(tr("Video zum Grundmodul", "Core application video")),
      htmltools::p(tr("Das Video f\u00fchrt durch Import, Auswertung und Export des Grundmoduls. Das neue Materialmodul ist in den Erl\u00e4uterungen beschrieben.",
        "The video introduces import, analysis and export in the core application. The explanations cover the new material module.")),
      htmltools::tags$video(id = ns("video_tutorial"), src = lang$server(1), preload = "none",
        controls = NA, `aria-label` = tr("Einf\u00fchrungsvideo zum Grundmodul", "Core application introduction video"))))
  htmltools::attachDependencies(ui, spectran_introduction_dependency())
}

# The parent owns navigation. Repeated choices remain separate events.
introductionServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    event <- shiny::reactiveVal(NULL)
    sequence <- 0L
    routes <- c(zu_Import1 = "import", to_material = "transmission", to_explanations = "explanations")
    for (key in names(routes)) local({
      action <- key
      page <- unname(routes[[key]])
      shiny::observeEvent(input[[action]], {
        sequence <<- sequence + 1L
        event(list(page = page, sequence = sequence))
      }, ignoreInit = TRUE)
    })
    shiny::reactive(event())
  })
}

# Retained isolated showcase for the introduction's navigation events.
introduction_app <- function(language = "Deutsch") {
  the$language <- language
  shiny::addResourcePath("extr", system.file("app/www", package = "Spectran"))
  shiny::shinyApp(shiny::fluidPage(introductionUI("intro"), shiny::textOutput("destination")),
    function(input, output, session) {
      destination <- introductionServer("intro")
      output$destination <- shiny::renderText({
        event <- destination()
        if (is.null(event)) "Ready" else paste(event$page, event$sequence)
      })
    })
}
