# Task-oriented presentation of the existing material calculation.

material_workspace_text <- function(key, ...) {
  strings <- list(
    title = c("Transmission & reflection", "Transmission & Reflexion"),
    intro = c("Explore how a material changes light.", "Entdecken Sie, wie ein Material Licht ver\u00e4ndert."),
    setup = c("1  Set up", "1  Ausw\u00e4hlen"),
    results = c("2  Results", "2  Ergebnisse"),
    path = c("Light path", "Lichtpfad"),
    history_step = c("Step", "Schritt"),
    history_from = c("From step", "Aus Schritt"),
    history_start = c("Start", "Start"),
    history_help = c("Show displays a step's comparison and saved result below. Restore also makes its light spectrum the active source for the next material. Yellow marks the active source. Later steps and alternative paths are kept.", "Anzeigen zeigt den Vergleich und das gespeicherte Ergebnis eines Schritts unten. Wiederherstellen verwendet dessen Lichtspektrum auch als aktive Quelle f\u00fcr das n\u00e4chste Material. Gelb markiert die aktive Quelle. Sp\u00e4tere Schritte und alternative Pfade bleiben erhalten."),
    history_started = c("A new light path starts with source %s.", "Ein neuer Lichtpfad beginnt mit Quelle %s."),
    history_continued = c("%s is now the active light source (step %s). Its material result is saved below.", "%s ist jetzt die aktive Lichtquelle (Schritt %s). Das Materialergebnis ist unten gespeichert."),
    history_restored = c("Step %s, %s, is now the active light source. Later steps are kept.", "Schritt %s, %s, ist jetzt die aktive Lichtquelle. Sp\u00e4tere Schritte bleiben erhalten."),
    history_saved = c("Saved material result", "Gespeichertes Materialergebnis"),
    history_saved_help = c("This is the saved result for the step selected with Show. The active light source is marked in yellow above.", "Dies ist das gespeicherte Ergebnis des mit Anzeigen ausgew\u00e4hlten Schritts. Die aktive Lichtquelle ist oben gelb markiert."),
    history_source_help = c("This is the starting light source. Choose Show beside a later material step to see its saved result.", "Dies ist die urspr\u00fcngliche Lichtquelle. W\u00e4hlen Sie Anzeigen bei einem sp\u00e4teren Materialschritt, um dessen gespeichertes Ergebnis zu sehen."),
    history_combined = c("Combined effect up to this step", "Gesamtwirkung bis zu diesem Schritt"),
    history_combined_help = c("Compares the starting light with the light at the selected step, including only the materials along that light path. Combined values are shown only if no different illuminance was set between steps.", "Vergleicht das Ausgangslicht mit dem Licht am ausgew\u00e4hlten Schritt. Ber\u00fccksichtigt werden nur die Materialien auf diesem Lichtpfad. Gesamtwerte werden nur angezeigt, wenn zwischen den Schritten keine andere Beleuchtungsst\u00e4rke eingestellt wurde."),
    history_adjusted = c("Combined values are unavailable because a different illuminance was set between steps. Choose a step before that adjustment or a light path without an adjustment. You can still view each saved material result.", "Die Gesamtwerte sind nicht verf\u00fcgbar, da zwischen den Schritten eine andere Beleuchtungsst\u00e4rke eingestellt wurde. W\u00e4hlen Sie einen Schritt vor dieser Anpassung oder einen Lichtpfad ohne Anpassung. Alle gespeicherten Materialergebnisse bleiben verf\u00fcgbar."),
    downloads = c("Downloads", "Downloads"),
    navigation = c("Material workflow", "Material-Workflow"),
    source = c("Incident light", "Einfallendes Licht"),
    change_source = c("Change light source", "Lichtquelle \u00e4ndern"),
    use_source = c("Use light source", "Lichtquelle verwenden"),
    examples = c("Example sources", "Beispielquellen"),
    choose_spectrum = c("Choose a spectrum", "Spektrum ausw\u00e4hlen"),
    source_examples_help = c("Choose a category, preview a spectrum, then use it as the incident light. Measured examples are illustrative; standard illuminants are identified below.", "W\u00e4hlen Sie eine Kategorie und pr\u00fcfen Sie das Spektrum, bevor Sie es als einfallendes Licht verwenden. Messbeispiele dienen der Veranschaulichung; Normlichtarten sind unten gekennzeichnet."),
    file_help = c("Upload wavelength (nm) and spectral irradiance (W/m\u00b2/nm). Check the preview before using the source.", "Laden Sie Wellenl\u00e4nge (nm) und spektrale Bestrahlungsst\u00e4rke (W/m\u00b2/nm). Pr\u00fcfen Sie die Vorschau, bevor Sie die Quelle verwenden."),
    csv_settings = c("CSV format settings", "CSV-Formateinstellungen"),
    sample_source = c("Try the sample file", "Beispieldatei ausprobieren"),
    model = c("Calculation model", "Berechnungsmodell"),
    model_brief = c("Receiver values assume F = 1: all transmitted or reflected light reaches the receiver. Geometry can reduce the received fraction.", "Empf\u00e4ngerwerte setzen F = 1 voraus: Das gesamte transmittierte oder reflektierte Licht erreicht den Empf\u00e4nger. Die Geometrie kann den empfangenen Anteil verringern."),
    source_help = c("Choose a bundled example, upload a spectrum, or create your own.", "W\u00e4hlen Sie ein Beispiel, laden Sie ein Spektrum oder erstellen Sie ein eigenes."),
    automatic = c("Starting example \u00b7 daylight D65", "Startbeispiel \u00b7 Tageslicht D65"),
    current = c("Active source for the next calculation", "Aktive Quelle f\u00fcr die n\u00e4chste Berechnung"),
    negative_source = c("This source contains negative measured values. Results remain available, but it cannot be used for a sequence of materials. Choose a standard illuminant or upload corrected measurements using Change light source.", "Diese Quelle enth\u00e4lt negative Messwerte. Ergebnisse sind verf\u00fcgbar, aber eine Folge mehrerer Materialien ist damit nicht m\u00f6glich. W\u00e4hlen Sie \u00fcber Lichtquelle \u00e4ndern eine Normlichtart oder laden Sie korrigierte Messwerte."),
    continuation_unavailable = c("Another material needs a nonnegative light spectrum.", "Ein weiteres Material ben\u00f6tigt ein nichtnegatives Lichtspektrum."),
    material = c("Material", "Material"),
    browse = c("Browse materials", "Materialien entdecken"),
    choose = c("Use this material", "Material verwenden"),
    library = c("Material library", "Materialbibliothek"),
    own_csv = c("My CSV file", "Eigene CSV-Datei"),
    library_intro = c("Open a category to compare curves and read about each material before choosing.", "\u00d6ffnen Sie eine Kategorie, um Kurven und Materialinformationen vor der Auswahl zu vergleichen."),
    colour_hint = c("Colour swatches approximate appearance under daylight D65. Gloss, texture and viewing angle are not represented.", "Die Farbfelder zeigen eine Ann\u00e4herung unter Tageslicht D65. Glanz, Struktur und Blickwinkel werden nicht dargestellt."),
    colour_label = c("Approximate colour under D65", "Ungef\u00e4hre Farbe unter D65"),
    search = c("Find a material", "Material suchen"),
    search_hint = c("Name, material, manufacturer or library\u2026", "Name, Material, Hersteller oder Bibliothek\u2026"),
    no_matches = c("No materials match. Try a shorter or different search.", "Keine passenden Materialien. Versuchen Sie einen k\u00fcrzeren oder anderen Suchbegriff."),
    complete = c("Full wavelength coverage", "Vollst\u00e4ndiger Wellenl\u00e4ngenbereich"),
    decisions = c("Needs wavelength choices", "Erg\u00e4nzung der Wellenl\u00e4ngen n\u00f6tig"),
    measurement = c("Measurement details & source", "Messdetails & Herkunft"),
    glazing_sample = c("Glazing sample from the fa\u00e7ade catalogue.", "Verglasungsprobe aus dem Fassadenkatalog."),
    documented_thickness = c("Documented thickness: %s mm.", "Dokumentierte Dicke: %s mm."),
    catalogue_designation = c("Original catalogue description", "Originale Katalogbeschreibung"),
    preview = c("Material preview", "Materialvorschau"),
    advanced = c("Material details & assumptions", "Materialdetails & Annahmen"),
    advanced_required = c("Confirm measurement assumptions", "Messannahmen best\u00e4tigen"),
    geometry_confirmation = c("Geometry for this result", "Geometrie f\u00fcr dieses Ergebnis"),
    measurement_choices = c("Measurement confirmation needed", "Messannahmen zu best\u00e4tigen"),
    missing = c("Complete the missing wavelengths", "Fehlende Wellenl\u00e4ngen erg\u00e4nzen"),
    missing_help = c("The calculation uses 380\u2013780 nm. This material does not cover the whole range. Choose an assumption for each missing end; the preview shows its effect. These are assumptions, not measured values.", "Die Berechnung verwendet 380\u2013780 nm. Dieses Material deckt den Bereich nicht vollst\u00e4ndig ab. W\u00e4hlen Sie f\u00fcr jeden fehlenden Randbereich eine Annahme; die Vorschau zeigt deren Wirkung. Dies sind Annahmen, keine Messwerte."),
    gap_heading = c("Review gaps between measurements", "L\u00fccken zwischen Messwerten pr\u00fcfen"),
    gap_help = c("There are gaps between the supplied measurements. Review the ranges below and confirm whether straight-line interpolation is acceptable. The preview distinguishes interpolated values from measurements.", "Zwischen den gelieferten Messwerten liegen L\u00fccken. Pr\u00fcfen Sie die Bereiche unten und best\u00e4tigen Sie, ob eine lineare Interpolation vertretbar ist. Die Vorschau unterscheidet interpolierte Werte von Messwerten."),
    short_end = c("Short wavelengths: %s", "Kurze Wellenl\u00e4ngen: %s"),
    long_end = c("Long wavelengths: %s", "Lange Wellenl\u00e4ngen: %s"),
    zero_transmission = c("0% \u00b7 no light passes through", "0 % \u00b7 kein Licht wird durchgelassen"),
    one_transmission = c("100% \u00b7 all light passes through", "100 % \u00b7 alles Licht wird durchgelassen"),
    zero_reflection = c("0% \u00b7 no light is reflected", "0 % \u00b7 kein Licht wird reflektiert"),
    one_reflection = c("100% \u00b7 all light is reflected", "100 % \u00b7 alles Licht wird reflektiert"),
    carry = c("Continue the nearest measured value", "N\u00e4chsten Messwert fortsetzen"),
    ready = c("Ready to calculate", "Bereit zur Berechnung"),
    needs_input = c("Complete the choices above to calculate", "Erg\u00e4nzen Sie die Angaben oben f\u00fcr die Berechnung"),
    calculate = c("Show material effect", "Materialwirkung anzeigen"),
    edit = c("Change material or settings", "Material oder Einstellungen \u00e4ndern"),
    "next" = c("Add another material", "Weiteres Material hinzuf\u00fcgen"),
    next_help = c("Optional: use this result as the light entering the next material. Your current result stays in the light path.", "Optional: Verwenden Sie dieses Ergebnis als einfallendes Licht f\u00fcr das n\u00e4chste Material. Das aktuelle Ergebnis bleibt im Lichtpfad erhalten."),
    use_output = c("Use as next light source", "Als n\u00e4chste Lichtquelle verwenden"),
    after_material = c("After %s", "Nach %s"),
    next_source_name = c("Name of next light source", "Name der n\u00e4chsten Lichtquelle"),
    rescale = c("Set a different receiver illuminance (advanced)", "Andere Beleuchtungsst\u00e4rke am Empf\u00e4nger festlegen (erweitert)"),
    preserve_lux = c("Calculated receiver illuminance: %s lx. This value is preserved unless you explicitly choose a different scenario above.", "Berechnete Beleuchtungsst\u00e4rke am Empf\u00e4nger: %s lx. Dieser Wert bleibt erhalten, sofern Sie oben kein anderes Szenario w\u00e4hlen."),
    empty = c("Your result will appear here", "Hier erscheint Ihr Ergebnis"),
    empty_help = c("Choose a light source and material in Set up, then show the material effect.", "W\u00e4hlen Sie unter Ausw\u00e4hlen eine Lichtquelle und ein Material und zeigen Sie dann die Materialwirkung an."),
    start_setup = c("Go to setup", "Zur Auswahl"),
    before = c("Incident illuminance", "Einfallende Beleuchtungsst\u00e4rke"),
    after = c("Receiver illuminance \u00b7 F = 1", "Beleuchtungsst\u00e4rke am Empf\u00e4nger \u00b7 F = 1"),
    stored = c("Result for", "Ergebnis f\u00fcr"),
    current_result = c("Current result: %s", "Aktuelles Ergebnis: %s"),
    options = c("Adjust the plot", "Diagramm anpassen"),
    close = c("Close", "Schlie\u00dfen"),
    back = c("Back to results", "Zur\u00fcck zum Ergebnis"),
    selected = c("Selected", "Ausgew\u00e4hlt")
  )
  values <- strings[[key]]
  if (is.null(values)) stop("Unknown workspace label: ", key)
  value <- values[[if (transmission_language_setting() == "Deutsch") 2L else 1L]]
  if (length(list(...))) sprintf(value, ...) else value
}

# Localized display only; the calculation and export values are unchanged.
material_workspace_metric <- function(value, defined = TRUE, digits = 3L) {
  label <- format_transmission_metric(value, defined, digits)
  if (transmission_language_setting() == "Deutsch") sub(".", ",", label, fixed = TRUE) else label
}

material_workspace_history_text <- function(key, ...) {
  workspace_keys <- c(history_tree = "path", history_root = "history_start",
    history_node = "history_step", history_parent = "history_from",
    history_new_root = "history_started", promoted_status = "history_continued",
    restored_status = "history_restored", archive_heading = "history_saved",
    archive_intro = "history_saved_help", aria_archived_results = "history_saved")
  if (key %in% names(workspace_keys)) material_workspace_text(workspace_keys[[key]], ...)
  else transmission_text(key, ...)
}

material_workspace_dependency <- function() {
  htmltools::htmlDependency(
    "spectran-material-workspace", "1.0.0",
    src = c(file = system.file("app/www", package = "Spectran")),
    stylesheet = "material-workspace.css", all_files = FALSE
  )
}

# The thumbnails use the supplied coefficient values on identical axes.
# Missing ends stay blank. No completion or new optical metric is implied.
material_thumbnail <- function(curve, label) {
  inside <- curve$wavelength_nm >= 380 & curve$wavelength_nm <= 780
  curve <- curve[inside, , drop = FALSE]
  x <- 26 + (curve$wavelength_nm - 380) / 400 * 252
  y <- 82 - curve$transmittance * 66
  points <- paste(sprintf("%.2f,%.2f", x, y), collapse = " ")
  htmltools::tags$svg(
    viewBox = "0 0 300 108", role = "img", `aria-label` = label,
    class = "material-thumbnail",
    htmltools::tags$title(label),
    htmltools::tags$path(d = "M26 16H278 M26 49H278 M26 82H278", stroke = "#e4e7e8", fill = "none"),
    htmltools::tags$polyline(points = points, stroke = "#333b43", fill = "none", `stroke-width` = "2.2"),
    htmltools::tags$text(x = "2", y = "19", "100%"),
    htmltools::tags$text(x = "8", y = "85", "0"),
    htmltools::tags$text(x = "26", y = "102", "380"),
    htmltools::tags$text(x = "144", y = "102", "580"),
    htmltools::tags$text(x = "252", y = "102", "780 nm")
  )
}

material_browser_records <- function(mode, query = "") {
  records <- filter_transmission_catalogue(
    material_catalogue_records_data(mode), collection = "all", query = query
  )
  if (nzchar(trimws(query))) return(records)
  records[stringr::str_order(paste(records$catalogue != "tub67600", records$category_en,
    !records$featured, records$catalogue_id), numeric = TRUE), , drop = FALSE]
}

material_browser_card <- function(record, ns, selected = NULL) {
  id <- record$catalogue_id[[1L]]
  item <- transmission_catalogue_record(id)
  name <- transmission_catalogue_localized_value(record, "display_name")
  description <- material_browser_description(record)
  library <- transmission_catalogue_localized_value(record, "catalogue_label")
  complete <- record$wavelength_min_nm <= 380 && record$wavelength_max_nm >= 780
  htmltools::tags$article(
    class = paste("material-browser-card", if (identical(id, selected)) "is-selected"),
    htmltools::tags$div(class = "material-card-library", library),
    htmltools::div(class = "material-card-title",
      htmltools::h4(name),
      if (identical(record$material_mode[[1L]], "reflection")) material_browser_colour(item$curve)),
    material_thumbnail(item$curve, name),
    htmltools::p(class = "material-card-description",
      if (!is.na(description)) description),
    htmltools::tags$div(class = "material-card-coverage",
      htmltools::span(paste0(record$wavelength_min_nm, "\u2013", record$wavelength_max_nm, " nm")),
      htmltools::span(class = if (complete) "material-badge" else "material-badge needs-choice",
        material_workspace_text(if (complete) "complete" else "decisions")),
      if (record$transmittance_type %in% c("internal", "unknown") ||
          identical(record$scattering[[1L]], "yes"))
        htmltools::span(class = "material-badge needs-choice", material_workspace_text("measurement_choices"))),
    shiny::actionButton(ns(paste0("pick_", make.names(id))),
      material_workspace_text(if (identical(id, selected)) "selected" else "choose"),
      icon = shiny::icon(if (identical(id, selected)) "check" else "plus"),
      class = "btn-default material-card-choose"),
    htmltools::tags$details(
      htmltools::tags$summary(material_workspace_text("measurement")),
      if (identical(record$catalogue[[1L]], "facade_windows")) htmltools::p(
        htmltools::strong(paste0(material_workspace_text("catalogue_designation"), ": ")),
        transmission_catalogue_localized_value(record, "source_description")),
      htmltools::p(transmission_catalogue_localized_value(record, "measurement_geometry")),
      htmltools::p(record$source_reference),
      htmltools::a(href = record$source_url, target = "_blank", rel = "noopener noreferrer",
        transmission_text("source")),
      htmltools::p(transmission_catalogue_localized_value(record, "licence"))
    )
  )
}

material_browser_colour <- function(curve) {
  prepared <- prepare_transmission_curve(
    data.frame(wavelength_nm = curve$wavelength_nm, value = curve$transmittance),
    scale = "fraction")
  if (!isTRUE(prepared$diagnostics$ready)) return(NULL)
  colour <- material_reflection_colour(prepared$completed)
  if (!isTRUE(colour$defined)) return(NULL)
  label <- paste(material_workspace_text("colour_label"), colour$hex)
  htmltools::span(class = "material-card-swatch", role = "img", `aria-label` = label,
    title = label, style = paste0("background-color:", colour$hex, ";"))
}

# Raw catalogue prose sometimes embeds missing thickness or metre values without
# units. The separate measurement details retain the supplied thickness in mm.
material_browser_description <- function(record) {
  description <- transmission_catalogue_localized_value(record, "source_description")
  if (identical(record$catalogue[[1L]], "facade_windows") &&
      !identical(record$catalogue_id[[1L]], "facade:JIS_Z8902")) {
    thickness <- record$thickness_mm[[1L]]
    return(paste(material_workspace_text("glazing_sample"),
      if (is.finite(thickness)) material_workspace_text("documented_thickness",
        material_workspace_metric(thickness))))
  }
  sub("; (thickness|Dicke) [^;]+", "", description)
}

material_browser_ui <- function(records, ns, selected, query = "") {
  if (!nrow(records)) return(htmltools::p(role = "status", material_workspace_text("no_matches")))
  category_column <- if (transmission_language_setting() == "Deutsch") "category_de" else "category_en"
  categories <- unique(records[[category_column]])
  htmltools::tagList(lapply(seq_along(categories), function(i) {
    category <- categories[[i]]
    group <- records[records[[category_column]] == category, , drop = FALSE]
    htmltools::tags$details(
      class = "material-category", open = if (nzchar(trimws(query))) NA else NULL,
      name = if (!nzchar(trimws(query))) ns("material-categories") else NULL,
      htmltools::tags$summary(htmltools::span(category),
        htmltools::span(class = "material-category-count", nrow(group))),
      htmltools::tags$div(class = "material-card-grid",
        lapply(seq_len(nrow(group)), function(j) material_browser_card(group[j, ], ns, selected)))
    )
  }))
}

material_source_ui <- function(id) shiny::uiOutput(shiny::NS(id, "summary"))

# Import has its own draft state; the parent receives only confirmed imports.
material_source_server <- function(id, current, history, on_import, automatic = shiny::reactive(FALSE)) {
  stopifnot(shiny::is.reactive(current), shiny::is.reactive(history), is.function(on_import))
  shiny::moduleServer(id, function(input, output, session) {
    draft <- importServer("picker", transmission_history = history, workspace = TRUE)
    output$summary <- shiny::renderUI({
      source <- current()
      name <- if (is.null(source)) material_workspace_text("source") else source$name
      lux <- if (is.null(source)) NA_real_ else material_photopic_lux(source$spectrum)
      htmltools::tags$section(class = "material-source-card",
        htmltools::div(class = "material-source-icon", shiny::icon("sun")),
        htmltools::div(class = "material-source-copy",
          htmltools::p(class = "material-eyebrow", material_workspace_text("source")),
          htmltools::h3(name),
          htmltools::p(class = "material-source-meta",
            htmltools::strong(paste(material_workspace_metric(lux), "lx")),
            " \u00b7 ", material_workspace_text(if (isTRUE(automatic()) &&
              identical(source$node_id, "node-1")) "automatic" else "current")),
          if (!is.null(source) && any(source$spectrum$Bestrahlungsstaerke < 0))
            htmltools::p(class = "material-source-warning", material_workspace_text("negative_source"))),
        shiny::actionButton(session$ns("change"), material_workspace_text("change_source"),
          icon = shiny::icon("pen"), class = "btn-default"))
    })
    shiny::observeEvent(input$change, {
      shiny::showModal(shiny::modalDialog(
        title = material_workspace_text("change_source"),
        htmltools::div(class = "material-source-picker",
          htmltools::p(material_workspace_text("source_help")), material_source_picker_ui(session$ns("picker"))),
        size = "l", easyClose = TRUE,
        footer = shiny::modalButton(material_workspace_text("close"))
      ))
    })
    shiny::observeEvent(draft$revision, {
      shiny::req(draft$revision > 0, draft$Spectrum)
      on_import(list(spectrum = draft$Spectrum, name = draft$Name,
        origin = draft$Origin, provenance = draft$provenance, destination = draft$Destination))
      shiny::removeModal()
      shinyalert::closeAlert()
    }, ignoreInit = TRUE)
    invisible(NULL)
  })
}

material_workspace_ui <- function(id, source_ui = NULL) {
  ns <- shiny::NS(id)
  t <- material_workspace_text
  # Reuse the existing presentation stylesheet; only the new layout overrides it.
  common_style <- transmissionUI(id, layout = "review")$children[[1L]]
  ui <- htmltools::tags$div(
    class = "spectran-transmission-module material-workspace",
    common_style, shinyjs::useShinyjs(),
    htmltools::tags$header(class = "material-workspace-header",
      htmltools::p(class = "material-eyebrow", "SPECTRAN / LiTG"),
      htmltools::h2(t("title")), htmltools::p(t("intro"))),
    source_ui,
    htmltools::div(class = "material-workspace-flow",
      htmltools::tags$nav(class = "material-workspace-nav", `aria-label` = t("navigation"),
        shiny::radioButtons(ns("section_nav"),
          label = htmltools::span(class = "sr-only", t("navigation")),
          choices = stats::setNames(c("spectrum", "results", "history", "export"),
            c(t("setup"), t("results"), t("path"), t("downloads"))),
          selected = "spectrum", inline = TRUE, width = "100%")),
      transmission_tabset_panel(id = ns("section"), type = "hidden",
        shiny::tabPanel(t("setup"), value = "spectrum",
          htmltools::div(class = "material-setup-grid",
            htmltools::tags$section(class = "material-setup-controls",
              htmltools::h3(t("material")),
              shiny::radioButtons(ns("material_mode"), label = NULL,
                choices = stats::setNames(c("transmission", "reflection"),
                  c(material_text("transmission"), material_text("reflection"))),
                selected = "transmission", inline = TRUE),
              shiny::radioButtons(ns("input_source"), label = NULL,
                choices = stats::setNames(c("catalogue", "upload"), c(t("library"), t("own_csv"))),
                selected = "catalogue", inline = TRUE),
              shiny::conditionalPanel(sprintf("input['%s'] === 'catalogue'", ns("input_source")),
                shiny::uiOutput(ns("workspace_selection")),
                shiny::actionButton(ns("browse_material"), t("browse"),
                  icon = shiny::icon("layer-group"), class = "btn-primary material-browse-button")),
              shiny::conditionalPanel(sprintf("input['%s'] === 'upload'", ns("input_source")),
                shiny::fileInput(ns("filter_file"), t("own_csv"), accept = c(".csv", "text/csv", "text/plain"),
                  width = "100%", buttonLabel = transmission_text("file_browse"),
                  placeholder = transmission_text("file_none")),
                shiny::uiOutput(ns("file_transport_status")),
                shiny::downloadButton(ns("download_template"), transmission_text("template_download")),
                htmltools::tags$details(class = "material-disclosure",
                  htmltools::tags$summary(transmission_text("csv_settings")),
                  spectral_csv_settingsUI(ns("csv"), labels = transmission_csv_settings_labels()))),
              shiny::uiOutput(ns("coverage_controls")),
              shiny::uiOutput(ns("material_details")),
              htmltools::tags$details(class = "material-disclosure material-provenance",
                htmltools::tags$summary(t("measurement")), shiny::uiOutput(ns("catalogue_info")))),
            htmltools::tags$section(class = "material-preview-card",
              htmltools::h3(t("preview")), shiny::uiOutput(ns("preview_outputs")),
              htmltools::tags$details(class = "material-disclosure",
                htmltools::tags$summary(t("model")), shiny::uiOutput(ns("material_model"))),
              htmltools::p(class = "material-model-brief", t("model_brief")))),
          htmltools::div(class = "material-calculate-bar",
            shiny::uiOutput(ns("readiness")),
            shiny::actionButton(ns("spectrum_forward"), t("calculate"),
              icon = shiny::icon("arrow-right"), class = "btn-primary btn-lg"))),
        shiny::tabPanel(t("results"), value = "results",
          shiny::uiOutput(ns("workspace_result_context")),
          transmissionApplyControlsUI(ns("apply")), transmissionApplyResultsUI(ns("apply")),
          shiny::uiOutput(ns("workspace_result_actions"))),
        shiny::tabPanel(t("path"), value = "history",
          transmissionHistoryDetailsUI(ns("history"), workspace = TRUE)),
        shiny::tabPanel(t("downloads"), value = "export",
          transmissionHistoryExportUI(ns("history"))),
        shiny::tabPanel(t("next"), value = "promotion",
          htmltools::div(class = "material-next-step",
            htmltools::h3(t("next")), htmltools::p(t("next_help")),
            transmissionHistoryControlsUI(ns("history"), compact = TRUE, workspace = TRUE),
            shiny::actionButton(ns("promotion_back"), t("back"), icon = shiny::icon("arrow-left")))),
        selected = "spectrum"))
  )
  htmltools::attachDependencies(ui, material_workspace_dependency())
}

material_workspace_result_summary <- function(snapshot, ns = identity) {
  t <- material_workspace_text
  if (is.null(snapshot)) return(htmltools::div(class = "material-empty",
    shiny::icon("chart-line"), htmltools::h3(t("empty")), htmltools::p(t("empty_help")),
    shiny::actionButton(ns("results_back"), t("start_setup"), class = "btn-primary")))
  photopic <- snapshot$active_metrics[snapshot$active_metrics$metric_id == "photopic_illuminance", ]
  htmltools::div(class = "material-result-summary",
    htmltools::div(class = "material-result-title", htmltools::p(class = "material-eyebrow", t("stored")),
      htmltools::h3(snapshot$metadata$filter_name), htmltools::p(snapshot$incident_name)),
    htmltools::div(class = "material-result-value", htmltools::span(t("before")),
      htmltools::strong(paste(material_workspace_metric(photopic$incident_value), "lx"))),
    htmltools::div(class = "material-result-value", htmltools::span(t("after")),
      htmltools::strong(paste(material_workspace_metric(photopic$transmitted_value), "lx"))))
}

# Retained isolated app for the complete workspace, including source changes.
material_workspace_app <- function(language = "English") {
  the$language <- language
  the$palette <- "Lang"
  shiny::addResourcePath("extr", system.file("app/www", package = "Spectran"))
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(disable = TRUE),
    shinydashboard::dashboardSidebar(disable = TRUE),
    shinydashboard::dashboardBody(
    htmltools::tags$link(rel = "stylesheet", href = "extr/style.css"),
    htmltools::p(class = "spectran-review-build", getOption("Spectran.review_build_id", "Material workspace showcase")),
    transmissionUI("material", layout = "workspace", source_ui = material_source_ui("light"))))
  server <- function(input, output, session) {
    Spectrum <- shiny::reactiveValues()
    initialize_spectran_spectrum_state(Spectrum)
    shiny::isolate(activate_spectran_default_daylight(Spectrum))
    material <- transmissionServer("material", workspace = TRUE,
      incident_spectrum = shiny::reactive(Spectrum$Spectrum),
      incident_name = shiny::reactive(Spectrum$Name),
      active_state = shiny::reactive(spectran_transmission_active_state(Spectrum)))
    material_source_server("light", current = shiny::reactive(spectran_transmission_active_state(Spectrum)),
      history = material$history, automatic = shiny::reactive(Spectrum$automatic_source),
      on_import = function(request) activate_spectran_spectrum(Spectrum,
        request$spectrum, request$name, request$origin, "import", "node-1", request$provenance,
        destination = request$destination))
    shiny::observeEvent(material$promotion_event(), {
      activate_spectran_transmission_event(Spectrum, material$promotion_event())
    }, ignoreInit = TRUE)
    shiny::observeEvent(material$restore_event(), {
      activate_spectran_transmission_event(Spectrum, material$restore_event())
    }, ignoreInit = TRUE)
  }
  shiny::shinyApp(ui, server)
}
