# Task-oriented presentation of the existing material calculation.

material_workspace_text <- function(key, ...) {
  strings <- list(
    title = c("Transmission & reflection", "Transmission & Reflexion"),
    intro = c("Explore how a material changes light.", "Entdecken Sie, wie ein Material Licht ver\u00e4ndert."),
    loading_title = c("Preparing materials", "Materialien werden vorbereitet"),
    loading = c("Loading the material library and preparing the workspace. This window closes automatically when everything is ready.", "Die Materialbibliothek wird geladen und der Arbeitsbereich vorbereitet. Dieses Fenster schlie\u00dft sich automatisch, sobald alles bereit ist."),
    loading_error = c("The material workspace could not be loaded. Open another page and return to try again.", "Der Materialbereich konnte nicht geladen werden. Wechseln Sie zu einer anderen Seite und kehren Sie zur\u00fcck, um es erneut zu versuchen."),
    type_label = c("Type:", "Typ:"),
    source_label = c("Source:", "Quelle:"),
    original_source = c("Original source (as cited in the collection)", "Originalquelle (laut Sammlung)"),
    upload_provenance = c("This material comes from your CSV file. Measurement details are documented only when you enter them in Material details.", "Dieses Material stammt aus Ihrer CSV-Datei. Messangaben werden nur dokumentiert, wenn Sie diese unter Materialdetails eintragen."),
    facade_products = c("Fa\u00e7ade glazing products", "Verglasungsprodukte f\u00fcr Fassaden"),
    spectral_colours = c("Show spectral colours", "Spektralfarben anzeigen"),
    spectral_colours_help = c("Spectral colours indicate wavelength, not the material's appearance.", "Spektralfarben kennzeichnen die Wellenl\u00e4nge, nicht die Materialfarbe."),
    light_level = c("Light level", "Lichtniveau"),
    illuminance = c("Illuminance", "Beleuchtungsst\u00e4rke"),
    melanopic_edi = c("Melanopic EDI", "Melanopische EDI"),
    target_level = c("Target value (lx)", "Zielwert (lx)"),
    level_help = c("Both values are in lx. Melanopic EDI weights the spectrum for melanopsin; illuminance weights it for photopic vision.", "Beide Werte werden in lx angegeben. Die melanopische EDI bewertet das Spektrum f\u00fcr Melanopsin, die Beleuchtungsst\u00e4rke f\u00fcr das photopische Sehen."),
    level_invalid = c("Enter a finite light level of zero or greater.", "Geben Sie ein endliches Lichtniveau von null oder gr\u00f6\u00dfer ein."),
    level_zero = c("This spectrum has no response for the selected quantity and cannot reach a positive target. Choose another spectrum or use zero.", "Dieses Spektrum hat f\u00fcr die gew\u00e4hlte Gr\u00f6\u00dfe keine Wirkung und kann keinen positiven Zielwert erreichen. W\u00e4hlen Sie ein anderes Spektrum oder null."),
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
    saved_result_status = c("This result is saved in the light path. A new calculation uses %s as its incident light.", "Dieses Ergebnis ist im Lichtpfad gespeichert. Eine neue Berechnung verwendet %s als einfallendes Licht."),
    saved_result_intro = c("This saved calculation is available for inspection and download.", "Diese gespeicherte Berechnung steht zur Ansicht und zum Download bereit."),
    history_saved_status = c("%s is saved in the light path as step %s. The active light source is unchanged.", "%s ist als Schritt %s im Lichtpfad gespeichert. Die aktive Lichtquelle bleibt unver\u00e4ndert."),
    path_sources = c("From light path", "Aus dem Lichtpfad"),
    path_source_help = c("Choose a saved step as the incident light. All saved steps and alternative paths are kept.", "W\u00e4hlen Sie einen gespeicherten Schritt als einfallendes Licht. Alle gespeicherten Schritte und alternativen Pfade bleiben erhalten."),
    path_source_select = c("Saved light source", "Gespeicherte Lichtquelle"),
    source_preview = c("Selected light source", "Ausgew\u00e4hlte Lichtquelle"),
    path_plot = c("Spectra along this light path", "Spektren entlang dieses Lichtpfads"),
    path_plot_help = c("Start: pale spectral fill. Selected step: full spectral fill and solid outline. The key identifies the intermediate steps. Only this path is shown.", "Start: helle Spektralfarben. Ausgew\u00e4hlter Schritt: volle Spektralfarben und durchgezogene Linie. Die Legende kennzeichnet die Zwischenschritte. Nur dieser Pfad wird gezeigt."),
    path_plot_readability = c("Longer paths can be harder to compare on small screens. Select an earlier step to view part of the path, or download the PNG for a larger view. All steps up to the selected result remain included.", "L\u00e4ngere Lichtpfade lassen sich auf kleinen Bildschirmen schwerer vergleichen. W\u00e4hlen Sie einen fr\u00fcheren Schritt f\u00fcr eine Teilansicht oder laden Sie das PNG f\u00fcr eine gr\u00f6\u00dfere Ansicht herunter. Alle Schritte bis zum ausgew\u00e4hlten Ergebnis bleiben enthalten."),
    path_plot_adjusted = c("The plot shows the saved light levels, including your adjustments. It therefore includes more than the material effect alone.", "Das Diagramm zeigt die gespeicherten Lichtniveaus einschlie\u00dflich Ihrer Anpassungen. Es enth\u00e4lt daher mehr als die reine Materialwirkung."),
    path_plot_scenario = c("Receiver scenario F = 1 (idealised geometry)", "Empf\u00e4ngerszenario F = 1 (idealisierte Geometrie)"),
    path_plot_rescaled = c("Saved absolute light levels, including explicit light-level adjustments", "Gespeicherte absolute Lichtniveaus, einschlie\u00dflich ausdr\u00fccklicher Anpassungen"),
    path_plot_download = c("Light path plot (PNG)", "Lichtpfad-Diagramm (PNG)"),
    path_data_download = c("Light path spectra (CSV)", "Lichtpfad-Spektren (CSV)"),
    path_export_hint = c("Save the current result to enable its light path plot download.", "Speichern Sie das aktuelle Ergebnis, um sein Lichtpfad-Diagramm herunterzuladen."),
    path_empty = c("Save a material result to compare the steps along its light path.", "Speichern Sie ein Materialergebnis, um die Schritte entlang seines Lichtpfads zu vergleichen."),
    path_start = c("Start", "Start"),
    path_selected = c("Selected step", "Ausgew\u00e4hlter Schritt"),
    path_adjustment = c("light level adjusted", "Lichtniveau angepasst"),
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
    model_brief = c("Receiver values assume F = 1, an idealised geometrical coupling. Actual geometry can reduce the received light. No room geometry is calculated.", "Empf\u00e4ngerwerte setzen F = 1 voraus, eine idealisierte geometrische Kopplung. Die tats\u00e4chliche Geometrie kann das empfangene Licht verringern. Eine Raumgeometrie wird nicht berechnet."),
    source_help = c("Choose a bundled example, upload a spectrum, or create your own.", "W\u00e4hlen Sie ein Beispiel, laden Sie ein Spektrum oder erstellen Sie ein eigenes."),
    source_reset_heading = c("Replace the light path?", "Lichtpfad ersetzen?"),
    source_reset_action = c("Use source and replace light path", "Quelle verwenden und Lichtpfad ersetzen"),
    source_reset_one = c("Using %s will remove the saved material result and its light path from this session.", "Wenn Sie %s verwenden, werden das gespeicherte Materialergebnis und sein Lichtpfad aus dieser Sitzung entfernt."),
    source_reset_many = c("Using %s will remove all %d saved material results and their branches from this session.", "Wenn Sie %s verwenden, werden alle %d gespeicherten Materialergebnisse und ihre Zweige aus dieser Sitzung entfernt."),
    source_reset_irreversible = c("This cannot be undone. The selected source will start a new light path.", "Dies kann nicht r\u00fcckg\u00e4ngig gemacht werden. Mit der ausgew\u00e4hlten Quelle beginnt ein neuer Lichtpfad."),
    source_reset_done_one = c("A new light path starts with %s. The previous saved material result was removed.", "Ein neuer Lichtpfad beginnt mit %s. Das zuvor gespeicherte Materialergebnis wurde entfernt."),
    source_reset_done_many = c("A new light path starts with %s. The previous %d saved material results and their branches were removed.", "Ein neuer Lichtpfad beginnt mit %s. Die zuvor gespeicherten %d Materialergebnisse und ihre Zweige wurden entfernt."),
    source_reset_cancelled = c("Source change cancelled. %s was not used. The active source and saved light path are still available.", "Quellenwechsel abgebrochen. %s wurde nicht verwendet. Die aktive Quelle und der gespeicherte Lichtpfad sind weiterhin verf\u00fcgbar."),
    automatic = c("Starting example \u00b7 daylight D65", "Startbeispiel \u00b7 Tageslicht D65"),
    current = c("Active source for the next calculation", "Aktive Quelle f\u00fcr die n\u00e4chste Berechnung"),
    negative_source = c("%d negative light measurements were set to 0 before calculation. This can occur with measurement noise. The original values and correction are recorded in the audit export. You can save the result and continue with more materials.", "%d negative Lichtmesswerte wurden vor der Berechnung auf 0 gesetzt. Solche Werte k\u00f6nnen durch Messrauschen entstehen. Ursprungswerte und Korrektur sind im Pr\u00fcfexport dokumentiert. Sie k\u00f6nnen das Ergebnis speichern und weitere Materialien verwenden."),
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
    optional_measurements = c("Optional measurement details", "Freiwillige Messangaben"),
    confirmation_required = c("Required before calculation", "Vor der Berechnung erforderlich"),
    confirmation_done = c("Confirmed", "Best\u00e4tigt"),
    type_confirmation = c("Confirm the measurement type", "Messart best\u00e4tigen"),
    scattering_confirmation = c("Confirm the scattering measurement", "Messung der streuenden Probe best\u00e4tigen"),
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
    "next" = c("Save result / add material", "Ergebnis speichern / Material hinzuf\u00fcgen"),
    next_help = c("Save this result in the light path, or save it and use its spectrum for another material. Saved results remain available for this session.", "Speichern Sie dieses Ergebnis im Lichtpfad, oder speichern Sie es und verwenden Sie sein Spektrum f\u00fcr ein weiteres Material. Gespeicherte Ergebnisse bleiben in dieser Sitzung verf\u00fcgbar."),
    save_only = c("Save to light path", "Im Lichtpfad speichern"),
    save_name_required = c("Enter a name for the saved result.", "Geben Sie einen Namen f\u00fcr das gespeicherte Ergebnis ein."),
    save_unavailable = c("Calculate a current material result before saving. Already saved results are available in Light path.", "Berechnen Sie vor dem Speichern ein aktuelles Materialergebnis. Bereits gespeicherte Ergebnisse finden Sie im Lichtpfad."),
    include_incident = c("Include incident light name", "Namen des einfallenden Lichts aufnehmen"),
    use_output = c("Save and continue", "Speichern und fortfahren"),
    after_material = c("After %s", "Nach %s"),
    next_source_name = c("Name of saved result", "Name des gespeicherten Ergebnisses"),
    rescale = c("Set a different receiver light level (advanced)", "Anderes Lichtniveau am Empf\u00e4nger festlegen (erweitert)"),
    rescale_help = c("Scales the saved spectrum to the selected light level. This creates a new receiver scenario and also affects subsequent material steps.", "Skaliert das gespeicherte Spektrum auf das gew\u00e4hlte Lichtniveau. Dadurch entsteht ein neues Empf\u00e4ngerszenario, das auch nachfolgende Materialschritte beeinflusst."),
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
    archive_intro = "history_saved_help", aria_archived_results = "history_saved",
    results_archived = "saved_result_status", frozen_result_heading = "history_saved",
    frozen_result_intro = "saved_result_intro", aria_applied_archived = "history_saved",
    import_confirm_heading = "source_reset_heading", import_confirm_action = "source_reset_action",
    import_confirm_message_one = "source_reset_one", import_confirm_message_many = "source_reset_many",
    import_confirm_irreversible = "source_reset_irreversible", import_direct = "history_started",
    import_completed_one = "source_reset_done_one", import_completed_many = "source_reset_done_many",
    import_cancelled = "source_reset_cancelled")
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
material_thumbnail <- function(curve, label, spectral_colours = TRUE, gradient_id = "material-spectrum") {
  inside <- curve$wavelength_nm >= 380 & curve$wavelength_nm <= 780
  curve <- curve[inside, , drop = FALSE]
  x <- 26 + (curve$wavelength_nm - 380) / 400 * 252
  y <- 82 - curve$transmittance * 66
  points <- paste(sprintf("%.2f,%.2f", x, y), collapse = " ")
  htmltools::tags$svg(
    viewBox = "0 0 300 108", role = "img", `aria-label` = label,
    class = "material-thumbnail",
    htmltools::tags$title(label),
    if (isTRUE(spectral_colours)) {
      palette <- transmission_spectral_palette()
      palette <- palette[unique(round(seq(1, length(palette), length.out = 21L)))]
      htmltools::tagList(
        htmltools::tags$defs(htmltools::tags$linearGradient(
          id = gradient_id, gradientUnits = "userSpaceOnUse", x1 = "26", x2 = "278", y1 = "0", y2 = "0",
          lapply(seq_along(palette), function(i) htmltools::tags$stop(
            offset = paste0(100 * (i - 1) / (length(palette) - 1), "%"), `stop-color` = palette[[i]])))),
        htmltools::tags$polygon(class = "material-thumbnail-fill", points = paste(sprintf("%.2f,82", x[[1L]]), points,
          sprintf("%.2f,82", x[[length(x)]])), fill = paste0("url(#", gradient_id, ")"), opacity = ".85"))
    },
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

material_browser_card <- function(record, ns, selected = NULL, spectral_colours = TRUE) {
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
    material_thumbnail(item$curve, name, spectral_colours, ns(paste0("spectrum_", make.names(id)))),
    htmltools::p(class = "material-card-description",
      if (!is.na(description)) description),
    material_catalogue_source_ui(record),
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
        material_catalogue_description(record)),
      material_optional_metadata_ui(transmission_text("catalogue_geometry"),
        transmission_catalogue_localized_value(record, "measurement_geometry")),
      material_measurement_metadata_ui(record),
      material_catalogue_original_source_ui(record),
      material_optional_metadata_ui(transmission_text("licence"),
        transmission_catalogue_localized_value(record, "licence"))
    )
  )
}

material_confirmation_ui <- function(title, control) {
  htmltools::tags$section(class = "material-confirmation",
    htmltools::h4(title),
    htmltools::div(class = "material-confirmation-status", role = "status",
      htmltools::span(class = "material-confirmation-pending",
        shiny::icon("circle-exclamation"), material_workspace_text("confirmation_required")),
      htmltools::span(class = "material-confirmation-done",
        shiny::icon("check-circle"), material_workspace_text("confirmation_done"))),
    control)
}

material_optional_metadata_ui <- function(label, value) {
  if (is.null(value) || length(value) != 1L || is.na(value) ||
      trimws(value) %in% c("", "NA", "N/A", "NaN")) return(NULL)
  htmltools::p(htmltools::strong(paste0(label, ": ")), value)
}

# Name the collection beside its link, keeping original per-material references
# distinct. Raw catalogue metadata remain unchanged for the audit export.
material_catalogue_source_ui <- function(record) {
  label <- switch(record$catalogue[[1L]],
    spitschan2019 = "Spitschan et al. (2019)",
    tub67600 = "Rudawski et al. (2022)",
    facade_windows = "CIE / photobiologyFilters",
    transmission_catalogue_localized_value(record, "catalogue_label"))
  htmltools::p(class = "material-catalogue-source",
    htmltools::strong(paste0(transmission_text("source"), ": ")),
    htmltools::a(href = record$source_url, target = "_blank",
      rel = "noopener noreferrer", label))
}

material_catalogue_original_source_ui <- function(record) {
  if (!identical(record$catalogue[[1L]], "spitschan2019")) return(NULL)
  material_optional_metadata_ui(material_workspace_text("original_source"), record$source_reference)
}

material_catalogue_description <- function(record) {
  # Thickness has a separate field with units; source prose sometimes says NA
  # or gives metres without a unit. Keep the product designation and maker.
  sub("; (thickness|Dicke) [^;]+", "",
    transmission_catalogue_localized_value(record, "source_description"))
}

material_measurement_metadata_ui <- function(record) {
  htmltools::tagList(lapply(c("measurement_instrument", "relative_measurement_error"), function(field) {
    material_optional_metadata_ui(material_text(field),
      transmission_catalogue_localized_value(record, field))
  }))
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
  if (identical(record$catalogue[[1L]], "facade_windows") &&
      !identical(record$catalogue_id[[1L]], "facade:JIS_Z8902")) {
    thickness <- record$thickness_mm[[1L]]
    return(paste(material_workspace_text("glazing_sample"),
      if (is.finite(thickness)) material_workspace_text("documented_thickness",
        material_workspace_metric(thickness))))
  }
  material_catalogue_description(record)
}

material_browser_ui <- function(records, ns, selected, query = "", spectral_colours = TRUE) {
  if (!nrow(records)) return(htmltools::p(role = "status", material_workspace_text("no_matches")))
  category_column <- if (transmission_language_setting() == "Deutsch") "category_de" else "category_en"
  categories <- unique(records[[category_column]])
  htmltools::tagList(lapply(seq_along(categories), function(i) {
    category <- categories[[i]]
    group <- records[records[[category_column]] == category, , drop = FALSE]
    htmltools::tags$details(
      class = "material-category", open = if (nzchar(trimws(query))) NA else NULL,
      name = if (!nzchar(trimws(query))) ns("material-categories") else NULL,
      htmltools::tags$summary(htmltools::span(if (all(group$catalogue == "facade_windows"))
        material_workspace_text("facade_products") else category),
        htmltools::span(class = "material-category-count", nrow(group))),
      htmltools::tags$div(class = "material-card-grid",
        lapply(seq_len(nrow(group)), function(j) material_browser_card(group[j, ], ns, selected, spectral_colours)))
    )
  }))
}

material_source_icon <- function() {
  htmltools::tags$svg(
    viewBox = "-44 -44 88 88", width = "44", height = "44",
    fill = "none", stroke = "currentColor", `stroke-width` = "3.3",
    `stroke-linecap` = "round", `stroke-linejoin` = "round",
    `aria-hidden` = "true", focusable = "false",
    htmltools::tags$circle(r = "10.5"),
    lapply(seq(0, 315, by = 45), function(angle) {
      htmltools::tags$path(d = "M0 -18 V-25",
        transform = paste0("rotate(", angle, ")"))
    })
  )
}

material_source_ui <- function(id) shiny::uiOutput(shiny::NS(id, "summary"))

# Import has its own draft state; the parent receives only confirmed imports.
material_source_server <- function(id, current, history, on_import, automatic = shiny::reactive(FALSE), on_restore = NULL) {
  stopifnot(shiny::is.reactive(current), shiny::is.reactive(history), is.function(on_import))
  shiny::moduleServer(id, function(input, output, session) {
    draft <- importServer("picker", transmission_history = history, workspace = TRUE)
    output$summary <- shiny::renderUI({
      source <- current()
      name <- if (is.null(source)) material_workspace_text("source") else source$name
      lux <- if (is.null(source)) NA_real_ else material_photopic_lux(source$spectrum)
      edi <- if (is.null(source)) NA_real_ else material_light_level(source$spectrum, "melanopic")
      htmltools::tags$section(class = "material-source-card",
        htmltools::div(class = "material-source-icon", material_source_icon()),
        htmltools::div(class = "material-source-copy",
          htmltools::p(class = "material-eyebrow", material_workspace_text("source")),
          htmltools::h3(name),
          htmltools::p(class = "material-source-meta",
            htmltools::strong(paste(material_workspace_metric(lux), "lx")),
            " \u00b7 ", material_workspace_text("melanopic_edi"), ": ", material_workspace_metric(edi), " lx",
            " \u00b7 ", material_workspace_text(if (isTRUE(automatic()) &&
              identical(source$node_id, "node-1")) "automatic" else "current")),
          if (!is.null(source)) material_source_preprocessing_note(source$provenance)),
        shiny::actionButton(session$ns("change"), material_workspace_text("change_source"),
          icon = shiny::icon("pen"), class = "btn-default"))
    })
    shiny::observeEvent(input$change, {
      saved <- history()
      path_ui <- if (!is.null(saved) && length(saved$nodes) > 1L && is.function(on_restore)) {
        choices <- vapply(saved$nodes, function(node) paste0("N", node$sequence_id, " \u00b7 ", node$name), character(1))
        htmltools::tagList(
          htmltools::p(material_workspace_text("path_source_help")),
          shiny::selectInput(session$ns("path_source"), material_workspace_text("path_source_select"),
            choices = stats::setNames(names(choices), choices), selected = saved$active_node_id, width = "100%"),
          shiny::uiOutput(session$ns("path_source_summary")),
          shiny::plotOutput(session$ns("path_source_plot"), height = "auto"),
          shiny::actionButton(session$ns("use_path_source"), material_workspace_text("use_source"),
            icon = shiny::icon("check"), class = "btn-primary"))
      }
      shiny::showModal(shiny::modalDialog(
        title = material_workspace_text("change_source"),
        htmltools::div(class = "material-source-picker",
          lang = if (transmission_language_setting() == "Deutsch") "de" else "en",
          htmltools::p(material_workspace_text("source_help")), material_source_picker_ui(session$ns("picker"), path_ui)),
        size = "l", easyClose = TRUE,
        footer = shiny::modalButton(material_workspace_text("close"))
      ))
    })
    selected_path_source <- shiny::reactive({
      saved <- history()
      shiny::req(saved, input$path_source, input$path_source %in% names(saved$nodes))
      saved$nodes[[input$path_source]]
    })
    output$path_source_summary <- shiny::renderUI({
      node <- selected_path_source()
      htmltools::p(htmltools::strong(node$name), htmltools::br(),
        material_workspace_text("illuminance"), ": ", material_workspace_metric(material_photopic_lux(node$spectrum)), " lx \u00b7 ",
        material_workspace_text("melanopic_edi"), ": ", material_workspace_metric(material_light_level(node$spectrum, "melanopic")), " lx")
    })
    path_source_width <- shiny::reactive(session$clientData[[paste0("output_", session$ns("path_source_plot"), "_width")]] %||% 800)
    output$path_source_plot <- shiny::renderPlot({
      node <- selected_path_source()
      source_history <- new_transmission_history(new_transmission_active_spectrum(
        node$spectrum, node$name, "Light path preview", 0L, "import", node$node_id))
      width <- path_source_width()
      material_path_plot(source_history, font_size = if (width < 500) 10 else 12,
        label_width = material_path_label_width(width), width_px = width)
    }, height = function() {
      wrap <- max(10L, material_path_label_width(path_source_width()) - 8L)
      330 + 18 * ceiling(nchar(selected_path_source()$name) / wrap)
    }, alt = function() selected_path_source()$name, res = 96)
    shiny::observeEvent(input$use_path_source, {
      node <- selected_path_source()
      shiny::req(is.function(on_restore))
      on_restore(node$node_id)
      shiny::removeModal()
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

material_workspace_ui <- function(id, source_ui = NULL, help_links = list()) {
  ns <- shiny::NS(id)
  t <- material_workspace_text
  # Reuse the existing presentation stylesheet; only the new layout overrides it.
  common_style <- transmissionUI(id, layout = "review")$children[[1L]]
  ui <- htmltools::tags$div(
    class = "spectran-transmission-module material-workspace",
    lang = if (transmission_language_setting() == "Deutsch") "de" else "en",
    common_style, shinyjs::useShinyjs(),
    htmltools::tags$header(class = "material-workspace-header",
      htmltools::p(class = "material-eyebrow material-brand", "LiTG Spectran"),
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
              help_links$from_material,
              htmltools::div(class = "material-selector-row",
                shiny::radioButtons(ns("material_mode"), label = t("type_label"),
                  choiceNames = list(htmltools::tagList(shiny::icon("arrow-right"), material_text("transmission")),
                    htmltools::tagList(shiny::icon("reply"), material_text("reflection"))),
                  choiceValues = c("transmission", "reflection"),
                  selected = "transmission", inline = TRUE, width = "100%")),
              htmltools::div(class = "material-selector-row",
                shiny::radioButtons(ns("input_source"), label = t("source_label"),
                  choiceNames = list(htmltools::tagList(shiny::icon("layer-group"), t("library")),
                    htmltools::tagList(shiny::icon("file-csv"), t("own_csv"))),
                  choiceValues = c("catalogue", "upload"),
                  selected = "catalogue", inline = TRUE, width = "100%")),
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
              shiny::uiOutput(ns("type_acknowledgement")),
              shiny::uiOutput(ns("scattering_acknowledgement")),
              htmltools::tags$details(class = "material-disclosure material-provenance",
                htmltools::tags$summary(t("measurement")), shiny::uiOutput(ns("catalogue_info")))),
            htmltools::tags$section(class = "material-preview-card",
              htmltools::h3(t("preview")), help_links$from_preview,
              shiny::uiOutput(ns("preview_outputs")),
              htmltools::tags$details(class = "material-disclosure",
                htmltools::tags$summary(t("model")), shiny::uiOutput(ns("material_model"))),
              htmltools::p(class = "material-model-brief", t("model_brief")),
              help_links$from_setup_model)),
          htmltools::div(class = "material-calculate-bar",
            shiny::uiOutput(ns("readiness")),
            shiny::actionButton(ns("spectrum_forward"), t("calculate"),
              icon = shiny::icon("arrow-right"), class = "btn-primary btn-lg"))),
        shiny::tabPanel(t("results"), value = "results",
          shiny::uiOutput(ns("workspace_result_context")),
          htmltools::p(class = "material-model-brief material-result-model", t("model_brief")),
          help_links$from_result_model,
          transmissionApplyControlsUI(ns("apply")), transmissionApplyResultsUI(ns("apply")),
          shiny::uiOutput(ns("workspace_result_actions"))),
        shiny::tabPanel(t("path"), value = "history",
          transmissionHistoryDetailsUI(ns("history"), workspace = TRUE, help_ui = help_links$from_path)),
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
        destination = request$destination), on_restore = material$restore_node)
    shiny::observeEvent(material$promotion_event(), {
      activate_spectran_transmission_event(Spectrum, material$promotion_event())
    }, ignoreInit = TRUE)
    shiny::observeEvent(material$restore_event(), {
      activate_spectran_transmission_event(Spectrum, material$restore_event())
    }, ignoreInit = TRUE)
  }
  shiny::shinyApp(ui, server)
}
