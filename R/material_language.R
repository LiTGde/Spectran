# Semantic additions for the shared material workflow. Each entry is EN, DE.
material_strings <- function()
  list(
    default_daylight_name = c(
      "Daylight D65, 100 lx (automatic)",
      "Tageslicht D65, 100 lx (automatisch)"
    ),
    default_daylight_notice = c(
      "No light spectrum has been imported. This light path starts automatically with daylight D65 at 100 lx. To choose a different starting spectrum, use Import.",
      "Es wurde noch kein Lichtspektrum importiert. Als Ausgangsbasis dieses Lichtpfads wurde automatisch Tageslicht D65 mit 100 lx gew\u00e4hlt. Ein anderes Ausgangsspektrum k\u00f6nnen Sie unter Import laden."
    ),
    edi_group = c(
      "Equivalent daylight illuminance (EDI)",
      "\u00c4quivalente Tageslicht-Beleuchtungsst\u00e4rke (EDI)"
    ),
    der_group = c(
      "Daylight efficacy ratios (DER)",
      "Tageslicht-Effizienzverh\u00e4ltnisse (DER)"
    ),
    aria_balance_table = c(
      "Alpha-opic EDI and DER changes table",
      "Tabelle der \u00c4nderungen alpha-opischer EDI und DER"
    ),
    aria_archived_balance_table = c(
      "Archived alpha-opic EDI and DER changes table",
      "Archivierte Tabelle der \u00c4nderungen alpha-opischer EDI und DER"
    ),
    export_bundle_balance_png = c(
      "Alpha-opic EDI and DER table (PNG)",
      "alpha-opisch (PNG)"
    ),
    export_bundle_balance_csv = c(
      "Alpha-opic EDI and DER (CSV)",
      "alpha-opisch (CSV)"
    ),
    download_balance_table_png = c(
      "Alpha-opic EDI and DER table (PNG)",
      "alpha-opisch (PNG)"
    ),
    download_balance_table_csv = c(
      "Alpha-opic EDI and DER table (CSV)",
      "alpha-opisch (CSV)"
    ),
    balance_heading = c(
      "Alpha-opic EDI and DER before and after the material",
      "Alpha-opische EDI und DER vor und nach der Materialwirkung"
    ),
    balance_intro = c(
      "EDI values are equivalent daylight illuminances in lux. DER values describe the spectral effect relative to daylight D65 at equal photopic illuminance. Absolute changes use the unit of the corresponding row; relative changes use its incident value as denominator.",
      "EDI-Werte sind \u00e4quivalente Tageslicht-Beleuchtungsst\u00e4rken in Lux. DER-Werte beschreiben die spektrale Wirkung relativ zu Tageslicht D65 bei gleicher photopischer Beleuchtungsst\u00e4rke. Absolute \u00c4nderungen verwenden die Einheit der jeweiligen Zeile, relative \u00c4nderungen deren einfallenden Wert als Nenner."
    ),
    tab_promotion = c("Promotion", "\u00dcbernahme"),
    history_source = c("Source", "Quelle"),
    history_selected = c("Shown below", "Unten angezeigt"),
    history_show = c("Show", "Anzeigen"),
    history_restore = c("Restore", "Wiederherstellen"),
    history_actions = c("Actions", "Aktionen"),
    history_already_shown = c(
      "Already shown below",
      "Wird bereits unten angezeigt"
    ),
    history_already_active = c(
      "Already the active source",
      "Bereits die aktive Quelle"
    ),
    history_actions_help = c(
      "Show selects a node for both the cumulative comparison and its archived result below. Restore also makes it the active source for the next material. Yellow marks the active source. Unavailable actions are greyed out. Later branches are preserved.",
      "Anzeigen w\u00e4hlt einen Knoten f\u00fcr die Gesamt\u00e4nderung und das archivierte Ergebnis darunter. Wiederherstellen macht ihn zus\u00e4tzlich zur aktiven Quelle f\u00fcr das n\u00e4chste Material. Gelb markiert die aktive Quelle. Nicht verf\u00fcgbare Aktionen sind ausgegraut. Sp\u00e4tere Zweige bleiben erhalten."
    ),
    archive_source = c(
      "This node is the imported source. It has no archived material result. Select a transmission or reflection node with Show to inspect its result.",
      "Dieser Knoten ist die importierte Quelle. F\u00fcr ihn gibt es kein archiviertes Materialergebnis. W\u00e4hlen Sie bei einem Transmissions- oder Reflexionsknoten Anzeigen, um dessen Ergebnis zu sehen."
    ),
    coefficient_source = c(
      "Selected light spectrum",
      "Gew\u00e4hltes Lichtspektrum"
    ),
    coefficient_incident = c(
      "Selected spectrum (%)",
      "Gew\u00e4hltes Spektrum (%)"
    ),
    coefficient_difference = c(
      "Difference (percentage points)",
      "Abweichung (Prozentpunkte)"
    ),
    coefficient_note = c(
      "Spectrally weighted material coefficients over 380-780 nm. Difference = selected spectrum minus D65, in percentage points. A positive value means a higher coefficient under the selected light. A zero weighted input gives an undefined coefficient; the D65 reference remains available.",
      "Spektral gewichtete Materialgrade \u00fcber 380-780 nm. Abweichung = gew\u00e4hltes Spektrum minus D65, in Prozentpunkten. Ein positiver Wert bedeutet einen h\u00f6heren Grad unter dem gew\u00e4hlten Licht. Bei einem gewichteten Eingangswert von null ist der Grad nicht definiert; der D65-Bezugswert bleibt verf\u00fcgbar."
    ),
    colour_heading = c(
      "Approximate material colour",
      "Ungef\u00e4hre Materialfarbe"
    ),
    colour_d65 = c("Under daylight D65", "Unter Tageslicht D65"),
    colour_luminance = c("Y (white = 100%)", "Y (Wei\u00df = 100 %)"),
    colour_incomplete = c(
      "Complete the reflectance curve to show the material colour.",
      "Vervollst\u00e4ndigen Sie die Reflexionskurve f\u00fcr die Farbvorschau."
    ),
    colour_note = c(
      "Approximate screen colour under daylight D65, independent of the selected light spectrum. Material lightness is retained relative to a perfect white surface. Gloss, texture and viewing angle are not shown.",
      "Ungef\u00e4hre Bildschirmfarbe unter Tageslicht D65, unabh\u00e4ngig vom gew\u00e4hlten Lichtspektrum. Die Materialhelligkeit bleibt im Verh\u00e4ltnis zu einer ideal wei\u00dfen Fl\u00e4che erhalten. Glanz, Struktur und Blickwinkel werden nicht dargestellt."
    ),
    colour_method_heading = c(
      "How the colour is calculated",
      "So wird die Farbe berechnet"
    ),
    colour_method = c(
      "The D65 spectrum is multiplied by reflectance and integrated with the CIE 1931 2-degree observer over 380-780 nm. XYZ is normalized to a perfect diffuser under D65, then converted to sRGB with D65 display white and limited to the displayable range. The selected light spectrum and its illuminance do not change this reference preview. Assumed tails affect the colour.",
      "Das D65-Spektrum und der Reflexionsgrad werden multipliziert und mit dem CIE-1931-Normalbeobachter (2\u00b0) \u00fcber 380-780 nm integriert. XYZ wird auf eine ideal wei\u00dfe, diffus reflektierende Fl\u00e4che unter D65 bezogen und in sRGB mit D65-Bildschirmwei\u00df umgerechnet. Nicht darstellbare Werte werden begrenzt. Das gew\u00e4hlte Lichtspektrum und seine Beleuchtungsst\u00e4rke \u00e4ndern diese Bezugsvorschau nicht. Angenommene Randbereiche beeinflussen die Farbe."
    ),
    export_heading = c(
      "Export material results",
      "Materialergebnisse exportieren"
    ),
    aria_archived_results = c(
      "Archived promoted material results",
      "Archivierte Ergebnisse angewendeter Materialien"
    ),
    aria_archived_absolute_table = c(
      "Archived incident and receiver metrics table",
      "Archivierte Tabelle der einfallenden und empfangenen Kennwerte"
    ),
    alt_archived_spectral_comparison = c(
      "Incident spectrum and material output for the archived result, from 380 to 780 nm. Reflection output is spectral radiant exitance; receiver irradiance uses F = 1.",
      "Einfallendes Spektrum und Materialergebnis des archivierten Ergebnisses von 380 bis 780 nm. Das Reflexionsergebnis ist die spektrale spezifische Ausstrahlung; die Empfangsbestrahlungsst\u00e4rke verwendet F = 1."
    ),
    template_download = c(
      "Download 100% material-coefficient CSV template",
      "CSV-Vorlage mit 100 % Materialkoeffizient herunterladen"
    ),
    import_unlocks_transmission = c(
      "Import a source spectrum or open Transmission / Reflection directly to start with daylight D65 at 100 lx.",
      "Importieren Sie ein Quellspektrum oder starten Sie Transmission / Reflexion direkt mit Tageslicht D65 bei 100 lx."
    ),
    promote_button = c(
      "Promote receiver spectrum",
      "Empfangsspektrum \u00fcbernehmen"
    ),
    aria_history_table = c(
      "Session material history table",
      "Materialverlauf dieser Sitzung"
    ),
    alt_spectral_comparison = c(
      "Incident spectrum and material output from 380 to 780 nm. Reflection output is spectral radiant exitance; receiver irradiance uses the F = 1 assumption.",
      "Einfallendes Spektrum und Materialergebnis von 380 bis 780 nm. Das Reflexionsergebnis ist die spektrale spezifische Ausstrahlung; die Empfangsbestrahlungsst\u00e4rke verwendet F = 1."
    ),
    alt_construction = c(
      "Supplied and completed material coefficient on the visible wavelength grid. Colours and shapes distinguish source values, interpolation, and assumed tails.",
      "Gelieferter und vervollst\u00e4ndigter Materialkoeffizient im sichtbaren Wellenl\u00e4ngenraster. Farben und Formen kennzeichnen Quellwerte, Interpolation und angenommene Randbereiche."
    ),
    workflow_label = c("Material sections", "Materialbereiche"),
    promote_intro = c(
      "Promotion makes the chosen receiver scenario the active source. Its unscaled material result stays in history. Restoring an earlier node preserves later branches. History lasts for this Shiny session only.",
      "Die \u00dcbernahme macht das gew\u00e4hlte Empfangsszenario zur aktiven Quelle. Das unskalierte Materialergebnis bleibt im Verlauf erhalten. Das Wiederherstellen eines fr\u00fcheren Knotens erh\u00e4lt sp\u00e4tere Zweige. Der Verlauf gilt nur f\u00fcr diese Shiny-Sitzung."
    ),
    choose_heading = c(
      "Choose a material spectrum",
      "Materialspektrum ausw\u00e4hlen"
    ),
    choose_intro = c(
      "Select a TUB / 67600 example or upload a spectral material coefficient. The selected interaction determines whether the curve is transmittance or reflectance.",
      "W\u00e4hlen Sie ein TUB / 67600 Beispiel oder laden Sie einen spektralen Materialkoeffizienten hoch. Die gew\u00e4hlte Materialwirkung bestimmt, ob die Kurve Transmission oder Reflexion beschreibt."
    ),
    source_label = c(
      "Material-spectrum source",
      "Quelle des Materialspektrums"
    ),
    csv_label = c("Material CSV", "Material-CSV"),
    csv_value = c(
      "Material coefficient column",
      "Spalte des Materialkoeffizienten"
    ),
    file_transport_status = c(
      "File received. See the readiness panel for parsing and validation results.",
      "Datei empfangen. Der Statusbereich zeigt das Ergebnis des Einlesens und der Validierung."
    ),
    csv_tooltip = c(
      "Upload wavelength and material-coefficient columns. Choose the fraction or percent scale explicitly.",
      "Laden Sie Wellenl\u00e4nge und Materialkoeffizient hoch. W\u00e4hlen Sie Anteil oder Prozent ausdr\u00fccklich aus."
    ),
    show_transmittance_panel = c(
      "Show material coefficient",
      "Materialkoeffizient anzeigen"
    ),
    apply_intro = c(
      "Apply the selected material to the active spectrum. Receiver metrics use the F = 1 assumption shown above.",
      "Wenden Sie das gew\u00e4hlte Material auf das aktive Spektrum an. Die Empfangsmetriken verwenden die oben erl\u00e4uterte Annahme F = 1."
    ),
    mode = c("Material interaction", "Materialwirkung"),
    transmission = c("Transmission", "Transmission"),
    reflection = c("Reflection", "Reflexion"),
    menu = c("Transmission / Reflection", "Transmission / Reflexion"),
    tab_spectrum = c("Material spectrum", "Materialspektrum"),
    tub = c("TUB / DIN/TS 67600 examples", "TUB / DIN/TS 67600 Beispiele"),
    target_lux = c(
      "Carried-forward illuminance (lx)",
      "\u00dcbernommene Beleuchtungsst\u00e4rke (lx)"
    ),
    target_help = c(
      "The default preserves the material result with receiver factor F = 1. Reflection assumes a uniformly illuminated Lambertian surface filling the receiver hemisphere. Transmission assumes all measured transmitted light reaches the receiver. Edit illuminance to define a different receiver scenario. Values above the default are explicit rescaling, not material gain. Geometry and multiple room reflections are not simulated.",
      "Die Vorgabe erh\u00e4lt das Materialergebnis mit Empfangsfaktor F = 1. Bei Reflexion f\u00fcllt eine gleichm\u00e4\u00dfig beleuchtete lambertsche Fl\u00e4che die Empfangshalbkugel. Bei Transmission erreicht das gesamte erfasste transmittierte Licht den Empf\u00e4nger. Eine andere Beleuchtungsst\u00e4rke definiert ein neues Empfangsszenario. Werte \u00fcber der Vorgabe sind eine explizite Skalierung, kein Materialgewinn. Geometrie und Mehrfachreflexionen im Raum werden nicht simuliert."
    ),
    reflection_model = c(
      "Reflection: M\u03bb = E\u03bb \u00d7 \u03c1\u03bb is spectral radiant exitance. The receiver metrics below assume F = 1 and a uniformly illuminated Lambertian surface filling the receiver hemisphere. Each promotion follows one chosen light path.",
      "Reflexion: M\u03bb = E\u03bb \u00d7 \u03c1\u03bb ist die spektrale spezifische Ausstrahlung. Die Empfangsmetriken setzen F = 1 und eine gleichm\u00e4\u00dfig beleuchtete lambertsche Fl\u00e4che voraus, die die Empfangshalbkugel ausf\u00fcllt. Jede \u00dcbernahme folgt einem gew\u00e4hlten Lichtpfad."
    ),
    transmission_model = c(
      "Transmission: E\u2032\u03bb = E\u03bb \u00d7 \u03c4\u03bb. Receiver metrics assume F = 1. For scattering materials, the received fraction also depends on measurement and receiver geometry.",
      "Transmission: E\u2032\u03bb = E\u03bb \u00d7 \u03c4\u03bb. Die Empfangsmetriken setzen F = 1 voraus. Bei streuenden Materialien h\u00e4ngt der empfangene Anteil zus\u00e4tzlich von Mess- und Empfangsgeometrie ab."
    ),
    cumulative = c(
      "Cumulative changes along a branch",
      "Gesamt\u00e4nderung entlang eines Zweigs"
    ),
    cumulative_node = c(
      "Compare this node with its original source",
      "Diesen Knoten mit seiner urspr\u00fcnglichen Quelle vergleichen"
    ),
    cumulative_material = c(
      "Combined material effect (F = 1)",
      "Gesamte Materialwirkung (F = 1)"
    ),
    cumulative_help = c(
      "Only ancestors of the selected node contribute. Cumulative metrics are shown only for branches without illuminance rescaling. Changes are recalculated from the original and final spectra.",
      "Es z\u00e4hlen nur die Vorg\u00e4nger des gew\u00e4hlten Knotens. Kumulierte Kennwerte werden nur f\u00fcr Zweige ohne zwischenzeitliche Beleuchtungsst\u00e4rke-Skalierung angezeigt. \u00c4nderungen werden aus Ursprungs- und Ergebnisspektrum neu berechnet."
    ),
    cumulative_rescaled = c(
      "No cumulative metrics are shown for this branch because illuminance was rescaled. Choose a node before the rescaling or a branch without rescaling. Individual results remain available in history.",
      "F\u00fcr diesen Zweig werden keine kumulierten Kennwerte angezeigt, da die Beleuchtungsst\u00e4rke zwischenzeitlich skaliert wurde. W\u00e4hlen Sie einen Knoten vor der Skalierung oder einen Zweig ohne Skalierung. Die einzelnen Ergebnisse bleiben im Verlauf verf\u00fcgbar."
    ),
    effective_der_reference = c(
      "Effective MDER: DIN/TS 67600:2022-08, sections 6.2.4.2 and 6.2.4.4, Table 11. MEDI after the material divided by photopic illuminance before it; for the cumulative unscaled path, the denominator is the original source illuminance. This differs from the MDER of the outgoing light.",
      "Effektiver MDER: DIN/TS 67600:2022-08, Abschnitte 6.2.4.2 und 6.2.4.4, Tabelle 11. MEDI nach dem Material geteilt durch die photopische Beleuchtungsst\u00e4rke davor; beim kumulierten, unskalierten Lichtpfad dient die Beleuchtungsst\u00e4rke der Ursprungsquelle als Nenner. Dies unterscheidet sich vom MDER des austretenden Lichts."
    ),
    cumulative_download = c(
      "Download cumulative material effect (CSV)",
      "Gesamt\u00e4nderung herunterladen (CSV)"
    ),
    effective_der = c(
      "Effective MDER (MEDI / reference Ev)",
      "Effektiver MDER (MEDI / Referenz-Ev)"
    )
  )

material_text <- function(key, language_direct = NULL) {
  value <- material_strings()[[key]]
  if (is.null(value)) stop("Unknown material text key: ", key, call. = FALSE)
  value[[
    if (transmission_language_setting(language_direct) == "Deutsch") 2L else 1L
  ]]
}

material_labeler <- function(mode) {
  mode <- material_mode(mode = mode)
  function(key, ..., language_direct = NULL) {
    value <- transmission_text(key, ..., language_direct = language_direct)
    if (mode != "reflection") return(value)
    german <- transmission_language_setting(language_direct) == "Deutsch"
    overrides <- list(
      result_tab_d65 = c("Reflectance", "Reflexionsgrad"),
      d65_heading = c("Reflectance", "Reflexionsgrad"),
      download_d65 = c("Reflectance comparison (CSV)", "Reflexionsgrade (CSV)"),
      export_bundle_d65_png = c(
        "Reflectance comparison (PNG)",
        "Reflexionsgrade (PNG)"
      ),
      export_bundle_d65_csv = c(
        "Reflectance comparison (CSV)",
        "Reflexionsgrade (CSV)"
      ),
      download_d65_table_png = c(
        "Reflectance comparison (PNG)",
        "Reflexionsgrade (PNG)"
      ),
      table_retained = c("Reflected (%)", "Reflektiert (%)"),
      metric_tau_v_d65 = c(
        "Photopic reflectance",
        "Photopischer Reflexionsgrad"
      ),
      tail_opaque = c(
        "Assume zero reflectance (0%)",
        "Reflexionsgrad null annehmen (0 %)"
      ),
      tail_transparent = c(
        "Assume full reflectance (100%)",
        "Vollst\u00e4ndige Reflexion annehmen (100 %)"
      ),
      require_lower_tail = c(
        "Choose the lower-tail assumption (%s): zero reflectance (0%%), full reflectance (100%%), or carry the first supplied value backward.",
        "W\u00e4hlen Sie die Annahme f\u00fcr den unteren Randbereich (%s): Reflexionsgrad null (0 %%), vollst\u00e4ndige Reflexion (100 %%) oder den ersten gelieferten Wert nach unten fortschreiben."
      ),
      require_upper_tail = c(
        "Choose the upper-tail assumption (%s): zero reflectance (0%%), full reflectance (100%%), or carry the last supplied value forward.",
        "W\u00e4hlen Sie die Annahme f\u00fcr den oberen Randbereich (%s): Reflexionsgrad null (0 %%), vollst\u00e4ndige Reflexion (100 %%) oder den letzten gelieferten Wert nach oben fortschreiben."
      ),
      export_scale_max = c(
        "Maximum irradiance / exitance (mW/m\u00b2/nm, optional)",
        "Maximum: Bestrahlungsst\u00e4rke / spezifische Ausstrahlung (mW/m\u00b2/nm, optional)"
      ),
      plot_result_subtitle = c(
        "Dashed: incident irradiance E\u03bb \u00b7 Solid: reflected exitance M\u03bb",
        "Gestrichelt: Bestrahlungsst\u00e4rke E\u03bb \u00b7 Durchgezogen: spezifische Ausstrahlung M\u03bb"
      ),
      plot_spectral_irradiance = c(
        "E\u03bb / M\u03bb (mW m\u207b\u00b2 nm\u207b\u00b9)",
        "E\u03bb / M\u03bb (mW m\u207b\u00b2 nm\u207b\u00b9)"
      ),
      table_transmitted = c("Receiver (F = 1)", "Empf\u00e4nger (F = 1)"),
      gt_retained_note = c(
        "Receiver irradiance assumes F = 1. Native reflection is exitance M\u03bb = E\u03bb \u00d7 \u03c1\u03bb. Retained fractions refer to the incident spectrum.",
        "Die Empfangsbestrahlungsst\u00e4rke setzt F = 1 voraus. Das native Reflexionsergebnis ist die spezifische Ausstrahlung M\u03bb = E\u03bb \u00d7 \u03c1\u03bb. Erhaltene Anteile beziehen sich auf das einfallende Spektrum."
      ),
      type = c("Reflectance definition", "Definition des Reflexionsgrads"),
      type_tooltip = c(
        "Passive, nonfluorescent surface model. Prefer total hemispherical reflectance. Directional or unknown coefficients require explicit qualification. No angular redistribution or wavelength conversion is simulated.",
        "Passives, nicht fluoreszierendes Oberfl\u00e4chenmodell. Gesamtreflexionsgrade \u00fcber die Halbkugel werden bevorzugt. Gerichtete oder unbekannte Koeffizienten m\u00fcssen ausdr\u00fccklich eingeordnet werden. Winkelverteilung und Wellenl\u00e4ngenumwandlung werden nicht simuliert."
      ),
      type_internal = c(
        "Directional / qualified",
        "Gerichtet / eingeschr\u00e4nkt"
      ),
      type_total = c("Total hemispherical", "Gesamt \u00fcber die Halbkugel")
    )
    if (!is.null(overrides[[key]])) {
      value <- overrides[[key]][[if (german) 2L else 1L]]
      arguments <- list(...)
      if (length(arguments) > 0L)
        value <- do.call(sprintf, c(list(fmt = value), arguments))
      return(value)
    }
    material_relabel(value, mode, language_direct)
  }
}

# Adapt shared legacy diagnostics without changing their stable audit record.
material_relabel <- function(value, mode, language_direct = NULL) {
  if (material_mode(mode = mode) != "reflection") return(value)
  replacements <- c(
    "Transmissions" = "Reflexions",
    "transmittance" = "reflectance",
    "Transmittance" = "Reflectance",
    "transmission" = "reflection",
    "Transmission" = "Reflection",
    "transmitted" = "reflected",
    "Transmitted" = "Reflected",
    "transmittiert" = "reflektiert",
    "Transmittiert" = "Reflektiert",
    "transmittier" = "reflektier",
    "Transmittier" = "Reflektier",
    "Transmissionsgrad" = "Reflexionsgrad"
  )
  if (transmission_language_setting(language_direct) == "Deutsch")
    replacements[c("Transmission", "transmission")] <- c(
      "Reflexion",
      "Reflexion"
    )
  for (from in names(replacements))
    value <- gsub(from, replacements[[from]], value, fixed = TRUE)
  value
}
