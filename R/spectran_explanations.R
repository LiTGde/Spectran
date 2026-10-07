# Static explanations and session-local navigation. These pages never modify a
# spectrum, a material draft, or a saved result. Context links are detached UI
# parts of this module, injected by the app into the relevant feature UIs.
spectran_help_text <- function(de, en) {
  if (identical(transmission_language_setting(), "Deutsch")) de else en
}

spectran_explanation_topics <- function() {
  tr <- spectran_help_text
  list(
    spectrum = list(group = "basics", icon = "chart-area",
      title = tr("Ein Spektrum lesen", "Reading a spectrum"),
      summary = tr("Wellenl\u00e4nge, H\u00f6he, Fl\u00e4che und Empfindlichkeitskurven.", "Wavelength, height, area and sensitivity curves.")),
    quantities = list(group = "basics", icon = "sun",
      title = tr("Beleuchtungsst\u00e4rke, EDI und DER", "Illuminance, EDI and DER"),
      summary = tr("Lichtmenge und spektrale Zusammensetzung unterscheiden.", "Distinguish light level from spectral composition.")),
    colour = list(group = "basics", icon = "palette",
      title = tr("Lichtfarbe und Farbwiedergabe", "Light colour and colour rendering"),
      summary = htmltools::HTML(tr("Was CCT, R<sub>a</sub> und die Testfarben beschreiben.",
        "What CCT, R<sub>a</sub> and the test colours describe."))),
    age = list(group = "basics", icon = "eye",
      title = tr("Alter und Auge", "Age and the eye"),
      summary = tr("Augenmedien, Pupille und der 32-j\u00e4hrige Referenzbeobachter.", "Ocular media, pupil size and the 32-year-old reference observer.")),
    workflow = list(group = "basics", icon = "file-import",
      title = tr("Import, Skalierung und Export", "Import, scaling and export"),
      summary = tr("Von der Lichtquelle zu vergleichbaren Ergebnissen.", "From a light source to comparable results.")),
    material = list(group = "materials", icon = "layer-group",
      title = tr("Materialwirkung im \u00dcberblick", "Material effects at a glance"),
      summary = tr("Einfallendes Licht und zwei m\u00f6gliche Lichtwege.", "Incident light and two possible light paths.")),
    interactions = list(group = "materials", icon = "arrows-left-right",
      title = tr("Einfallend und austretend", "Incident and outgoing light"),
      summary = tr("Transmission und Reflexion mit ihren Ein- und Ausgangsgr\u00f6\u00dfen.", "Transmission and reflection with their input and output quantities.")),
    material_spectra = list(group = "materials", icon = "wave-square",
      title = tr("Wie ein Material das Spektrum ver\u00e4ndert", "How a material changes the spectrum"),
      summary = tr("Das Zusammenspiel von Lichtquelle und Materialkurve.", "How the source spectrum and material curve combine.")),
    receiver = list(group = "materials", icon = "bullseye",
      title = tr("Vom Material zum Empf\u00e4nger: F", "From material to receiver: F"),
      summary = tr("Was das Empfangsszenario F = 1 bedeutet.", "What the F = 1 receiver scenario means.")),
    path = list(group = "materials", icon = "route",
      title = tr("Schritte und Gesamtwirkung im Lichtpfad", "Steps and combined effects in the light path"),
      summary = tr("Ergebnisse speichern, fortsetzen und vom Start aus vergleichen.", "Save results, continue a path and compare against its start."))
  )
}

spectran_explanation_routes <- function() {
  c(from_import = "workflow", from_export = "workflow", from_spectrum = "spectrum",
    from_quantities = "quantities", from_colour = "colour", from_age = "age",
    from_material = "material", from_preview = "material_spectra",
    from_setup_model = "receiver", from_result_model = "receiver", from_path = "path")
}

spectran_explanation_links_ui <- function(id) {
  ns <- shiny::NS(id)
  tr <- spectran_help_text
  labels <- c(
    from_import = tr("Import und Skalierung erkl\u00e4rt", "Import and scaling explained"),
    from_export = tr("Ausgaben und Vergleichbarkeit erkl\u00e4rt", "Outputs and comparability explained"),
    from_spectrum = tr("Spektrum und Kurven erkl\u00e4rt", "Spectrum and curves explained"),
    from_quantities = tr("EDI und DER erkl\u00e4rt", "EDI and DER explained"),
    from_colour = tr("Lichtfarbe und Farbwiedergabe erkl\u00e4rt", "Light colour and colour rendering explained"),
    from_age = tr("Alterskorrektur erkl\u00e4rt", "Age correction explained"),
    from_material = tr("Transmission und Reflexion erkl\u00e4rt", "Transmission and reflection explained"),
    from_preview = tr("Materialkurven erkl\u00e4rt", "Material curves explained"),
    from_setup_model = tr("Was bedeutet F = 1?", "What does F = 1 mean?"),
    from_result_model = tr("Was bedeutet F = 1?", "What does F = 1 mean?"),
    from_path = tr("Lichtpfad und Gesamtwirkung erkl\u00e4rt", "Light path and combined effects explained"))
  stats::setNames(lapply(names(labels), function(key) {
    shiny::actionLink(ns(key), labels[[key]], icon = shiny::icon("circle-question"),
      class = "spectran-help-link")
  }), names(labels))
}

spectran_help_figure <- function(file, alt) {
  tr <- spectran_help_text
  language <- if (identical(transmission_language_setting(), "Deutsch")) "de" else "en"
  src <- paste0("extr/explanations/", file, "-", language, ".svg")
  htmltools::tags$figure(class = "spectran-help-figure",
    htmltools::tags$img(src = src, alt = alt, width = "1200", height = "800", loading = "lazy"),
    htmltools::tags$figcaption(
      htmltools::a(href = src, target = "_blank", rel = "noopener",
        shiny::icon("up-right-from-square"),
        tr("Grafik in voller Gr\u00f6\u00dfe \u00f6ffnen (neuer Tab)", "Open full-size graphic (new tab)"))))
}

spectran_explanation_content <- function(topic) {
  tr <- spectran_help_text
  # Markup is limited to these static translations; image alt text stays plain.
  rich <- function(de, en) htmltools::HTML(tr(de, en))
  p <- htmltools::p
  h3 <- htmltools::h3
  note <- function(...) htmltools::div(class = "spectran-help-note", ...)
  formula <- function(...) htmltools::div(class = "spectran-help-formula", ...)
  card <- function(title, text) htmltools::div(class = "spectran-help-card",
    htmltools::h3(title), p(text))
  cards <- function(..., columns = 2L) htmltools::div(
    class = paste("spectran-help-cards", if (columns == 3L) "spectran-help-cards-three" else NULL), ...)
  application <- function(title, ...) htmltools::tags$section(class = "spectran-help-application",
    htmltools::h3(title), htmltools::div(class = "spectran-help-application-body", ...))
  refs <- function(...) htmltools::div(class = "spectran-help-references",
    htmltools::strong(tr("Weiterlesen: ", "Read more: ")), ...)
  link <- function(label, href) htmltools::a(label, href = href, target = "_blank", rel = "noopener")
  switch(topic,
    spectrum = htmltools::tagList(
      p(tr("Das Spektrum zeigt, wie sich die Strahlung auf die Wellenl\u00e4ngen verteilt. Spectran wertet den Bereich von 380 bis 780 nm aus.",
        "The spectrum shows how radiation is distributed across wavelengths. Spectran evaluates the range from 380 to 780 nm.")),
      cards(
        card(tr("Waagerecht: Wellenl\u00e4nge", "Horizontal: wavelength"), tr("Die Wellenl\u00e4nge \u03bb steht in Nanometern (nm). Kurze Wellenl\u00e4ngen liegen links, lange rechts. Die Spektralfarben dienen der Orientierung; ein Bildschirm kann monochromatisches Licht nicht farbgetreu wiedergeben.",
          "Wavelength \u03bb is given in nanometres (nm). Short wavelengths are on the left, long wavelengths on the right. Spectral colours are a visual guide; a screen cannot faithfully reproduce monochromatic light.")),
        card(tr("Senkrecht: spektrale Bestrahlungsst\u00e4rke", "Vertical: spectral irradiance"), rich("E<sub>\u03bb</sub> beschreibt die Strahlungsleistung pro Fl\u00e4che und Wellenl\u00e4ngenintervall. Die H\u00f6he eines Peaks allein beschreibt noch nicht die gesamte Bestrahlungsst\u00e4rke.",
          "E<sub>\u03bb</sub> describes radiant power per area and wavelength interval. The height of a peak alone does not describe the total irradiance."))),
      spectran_help_figure("05-spektrum-lesen", tr(
        "Ein schmaler hoher Peak und ein breiter niedrigerer Peak verdeutlichen den Unterschied zwischen H\u00f6he und Fl\u00e4che. Die Fl\u00e4che \u00fcber 380 bis 780 nm ergibt die Bestrahlungsst\u00e4rke. Darunter wird das Beispielspektrum mit V Lambda gewichtet und zur Kenngr\u00f6\u00dfe integriert.",
        "A tall narrow peak and a lower broad peak distinguish height from area. The area over 380 to 780 nm gives irradiance. Below, the example spectrum is weighted by V lambda and integrated to obtain a metric.")),
      application(
      tr("Vom Spektrum zur Kenngr\u00f6\u00dfe", "From spectrum to metric"),
      formula(tr("Fl\u00e4che unter dem Spektrum \u2192 Bestrahlungsst\u00e4rke", "Area under the spectrum \u2192 irradiance")),
      p(tr("Empfindlichkeitskurven zeigen, welche Wellenl\u00e4ngen bei einer Bewertung st\u00e4rker oder schw\u00e4cher gewichtet werden. F\u00fcr eine Kenngr\u00f6\u00dfe wird das Spektrum mit der jeweiligen Kurve gewichtet und \u00fcber die Wellenl\u00e4nge integriert. Die zur Darstellung skalierten Empfindlichkeitskurven sind keine zus\u00e4tzliche Strahlung.",
        "Sensitivity curves show which wavelengths receive more or less weight in an assessment. A metric is obtained by weighting the spectrum with the relevant curve and integrating over wavelength. Sensitivity curves scaled for display are not additional radiation.")),
      note(tr("Beim Vergleichen immer Einheit und Achsenskalierung pr\u00fcfen. Ein h\u00f6her gezeichneter Peak kann auch durch eine andere Skalierung entstehen.",
        "Always check units and axis scales when comparing plots. A taller displayed peak can also result from a different scale.")))),
    quantities = htmltools::tagList(
      p(tr("Dasselbe Spektrum l\u00e4sst sich nach Strahlungsleistung, Hellempfindlichkeit oder den f\u00fcnf \u03b1-opischen Empfindlichkeiten bewerten.",
        "The same spectrum can be assessed by radiant power, photopic sensitivity or the five \u03b1-opic sensitivities.")),
      cards(
      card(tr("Bestrahlungsst\u00e4rke", "Irradiance"), tr("Strahlungsleistung pro Fl\u00e4che, in W/m\u00b2 oder mW/m\u00b2. Die Gesamtbestrahlungsst\u00e4rke wird ohne Empfindlichkeitsgewichtung aus dem Spektrum gebildet.",
          "Radiant power per area, in W/m\u00b2 or mW/m\u00b2. Total irradiance is obtained from the spectrum without sensitivity weighting.")),
      card(rich("Photopische Beleuchtungsst\u00e4rke E<sub>v</sub>", "Photopic illuminance E<sub>v</sub>"), tr("Mit V(\u03bb) gewichtetes Licht am Empfangsort, in Lux (lx). Es beschreibt die photometrische Bewertung f\u00fcr das Tagsehen.",
          "Light at the receiving location weighted by V(\u03bb), in lux (lx). It describes the photometric assessment for daytime vision.")),
      card(tr("Melanopische EDI", "Melanopic EDI"), rich("Die Beleuchtungsst\u00e4rke des Referenztageslichts D65 mit derselben melanopischen Bestrahlungsst\u00e4rke wie das untersuchte Licht. Die Einheit ist ebenfalls lx; EDI ist eine andere Bewertung als E<sub>v</sub>.",
          "The illuminance of reference daylight D65 with the same melanopic irradiance as the light being assessed. Its unit is also lx; EDI is a different assessment from E<sub>v</sub>.")),
      columns = 3L),
      spectran_help_figure("06-beleuchtungsstaerke-edi-der", tr(
        "Drei Bewertungswege f\u00fchren vom selben Spektrum zur Bestrahlungsst\u00e4rke, zur Beleuchtungsst\u00e4rke und zur melanopischen EDI. Im frei gew\u00e4hlten Rechenbeispiel werden aus 100 Lux bei DER 0,6 eine EDI von 60 Lux. Verdoppeln auf 200 Lux ergibt 120 Lux EDI bei gleicher DER.",
        "Three assessments lead from the same spectrum to irradiance, illuminance and melanopic EDI. In a freely chosen numerical example, 100 lux at DER 0.6 gives 60 lux EDI. Doubling to 200 lux gives 120 lux EDI at the same DER.")),
      application(
      tr("Lichtniveau und Spektralform unterscheiden", "Distinguish light level from spectral shape"),
      p(
      rich("Die melanopische DER (MDER) ist das Verh\u00e4ltnis melanopische EDI / E<sub>v</sub>. Es ist einheitenlos und beschreibt die spektrale Zusammensetzung relativ zu D65. Bei unver\u00e4nderter Spektralform bleibt DER beim Skalieren gleich.",
          "Melanopic DER (MDER) is the ratio of melanopic EDI to E<sub>v</sub>. It is dimensionless and describes spectral composition relative to D65. With an unchanged spectral shape, DER stays constant when the level is scaled.")),
      formula(rich("Melanopische EDI = E<sub>v</sub> \u00d7 melanopische DER", "Melanopic EDI = E<sub>v</sub> \u00d7 melanopic DER")),
      p(tr("Im \u03b1-opischen Bereich stehen neben der melanopischen auch die rhodopische sowie die S-, M- und L-Zapfen-Bewertung zur Verf\u00fcgung. EDI und DER werden f\u00fcr jede Empfindlichkeit entsprechend gebildet.",
        "The \u03b1-opic view includes melanopic, rhodopic and S-, M- and L-cone assessments. EDI and DER are defined accordingly for each sensitivity.")),
      note(rich("Diese Kenngr\u00f6\u00dfen beschreiben einen Lichtreiz. Sie sagen allein keine individuelle Wirkung auf Schlaf, Wachheit oder Gesundheit voraus. Bei E<sub>v</sub> = 0 ist das Verh\u00e4ltnis EDI / E<sub>v</sub> nicht definiert.",
        "These metrics describe a light stimulus. On their own, they do not predict an individual's sleep, alertness or health response. When E<sub>v</sub> = 0, the ratio EDI / E<sub>v</sub> is undefined."))),
      refs(link("CIE S 026:2018", URL_CIE))),
    colour = htmltools::tagList(
      p(
      tr("Wie sieht das Licht selbst aus, und wie erscheinen beleuchtete Farben? CCT und Farbwiedergabe beantworten diese beiden unterschiedlichen Fragen.", "What does the light itself look like, and how do illuminated colours appear? CCT and colour rendering answer these two different questions.")),
      cards(
        card(tr("Lichtfarbe: CCT", "Light colour: CCT"), tr("Die \u00e4hnlichste Farbtemperatur in Kelvin beschreibt die N\u00e4he der Lichtfarbe zu einem Planckschen Strahler. Sie ist f\u00fcr Lichtfarben nahe dem Planckschen Kurvenzug sinnvoll. Gleiche CCT bedeutet nicht gleiche Spektren oder gleiche melanopische DER.",
          "Correlated colour temperature, in kelvin, relates the light colour to a Planckian radiator. It is meaningful for colours close to the Planckian locus. Equal CCT does not imply equal spectra or equal melanopic DER.")),
        card(rich("Farbwiedergabe: R<sub>a</sub> und R<sub>i</sub>", "Colour rendering: R<sub>a</sub> and R<sub>i</sub>"), rich("R<sub>a</sub> fasst die Farbwiedergabe von acht Testfarben gegen\u00fcber einer Referenzlichtquelle zusammen. Die einzelnen R<sub>i</sub> zeigen Unterschiede f\u00fcr bestimmte Testfarben. Der R<sub>a</sub>-Wert allein beschreibt nicht alle Farbeigenschaften einer Lichtquelle.",
          "R<sub>a</sub> summarises the colour rendering of eight test colours relative to a reference illuminant. Individual R<sub>i</sub> values show differences for particular test colours. R<sub>a</sub> alone does not describe every colour property of a light source."))),
      spectran_help_figure("07-lichtfarbe-farbwiedergabe", tr(
        "CCT ordnet die Lichtfarbe von warmwei\u00df bis kaltwei\u00df ein. Ri vergleichen einzelne Testfarben unter Testlicht und Referenz. Ra ist der Mittelwert R1 bis R8; R9 f\u00fcr ges\u00e4ttigtes Rot ist darin nicht enthalten. Die Farbfelder sind schematisch.",
        "CCT describes light colour from warm white to cool white. Ri compare individual test colours under test and reference lighting. Ra is the mean of R1 to R8; R9 for saturated red is not included. The colour patches are schematic.")),
      application(
      tr("Warum fehlt manchmal ein Wert?", "Why is a value sometimes unavailable?"),
      p(tr("In der photometrischen Auswertung k\u00f6nnen CIE-Anwendungsgrenzen ber\u00fccksichtigt werden. Au\u00dferhalb des zul\u00e4ssigen Bereichs ist eine Kenngr\u00f6\u00dfe gegebenenfalls nicht angegeben. Das Aufheben dieser Begrenzung erweitert die Rechenausgabe, nicht die physikalische Aussagekraft.",
        "The photometric analysis can enforce CIE applicability limits. Outside the applicable range, a metric may be unavailable. Disabling those limits expands the numerical output, not its physical interpretation."))),
      refs(link("CIE TN 013:2022 \u00b7 CCT", "https://www.cie.co.at/publications/terms-related-planckian-radiation-temperature-light-sources"),
        " \u00b7 ", link("CIE 13.3 \u00b7 CRI", "https://cie.co.at/publications/method-measuring-and-specifying-colour-rendering-properties-light-sources"))),
    age = htmltools::tagList(
      p(tr("Die Altersansicht zeigt eine modellhafte Anpassung der melanopischen Bewertung. Sie verwendet einen 32-j\u00e4hrigen Referenzbeobachter und trennt zwei Einfl\u00fcsse.",
        "The age view illustrates a model-based adjustment of the melanopic assessment. It uses a 32-year-old reference observer and separates two influences.")),
      cards(
        card(tr("Transmission der Augenmedien", "Transmission through the ocular media"), tr("Die spektrale Durchl\u00e4ssigkeit des Auges ver\u00e4ndert sich mit dem Alter. Deshalb h\u00e4ngt dieser Korrekturfaktor auch vom untersuchten Spektrum ab. Die relative Darstellung bezieht sich auf das Referenzalter.",
          "The spectral transmission of the eye changes with age. This correction factor therefore also depends on the spectrum being assessed. The relative display is referenced to the reference age.")),
        card(tr("Pupille", "Pupil"), tr("Die modellierte \u00c4nderung der Pupillengr\u00f6\u00dfe wird als weiterer Faktor ber\u00fccksichtigt. Die Gesamtansicht verbindet den Pupillenfaktor mit dem Transmissionsfaktor.",
          "The modelled change in pupil size is included as another factor. The combined view joins the pupil and transmission factors."))),
      spectran_help_figure("08-alter-und-auge", tr(
        "Das bestehende Spectran-Modell zeigt f\u00fcr 65 Jahre eine geringere Transmission der Augenmedien im kurzwelligen Bereich und eine kleinere Pupillenfl\u00e4che als mit 32 Jahren. Beide Faktoren werden mit der Referenzbewertung multipliziert. Dies ist keine Messung eines individuellen Auges.",
        "The existing Spectran model shows lower ocular transmission at short wavelengths and a smaller pupil area at age 65 than at age 32. Both factors multiply the reference assessment. This is not a measurement of an individual's eye.")),
      application(
      tr("Das Modellszenario einordnen", "Interpreting the model scenario"),
      formula(tr("Altersbezogene Bewertung = Referenzbewertung \u00d7 Transmissionsfaktor \u00d7 Pupillenfaktor", "Age-adjusted assessment = reference assessment \u00d7 transmission factor \u00d7 pupil factor")),
      note(tr("Das ist ein Altersmodell, keine Messung eines individuellen Auges. Die Einstellungen der Darstellung ver\u00e4ndern die Ansicht; die Alterswahl bestimmt das Modellszenario.",
        "This is an age model, not a measurement of an individual's eye. Display settings change the presentation; the age selection determines the model scenario."))),
      refs(link("DIN/TS 5031-100:2021-11", URL_DIN), " \u00b7 ", link("CIE S 026:2018", URL_CIE))),
    workflow = htmltools::tagList(
      p(
      tr("Ein Vergleich beginnt mit einer passenden Lichtquelle und einer klaren Bezugsgr\u00f6\u00dfe. Legen Sie beides fest, bevor Sie Ergebnisse auswerten und weitergeben.", "A comparison starts with an appropriate light source and a clear reference level. Set both before analysing and sharing results.")),
      cards(
      card(tr("1 \u00b7 Lichtquelle w\u00e4hlen", "1 \u00b7 Choose a light source"), tr("Importieren Sie eine CSV-Datei, w\u00e4hlen Sie ein Beispielspektrum oder konstruieren Sie eine Lichtquelle. Pr\u00fcfen Sie bei Dateien die Spalten, Einheiten und Hinweise der Importpr\u00fcfung.",
          "Import a CSV file, choose an example spectrum or construct a light source. For files, check columns, units and the import validation messages.")),
      card(tr("2 \u00b7 Niveau festlegen", "2 \u00b7 Set the level"), rich("Eine Skalierung multipliziert alle Wellenl\u00e4ngen mit demselben Faktor. Damit \u00e4ndert sich die Lichtmenge, w\u00e4hrend die Spektralform gleich bleibt. Im Materialmodul kann das Zielniveau als E<sub>v</sub> oder melanopische EDI vorgegeben werden.",
          "Scaling multiplies all wavelengths by the same factor. It changes the light level while preserving the spectral shape. In the material module, the target can be set as E<sub>v</sub> or melanopic EDI."))),
      spectran_help_figure("09-import-skalierung-export", tr(
        "Drei Schritte f\u00fchren von CSV, Beispiel oder konstruierter Lichtquelle \u00fcber eine einheitliche Skalierung zum Export von Abbildungen und Tabellen. Ein Vergleich kann auf gleichem Ev, gleicher melanopischer EDI oder den Originalwerten beruhen.",
        "Three steps lead from a CSV, example or constructed light source through uniform scaling to exported figures and tables. Comparisons can use equal Ev, equal melanopic EDI or original values.")),
      application(
      tr("3 \u00b7 Auswerten und exportieren", "3 \u00b7 Analyse and export"),
      p(
      tr("Vergleichen Sie Kenngr\u00f6\u00dfen zusammen mit dem Spektrum und exportieren Sie die gew\u00fcnschten Abbildungen oder Tabellen. Im Materialmodul w\u00e4hlen Sie zus\u00e4tzlich, welches aktuelle oder gespeicherte Ergebnis exportiert wird.",
          "Compare metrics alongside the spectrum and export the figures or tables you need. In the material module, also select which current or saved result to export.")),
      note(tr("F\u00fcr einen fairen Vergleich muss die Bezugsgr\u00f6\u00dfe feststehen: gleiche Beleuchtungsst\u00e4rke, gleiche melanopische EDI oder die unver\u00e4ndert gemessenen Werte. Das sind unterschiedliche Fragestellungen.",
        "A fair comparison needs a clear reference: equal illuminance, equal melanopic EDI or the original measured values. These answer different questions.")))),
    material = htmltools::tagList(
      p(tr("Trifft Licht auf ein Material, kann ein Anteil hindurchtreten, ein Anteil zur\u00fcckgeworfen und ein Anteil absorbiert werden. Im Modul w\u00e4hlen Sie Transmission oder Reflexion f\u00fcr den n\u00e4chsten Schritt.",
        "When light reaches a material, some may pass through, some may be reflected and some absorbed. In the module, choose transmission or reflection for the next step.")),
      spectran_help_figure("03-gemeinsamer-lichtweg", tr("Ein einfallender Lichtstrahl trifft auf ein Material. Transmission f\u00fchrt hindurch, Reflexion zur\u00fcck auf die Einfallsseite. Die Richtungen sind schematisch; Absorption ist nicht eingezeichnet.",
        "Incident light reaches a material. Transmission passes through; reflection returns to the incident side. Directions are schematic and absorption is omitted.")),
      note(tr("Die Pfeile erl\u00e4utern die Lichtwege. Spectran berechnet daraus keine Winkelverteilung, Raumgeometrie oder Mehrfachreflexion im Raum.",
        "The arrows explain the light paths. Spectran does not derive angular distributions, room geometry or room interreflections from them."))),
    interactions = htmltools::tagList(
      cards(
        card("Transmission", rich("Das einfallende Spektrum E<sub>\u03bb</sub> wird mit dem spektralen Transmissionsgrad \u03c4<sub>\u03bb</sub> multipliziert. Die Ausgangsgr\u00f6\u00dfe E\u2032<sub>\u03bb</sub> ist eine spektrale Bestrahlungsst\u00e4rke.",
          "The incident spectrum E<sub>\u03bb</sub> is multiplied by spectral transmittance \u03c4<sub>\u03bb</sub>. The output E\u2032<sub>\u03bb</sub> is a spectral irradiance.")),
        card(tr("Reflexion", "Reflection"), rich("Das einfallende Spektrum E<sub>\u03bb</sub> wird mit dem spektralen Reflexionsgrad \u03c1<sub>\u03bb</sub> multipliziert. Die Ausgangsgr\u00f6\u00dfe am Material ist M<sub>\u03bb</sub>, die reflektierte spektrale spezifische Ausstrahlung. Die Bestrahlungsst\u00e4rke am Empf\u00e4nger ben\u00f6tigt zus\u00e4tzlich eine geometrische Annahme.",
          "The incident spectrum E<sub>\u03bb</sub> is multiplied by spectral reflectance \u03c1<sub>\u03bb</sub>. The output at the material is M<sub>\u03bb</sub>, reflected spectral radiant exitance. Irradiance at the receiver also needs a geometrical assumption."))),
      spectran_help_figure("01-strahlenwege", tr("Transmission: E\u2032\u03bb = \u03c4\u03bb \u00d7 E\u03bb. Reflexion: M\u03bb = \u03c1\u03bb \u00d7 E\u03bb. Das einfallende Licht ist gelb, Transmission gr\u00fcn und Reflexion violett gekennzeichnet.",
        "Transmission: E\u2032\u03bb = \u03c4\u03bb \u00d7 E\u03bb. Reflection: M\u03bb = \u03c1\u03bb \u00d7 E\u03bb. Incident light is yellow, transmission green and reflection purple."))),
    material_spectra = htmltools::tagList(
      p(tr("Die Materialkurve beschreibt den Anteil, der bei jeder Wellenl\u00e4nge durchgelassen oder reflektiert wird. Die Multiplikation erfolgt Wellenl\u00e4nge f\u00fcr Wellenl\u00e4nge. Eine wellenl\u00e4ngenabh\u00e4ngige Kurve kann deshalb sowohl die Lichtmenge als auch die Spektralform ver\u00e4ndern.",
        "The material curve describes the fraction transmitted or reflected at each wavelength. Multiplication is performed wavelength by wavelength. A wavelength-dependent curve can therefore change both the light level and the spectral shape.")),
      spectran_help_figure("02-spektren", tr("Zwei schematische Zeilen zeigen: einfallendes Spektrum mal Transmissionsgrad ergibt transmittiertes Spektrum; einfallendes Spektrum mal Reflexionsgrad ergibt reflektiertes Spektrum. Eingangs- und Ausgangsspektren haben dieselbe Skalierung.",
        "Two illustrative rows show: incident spectrum times transmittance gives the transmitted spectrum; incident spectrum times reflectance gives the reflected spectrum. Incident and outgoing spectra share one scale.")),
      note(tr("Die gezeigten Kurven sind schematische Beispiele, keine Messdaten. In der Bibliothek gelten die dokumentierten Messbedingungen. Fehlende Wellenl\u00e4ngenbereiche m\u00fcssen bewusst erg\u00e4nzt werden; diese Annahmen geh\u00f6ren zum Ergebnis.",
        "These curves are illustrative examples, not measurements. Library records retain their documented measurement conditions. Missing wavelength ranges need an explicit completion choice; those assumptions belong to the result."))),
    receiver = htmltools::tagList(
      p(tr("F beschreibt hier die idealisierte geometrische Kopplung zwischen Material und Empf\u00e4nger. Der Materialkoeffizient beschreibt die spektrale Materialwirkung; F beschreibt, wie sie im Empfangsszenario ber\u00fccksichtigt wird.",
        "Here, F describes the idealised geometrical coupling between material and receiver. The material coefficient describes the spectral material effect; F describes how it is represented in the receiver scenario.")),
      note(htmltools::strong(tr("Spectran verwendet F = 1. ", "Spectran uses F = 1. ")),
        tr("F\u00fcr die Empfangswerte wird keine zus\u00e4tzliche geometrische Abschw\u00e4chung angesetzt. Die tats\u00e4chliche Geometrie kann die Bestrahlungsst\u00e4rke am Empf\u00e4nger verringern. F ist derzeit keine einstellbare Eingabe; Spectran berechnet keine Raumgeometrie.",
          "No additional geometrical attenuation is applied to the receiver values. Actual geometry can reduce the irradiance at the receiver. F is currently not an adjustable input; Spectran does not calculate room geometry.")),
      spectran_help_figure("04-material-und-empfaenger", tr("Materialwirkung und Empfang sind getrennt: E\u2032\u03bb beziehungsweise M\u03bb wird mit F zur Empf\u00e4nger-Bestrahlungsst\u00e4rke verkn\u00fcpft. Spectran verwendet F = 1.",
        "Material effect and reception are separate: E\u2032\u03bb or M\u03bb is related to receiver irradiance through F. Spectran uses F = 1.")),
      h3(tr("Besonderheit bei Reflexion", "For reflection")),
      p(rich("M<sub>\u03bb</sub> beschreibt die vom Material abgegebene Strahlung pro Fl\u00e4che, E<sub>\u03bb,Empf</sub> die am Empf\u00e4nger eintreffende Strahlung pro Fl\u00e4che. F\u00fcr E<sub>\u03bb,Empf</sub> = F \u00d7 M<sub>\u03bb</sub> wird eine homogen beleuchtete, diffus reflektierende Fl\u00e4che angenommen. Bei F = 1 f\u00fcllt sie idealisiert die gesamte vom Empf\u00e4nger gesehene Hemisph\u00e4re aus. F\u00fcr gerichtete Reflexion ist diese vereinfachte Beziehung keine allgemeine Raumsimulation.",
        "M<sub>\u03bb</sub> describes radiation leaving the material per unit area; E<sub>\u03bb,rec</sub> describes radiation reaching the receiver per unit area. E<sub>\u03bb,rec</sub> = F \u00d7 M<sub>\u03bb</sub> assumes a uniformly illuminated, diffusely reflecting surface. At F = 1, it ideally fills the receiver's entire viewed hemisphere. For directional reflection, this simplified relationship is not a general room simulation."))),
    path = htmltools::tagList(
      p(
      tr("Ein Lichtpfad verbindet die Ursprungslichtquelle mit gespeicherten Materialergebnissen. Entscheidend ist, ob Sie einen einzelnen Schritt oder die gesamte Ver\u00e4nderung seit dem Start betrachten.", "A light path connects the original light source to saved material results. Choose whether to examine one step or the full change since the start.")),
      cards(
        card(tr("Ein Ergebnis behalten", "Keep a result"), tr("Berechnen erzeugt zun\u00e4chst das aktuelle Ergebnis. \u00dcber \u201eErgebnis speichern / Material hinzuf\u00fcgen\u201c k\u00f6nnen Sie es im Lichtpfad speichern oder als Lichtquelle f\u00fcr den n\u00e4chsten Schritt verwenden. Gespeichert bedeutet hier: innerhalb der laufenden Sitzung. Nutzen Sie Export f\u00fcr eine Datei.",
          "Calculating first produces the current result. Use \u201cSave result / add material\u201d to save it in the light path or use it as the source for the next step. Saved here means within the current session. Use Export to keep a file.")),
        card(tr("Einzelschritt oder Gesamtwirkung", "Single step or combined effect"), tr("Das gespeicherte Materialergebnis vergleicht den jeweiligen Eingang mit diesem Ausgang. Die Gesamtwirkung vergleicht die Ursprungslichtquelle mit dem ausgew\u00e4hlten Schritt. Bei Verzweigungen wird nur dessen eigener Pfad gezeigt.",
          "The saved material result compares that step's input with its output. The combined effect compares the original source with the selected step. For branches, only that step's own path is shown."))),
      spectran_help_figure("10-schritte-und-gesamtwirkung", tr(
        "Die Ursprungsquelle und drei Materialschritte bilden einen Lichtpfad. Das Diagramm zeigt den Start in hellen Spektralfarben, zwei Zwischenstufen mit verschiedenen Linienarten und den ausgew\u00e4hlten Schritt 3 mit vollen Spektralfarben und durchgezogener Linie. Einzelschritt vergleicht hier Schritt 2 mit 3; Gesamtwirkung vergleicht den Start mit 3.",
        "The original source and three material steps form a light path. The plot uses pale spectral colours for the start, distinct line styles for the two intermediate steps, and full spectral colours with a solid line for selected step 3. The single-step view compares step 2 with 3; the combined effect compares the start with 3.")),
      application(
      tr("Das Lichtpfad-Diagramm lesen", "Reading the light-path plot"),
      p(tr("Der Start hat helle Spektralfarben. Der ausgew\u00e4hlte letzte Schritt hat volle Spektralfarben und eine durchgezogene Linie. Die Legende unterscheidet die Zwischenschritte. L\u00e4ngere Lichtpfade lassen sich auf kleinen Bildschirmen schwerer vergleichen. Ein fr\u00fcherer Schritt zeigt eine Teilansicht; das PNG bietet eine gr\u00f6\u00dfere Ansicht. Alle Schritte bleiben erhalten.",
        "The start uses pale spectral colours. The selected final step uses full spectral colours and a solid line. The legend distinguishes intermediate steps. Longer paths can be harder to compare on small screens. Select an earlier step for a partial view or use the PNG for a larger view. All steps are retained.")),
      note(rich("Wird ein Zwischenschritt auf ein neues Lichtniveau skaliert, ist der weitere Pfad kein reiner passiver Materialverlust mehr. Beachten Sie den Hinweis zur Skalierung. Der effektive MDER in der Gesamtbewertung verwendet E<sub>v</sub> der Ursprungsquelle als Nenner und ist daher vom DER des austretenden Lichts zu unterscheiden.",
        "If an intermediate step is rescaled to a new light level, the subsequent path no longer represents passive material loss alone. Check the rescaling note. Effective MDER in the combined assessment uses the original source's E<sub>v</sub> as its denominator, so it differs from the DER of the outgoing light.")))))
}

spectran_explanations_dependency <- function() {
  htmltools::htmlDependency("spectran-explanations", "1.0.0",
    src = c(file = system.file("app/www", package = "Spectran")),
    stylesheet = "spectran-explanations.css", all_files = FALSE)
}

spectran_explanations_ui <- function(id) {
  ns <- shiny::NS(id)
  tr <- spectran_help_text
  topics <- spectran_explanation_topics()
  choices <- lapply(c("basics", "materials"), function(group) {
    keys <- names(topics)[vapply(topics, function(x) x$group == group, logical(1))]
    stats::setNames(keys, vapply(topics[keys], `[[`, character(1), "title"))
  })
  names(choices) <- c(tr("Spektren und Kenngr\u00f6\u00dfen", "Spectra and metrics"), tr("Material und Lichtpfad", "Materials and light paths"))
  ui <- htmltools::div(class = "spectran-help-page", lang = if (transmission_language_setting() == "Deutsch") "de" else "en",
    shinyjs::useShinyjs(),
    htmltools::tags$header(class = "spectran-help-header",
      htmltools::p(class = "spectran-help-eyebrow", "LiTG Spectran"),
      htmltools::h2(id = ns("heading"), tabindex = "-1", tr("Erl\u00e4uterungen", "Explanations")),
      htmltools::p(tr("Licht verstehen, Ergebnisse einordnen und sicher durch Spectran navigieren.",
        "Understand light, interpret results and find your way around Spectran."))),
    htmltools::div(class = "spectran-help-toolbar",
      shiny::uiOutput(ns("return_ui")),
      shiny::actionButton(ns("home"), tr("Themen\u00fcbersicht", "All topics"), icon = shiny::icon("table-cells-large"))),
    shiny::selectInput(ns("topic"), tr("Thema", "Topic"),
      choices = c(stats::setNames(list("home"), tr("Themen\u00fcbersicht", "All topics")), choices),
      selected = "home", width = "100%", selectize = FALSE),
    shiny::uiOutput(ns("content")),
    shiny::uiOutput(ns("topic_navigation")))
  htmltools::attachDependencies(ui, spectran_explanations_dependency())
}

# active_page is a reactive main-page key; navigate(page) changes only the main
# navigation. All inputs on the caller's page remain mounted and unchanged.
# The returned open_from(topic, page, control_id) also supports entry controls
# owned by another module, retaining their explicit focus target for Back.
spectran_explanations_server <- function(id, active_page, navigate) {
  stopifnot(shiny::is.reactive(active_page), is.function(navigate))
  shiny::moduleServer(id, function(input, output, session) {
    tr <- spectran_help_text
    topics <- spectran_explanation_topics()
    page_names <- c(tutorial = tr("zur Einf\u00fchrung", "to Introduction"), import = tr("zum Import", "to Import"),
      analysis = tr("zur Auswertung", "to Analysis"), export = tr("zum Export", "to Export"),
      transmission = tr("zum Materialmodul", "to Transmission / Reflection"),
      validity = tr("zur Validit\u00e4t", "to Validity"), impressum = tr("zum Impressum", "to About"))
    previous <- shiny::reactiveVal("tutorial")
    topic <- shiny::reactiveVal("home")
    origin <- shiny::reactiveVal(NULL)
    pending_return <- shiny::reactiveVal(NULL)
    # Reuse the app's existing focus primitive. Waiting for the main navigation
    # input confirms that the destination is visible before focusing it. Native
    # focus also brings the heading and nearby navigation into the viewport.
    shiny::observeEvent(active_page(), {
      page <- active_page()
      if (identical(page, "explanations")) {
        transmission_focus_element(session$ns("heading"))
      } else if (page %in% names(page_names)) {
        previous(page)
        target <- pending_return()
        pending_return(NULL)
        origin(NULL)
        if (!is.null(target) && identical(target$page, page)) transmission_focus_element(target$id)
      }
    }, ignoreInit = FALSE)
    open_topic <- function(value) {
      stopifnot(value %in% c("home", names(topics)))
      topic(value)
      shiny::updateSelectInput(session, "topic", selected = value)
      if (identical(active_page(), "explanations")) transmission_focus_element(session$ns("heading"))
      navigate("explanations")
    }
    open_from <- function(value, page, control_id) {
      stopifnot(value %in% c("home", names(topics)), page %in% names(page_names),
        is.character(control_id), length(control_id) == 1L, !is.na(control_id), nzchar(control_id))
      origin(list(page = page, id = control_id))
      open_topic(value)
    }
    for (key in names(spectran_explanation_routes())) local({
      action <- key
      target <- spectran_explanation_routes()[[action]]
      shiny::observeEvent(input[[action]], {
        origin(list(page = active_page(), id = session$ns(action)))
        open_topic(target)
      }, ignoreInit = TRUE)
    })
    for (key in names(topics)) local({
      target <- key
      shiny::observeEvent(input[[paste0("topic_", target)]], open_topic(target), ignoreInit = TRUE)
    })
    shiny::observeEvent(input$topic, {
      if (input$topic %in% c("home", names(topics))) topic(input$topic)
    })
    move_topic <- function(offset) {
      index <- match(topic(), names(topics))
      if (is.na(index)) return(invisible(NULL))
      target <- index + offset
      if (target >= 1L && target <= length(topics)) open_topic(names(topics)[target])
    }
    shiny::observeEvent(input$previous_topic, {
      shiny::req(input$previous_topic > 0)
      move_topic(-1L)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$next_topic, {
      shiny::req(input$next_topic > 0)
      move_topic(1L)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$home, open_topic("home"))
    shiny::observeEvent(input$back, {
      pending_return(origin())
      navigate(previous())
    })
    output$return_ui <- shiny::renderUI(shiny::actionButton(session$ns("back"),
      paste(tr("Zur\u00fcck", "Back"), page_names[[previous()]]), icon = shiny::icon("arrow-left")))
    output$topic_navigation <- shiny::renderUI({
      index <- match(topic(), names(topics))
      if (is.na(index)) return(NULL)
      navigation_button <- function(direction) {
        is_next <- identical(direction, "next")
        target <- index + if (is_next) 1L else -1L
        available <- target >= 1L && target <= length(topics)
        label <- htmltools::span(class = "spectran-help-step-label",
          htmltools::span(class = "spectran-help-step-direction",
            if (is_next) tr("N\u00e4chstes Thema", "Next topic") else tr("Vorheriges Thema", "Previous topic")),
          htmltools::strong(if (available) topics[[target]]$title else
            if (is_next) tr("Letztes Thema", "Last topic") else tr("Erstes Thema", "First topic")))
        arrow <- shiny::icon(if (is_next) "arrow-right" else "arrow-left")
        shiny::actionButton(session$ns(paste0(direction, "_topic")),
          if (is_next) htmltools::tagList(label, arrow) else htmltools::tagList(arrow, label),
          class = paste("spectran-help-step", paste0("spectran-help-step-", direction)),
          disabled = !available)
      }
      htmltools::tags$nav(class = "spectran-help-pagination",
        `aria-label` = tr("Durch die Erl\u00e4uterungen bl\u00e4ttern", "Browse explanation topics"),
        htmltools::p(class = "spectran-help-progress",
          sprintf(tr("Thema %d von %d", "Topic %d of %d"), index, length(topics))),
        htmltools::div(class = "spectran-help-step-buttons",
          navigation_button("previous"), navigation_button("next")))
    })
    output$content <- shiny::renderUI({
      value <- topic()
      if (identical(value, "home")) return(htmltools::tagList(lapply(c("basics", "materials"), function(group) {
        keys <- names(topics)[vapply(topics, function(x) x$group == group, logical(1))]
        htmltools::tags$section(
          htmltools::h3(if (group == "basics") tr("Spektren und Kenngr\u00f6\u00dfen", "Spectra and metrics") else tr("Material und Lichtpfad", "Materials and light paths")),
          htmltools::div(class = "spectran-help-topic-grid", lapply(keys, function(key) {
            item <- topics[[key]]
            shiny::actionButton(session$ns(paste0("topic_", key)),
              htmltools::tagList(shiny::icon(item$icon), htmltools::strong(item$title), htmltools::span(item$summary)),
              class = "spectran-help-topic")
          })))
      })))
      htmltools::tags$article(class = "spectran-help-article",
        htmltools::h2(topics[[value]]$title),
        spectran_explanation_content(value))
    })
    list(topic = shiny::reactive(topic()), previous_page = shiny::reactive(previous()),
      open_from = open_from)
  })
}

# Retained isolated navigation showcase; no spectrum or external service needed.
spectran_explanations_app <- function(language = "Deutsch") {
  the$language <- language
  shiny::addResourcePath("extr", system.file("app/www", package = "Spectran"))
  links <- spectran_explanation_links_ui("help")
  ui <- shiny::fluidPage(shiny::tabsetPanel(id = "page", type = "hidden",
    shiny::tabPanel("Source", value = "analysis", shiny::h2("Explanation navigation showcase"),
      shiny::textInput("draft", "Draft value, preserved when returning", "unchanged"), links$from_quantities),
    shiny::tabPanel("Help", value = "explanations", spectran_explanations_ui("help"))))
  server <- function(input, output, session) {
    spectran_explanations_server("help", shiny::reactive(input$page),
      function(page) shiny::updateTabsetPanel(session, "page", selected = page))
  }
  shiny::shinyApp(ui, server)
}
