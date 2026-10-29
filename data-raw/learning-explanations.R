# Generate approved static learning illustrations at development time only.
# Run from the package root in the project R environment. No PNGs are shipped.
library(grid)
repo <- normalizePath(".")
out <- "inst/app/www/explanations"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
load("data/Specs.rda")
load("data/ColorP.rda")
source("R/analysis_helpers_age.R")
learning_english <- c(
  "SPECTRAN  / SPEKTREN UND KENNGRÖSSEN" = "SPECTRAN  / SPECTRA AND METRICS",
  "SPECTRAN  / MATERIAL UND LICHTPFAD" = "SPECTRAN  / MATERIALS AND LIGHT PATHS",
  "Ein Spektrum lesen." = "Reading a spectrum.",
  "Die Höhe gehört zu einer Wellenlänge. Die Fläche fasst einen Wellenlängenbereich zusammen." = "Height describes one wavelength. Area brings together a range of wavelengths.",
  "Wellenlänge λ (nm)" = "Wavelength λ (nm)",
  "Höhe bei λ" = "Height at λ", "Fläche unter der Kurve" = "Area under the curve",
  "HÖHE" = "HEIGHT", "FLÄCHE" = "AREA",
  "Ein hoher, schmaler Peak\nkann wenig Fläche beitragen." = "A tall, narrow peak can\ncontribute little area.",
  "Über 380–780 nm integriert:\nBestrahlungsstärke in mW/m²." = "Integrated over 380–780 nm:\nirradiance in mW/m².",
  "Einheit und Achsenskalierung mitlesen." = "Check the units and axis scales.",
  "VOM SPEKTRUM ZUR GEWICHTETEN KENNGRÖSSE" = "FROM THE SPECTRUM TO A WEIGHTED METRIC",
  "Spektrum" = "Spectrum", "Gewichtetes Spektrum" = "Weighted spectrum",
  "Integrieren und\nEinheit umrechnen" = "Integrate and\nconvert units",
  "Spektrum: künstliches Beispiel. V(λ): photopische Empfindlichkeit. Sie ist keine zusätzliche Strahlung." = "Spectrum: a synthetic example. V(λ): photopic sensitivity, not additional radiation.",
  "Spektralfarben dienen der Orientierung; ein Bildschirm gibt monochromatisches Licht nicht farbgetreu wieder." = "Spectral colours are a visual guide. A screen cannot faithfully reproduce monochromatic light.",
  "Ein Spektrum. Mehrere Bewertungen." = "One spectrum. Several assessments.",
  "Bestrahlungsstärke, Beleuchtungsstärke und EDI beschreiben unterschiedliche Aspekte desselben Lichts." = "Irradiance, illuminance and EDI describe different aspects of the same light.",
  "LICHT AM EMPFANGSORT" = "LIGHT AT THE RECEIVER", "Dasselbe Spektrum" = "The same spectrum",
  "OHNE EMPFINDLICHKEITSGEWICHTUNG" = "WITHOUT SENSITIVITY WEIGHTING",
  "GEWICHTET MIT V(λ)" = "WEIGHTED BY V(λ)", "MELANOPISCH GEWICHTET" = "MELANOPICALLY WEIGHTED",
  "Über die Wellenlängen integrieren" = "Integrate over wavelength",
  "Integrieren und in Lux umrechnen" = "Integrate and convert to lux",
  "Auf Referenztageslicht D65 beziehen" = "Relate to reference daylight D65",
  "Bestrahlungsstärke" = "Irradiance", "Beleuchtungsstärke Ev" = "Illuminance Ev", "Melanopische EDI" = "Melanopic EDI",
  "D65 mit gleicher melanopischer Bestrahlungsstärke" = "D65 with the same melanopic irradiance",
  "Lichtniveau verdoppeln" = "Double the light level",
  "Spektralform bleibt gleich." = "The spectral shape stays the same.",
  "DER = melanopische EDI / Ev" = "DER = melanopic EDI / Ev",
  "100 lx × 0,6 = 60 lx" = "100 lx × 0.6 = 60 lx", "200 lx × 0,6 = 120 lx" = "200 lx × 0.6 = 120 lx",
  "DER bleibt 0,6 · dimensionslos" = "DER stays at 0.6 · dimensionless",
  "Rechenbeispiel, keine Messwerte. Bei Ev = 0 ist DER nicht definiert. Keine Vorhersage einer individuellen Wirkung." = "Illustrative numbers, not measurements. DER is undefined when Ev = 0. This does not predict an individual's response.",
  "EDI und DER gibt es für alle fünf α-opischen Bewertungen: melanopisch, rhodopisch sowie S-, M- und L-Zapfen." = "EDI and DER exist for all five α-opic assessments: melanopic, rhodopic, S-cone, M-cone and L-cone.",
  "Lichtfarbe und Farbwiedergabe." = "Light colour and colour rendering.",
  "CCT ordnet die Lichtfarbe ein. Ra und Ri vergleichen die Wiedergabe von Testfarben mit einer Referenz." = "CCT describes light colour. Ra and Ri compare the rendering of test colours with a reference.",
  "Wie erscheint das Licht?" = "How does the light look?", "Wie erscheinen Farben?" = "How do colours look?",
  "warmweiß" = "warm white", "neutralweiß" = "neutral white", "kaltweiß" = "cool white",
  "niedrigere CCT" = "lower CCT", "höhere CCT (K)" = "higher CCT (K)",
  "Dieselben Oberflächen, verschieden beleuchtet" = "The same surfaces under different lighting",
  "Referenz" = "Reference", "Testlicht" = "Test light",
  "Ri: Vergleich für jede einzelne Testfarbe" = "Ri: comparison for each test colour",
  "Ra fasst R1 bis R8 zusammen" = "Ra summarises R1 to R8", "Mittelwert" = "Mean",
  "R9: gesättigtes Rot" = "R9: saturated red", "nicht in Ra enthalten" = "not included in Ra",
  "Farben sind schematisch, keine farbmetrische Simulation. Gleiche CCT bedeutet weder gleiche Spektren noch gleiche DER." = "Colours are schematic, not a colourimetric simulation. Equal CCT does not imply equal spectra or equal DER.",
  "CCT ist nahe dem Planckschen Kurvenzug sinnvoll. Außerhalb der CIE-Anwendungsgrenzen können Werte fehlen." = "CCT is meaningful near the Planckian locus. Values may be unavailable outside the CIE applicability limits.",
  "Alter und Auge: zwei Einflüsse." = "Age and the eye: two influences.",
  "Spectran verbindet die spektrale Transmission der Augenmedien mit einem Modell der Pupillengröße." = "Spectran combines spectral transmission through the ocular media with a model of pupil size.",
  "AUGENMEDIEN" = "OCULAR MEDIA", "PUPILLE" = "PUPIL",
  "Durchlässigkeit hängt von λ und Alter ab." = "Transmission varies with λ and age.",
  "32 Jahre" = "32 years", "65 Jahre" = "65 years",
  "Die Öffnung beeinflusst die Lichtmenge." = "Opening size affects the light level.",
  "Modellbeispiel mit kleinerer Pupillenfläche" = "Model example with a smaller pupil area",
  "DIE BEIDEN FAKTOREN WERDEN VERKNÜPFT" = "THE TWO FACTORS ARE COMBINED",
  "Referenzbewertung\nmit 32 Jahren" = "Reference assessment\nat 32 years",
  "Transmissions-\nfaktor" = "Transmission\nfactor", "Pupillen-\nfaktor" = "Pupil\nfactor",
  "Altersbezogene\nBewertung" = "Age-adjusted\nassessment",
  "Modellbeispiel für 65 Jahre, keine Messung eines individuellen Auges. Beide Korrekturfaktoren sind mit 32 Jahren gleich 1." = "Model example for age 65, not a measurement of an individual's eye. Both correction factors equal 1 at age 32.",
  "Der melanopische Transmissionsfaktor hängt zusätzlich vom untersuchten Lichtspektrum ab. Modell: DIN/TS 5031-100." = "The melanopic transmission factor also depends on the light spectrum. Model: DIN/TS 5031-100.",
  "Vom Import zum vergleichbaren Ergebnis." = "From import to comparable results.",
  "Lichtquelle wählen, Bezugsgröße festlegen und die passende Auswertung als Datei behalten." = "Choose a light source, set a comparison basis and keep the relevant results as a file.",
  "Lichtquelle" = "Light source", "Lichtniveau" = "Light level", "Ausgabe" = "Output",
  "CSV  ·  Beispiel  ·  Konstruktion" = "CSV  ·  Example  ·  Construction",
  "Spalten und Einheiten prüfen.\nImporthinweise beachten." = "Check columns and units.\nRead the import guidance.",
  "Alle Wellenlängen erhalten\ndenselben Skalierungsfaktor." = "Every wavelength receives\nthe same scaling factor.",
  "Spektralform bleibt erhalten." = "The spectral shape is preserved.",
  "Abbildungen + Tabellen" = "Figures + tables", "Export als Datei" = "Export to a file",
  "Vor einem Vergleich festlegen:" = "Choose a basis before comparing:",
  "gleiches Ev" = "equal Ev", "gleiche melanopische EDI" = "equal melanopic EDI", "Originalwerte" = "original values",
  "Die drei Bezugsgrößen beantworten unterschiedliche Fragen. Die Skalierung oben ist ein künstliches Beispiel." = "These three comparison bases answer different questions. The scaling above is a synthetic example.",
  "Im Materialmodul kann das Zielniveau als Ev oder melanopische EDI vorgegeben und das Ergebnis für den Export gewählt werden." = "In the material module, set a target Ev or melanopic EDI and choose which result to export.",
  "Einzelschritt und Gesamtwirkung unterscheiden." = "One step and the combined effect.",
  "Ein Lichtpfad behält die Ursprungsquelle und die gespeicherten Materialschritte zusammen." = "A light path keeps the original source and the saved material steps together.",
  "Ursprungsquelle" = "Original source", "Schritt 1" = "Step 1", "Schritt 2" = "Step 2", "Schritt 3" = "Step 3",
  "nach Material 1" = "after material 1", "nach Material 2" = "after material 2", "ausgewählt" = "selected",
  "Gesamtwirkung: Start → ausgewählter Schritt" = "Combined effect: start → selected step",
  "SPEKTREN ENTLANG DIESES PFADS" = "SPECTRA ALONG THIS PATH",
  "Spektrale Bestrahlungsstärke (rel. Skala)" = "Spectral irradiance (relative scale)",
  "Wellenlänge λ" = "Wavelength λ", "EINZELSCHRITT" = "SINGLE STEP", "GESAMTWIRKUNG" = "COMBINED EFFECT",
  "Eingang von Schritt 3 → Schritt 3" = "Input to step 3 → step 3",
  "Hier: Schritt 2 mit Schritt 3 vergleichen." = "Here: compare step 2 with step 3.",
  "Ursprungsquelle → Schritt 3" = "Original source → step 3",
  "Bei Verzweigungen nur diesen Pfad zeigen." = "For branches, show only this path.",
  "Künstliches Beispiel: 3 passive Schritte, keine Neuskalierung, F = 1. Neuskalieren verändert den Vergleich vom Start aus." = "Synthetic example: 3 passive steps, no rescaling, F = 1. Rescaling changes the comparison from the start.",
  "Berechnen → Ergebnis speichern / Material hinzufügen. Gespeichert wird in der Sitzung; für eine Datei den Export verwenden." = "Calculate → Save result / add material. Results are saved for this session; use Export to keep a file."
)
ink <- "#252C32"; muted <- "#59616A"; border <- "#DDE1E3"
yellow <- "#F8E350"; ochre <- "#8A7000"; teal <- "#127C77"
purple <- "#934879"; blue <- "#286898"; bg <- "#F4F6F7"
u <- function(x) unit(x, "native")
txt <- function(x, y, label, size = 20, col = ink, face = "plain", just = "left", rot = 0) {
  if (language == "en" && is.character(label) && label %in% names(learning_english)) label <- unname(learning_english[[label]])
  grid.text(label, x = u(x), y = u(y), just = just, rot = rot,
    gp = gpar(fontfamily = "Arial", fontsize = size * .72, col = col,
      fontface = face, lineheight = 1.28))
}
box <- function(x, y, w, h, fill = "white", stroke = border, radius = 12, lw = 1) {
  grid.roundrect(x = u(x), y = u(y), width = u(w), height = u(-h),
    just = c("left", "top"), r = unit(radius * .72, "pt"),
    gp = gpar(fill = fill, col = stroke, lwd = lw * .72))
}
line <- function(x, y, col = ink, width = 2, dash = "solid", arrow = FALSE) {
  grid.lines(x = u(x), y = u(y),
    arrow = if (arrow) grid::arrow(type = "closed", angle = 25, length = unit(8, "pt")),
    gp = gpar(col = col, fill = col, lwd = width * .72, lty = dash,
      lineend = "round", linejoin = "round"))
}
arr <- function(x1, y1, x2, y2, col = muted, width = 2.4, dash = "solid") {
  line(c(x1, x2), c(y1, y2), col, width, dash, TRUE)
}
circle <- function(x, y, r, fill, stroke = NA, lw = 1) {
  grid.circle(u(x), u(y), u(r), gp = gpar(fill = fill, col = stroke, lwd = lw * .72))
}
chip <- function(x, y, w, label, fill = "#FFF5BA", col = ink, size = 16) {
  box(x, y, w, 32, fill, NA, 16)
  txt(x + w / 2, y + 16, label, size, col, "bold", "centre")
}
header <- function(title, subtitle, topic = "SPEKTREN UND KENNGRÖSSEN") {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, 1200), yscale = c(800, 0)))
  box(0, 0, 1200, 800, bg, NA, 0)
  box(48, 36, 28, 7, yellow, NA, 0)
  txt(88, 40, paste("SPECTRAN  /", topic), 14, muted, "bold")
  txt(48, 92, title, 36, face = "bold")
  txt(48, 132, subtitle, 19, muted)
}
footer <- function(lines) {
  line(c(48, 1152), c(712, 712), border, 1)
  for (i in seq_along(lines)) txt(48, 736 + 23 * (i - 1), lines[[i]], 15.5, muted)
}
wl <- 380:780
source_curve <- .16 + .65 * exp(-((wl - 460) / 22)^2) + .53 * exp(-((wl - 610) / 93)^2)
v_lambda <- Specs$AS_wide[["V(lambda)"]]
stopifnot(identical(as.numeric(Specs$AS_wide$Wellenlaenge), as.numeric(wl)))
colour_band <- unname(ColorP$Lang[as.character(wl[-length(wl)])])
stopifnot(length(colour_band) == 400, !anyNA(colour_band))
plot_xy <- function(x, y, w, h, values, ymax = 1, col = ink, dash = "solid", width = 2.8) {
  line(x + (wl - 380) / 400 * w, y + h - values / ymax * h, col, width, dash)
}
fill_spectrum <- function(x, y, w, h, values, ymax = 1, pale = FALSE) {
  cols <- if (pale) vapply(colour_band, function(z) {
    rgbv <- grDevices::col2rgb(z) / 255
    grDevices::rgb(t(.2 * rgbv + .8))
  }, character(1)) else colour_band
  xx <- x + (wl - 380) / 400 * w
  yy <- y + h - values / ymax * h
  # A same-colour stroke covers subpixel anti-alias seams between adjacent
  # wavelength strips in raster previews. The separate data line remains exact.
  for (i in seq_len(400)) grid.polygon(u(c(xx[i] - .08, xx[i] - .08, xx[i + 1] + .08, xx[i + 1] + .08)),
    u(c(y + h, yy[i], yy[i + 1], y + h)), gp = gpar(fill = cols[i], col = cols[i], lwd = .8))
}
axes <- function(x, y, w, h, ymax = 1, ylabels = TRUE, xlabel = TRUE) {
  for (level in c(.5, 1) * ymax) line(c(x, x + w), rep(y + h - level / ymax * h, 2), "#E8ECEE", 1)
  line(c(x, x, x + w), c(y, y + h, y + h), "#69737A", 1.4)
  if (ylabels) for (level in c(0, .5, 1) * ymax) txt(x - 11, y + h - level / ymax * h,
    format(level, decimal.mark = if (language == "en") "." else ",", trim = TRUE), 14.5, muted, just = "right")
  if (xlabel) {
    for (nm in c(380, 480, 580, 680, 780)) txt(x + (nm - 380) / 400 * w, y + h + 20, nm, 14.5, muted, just = "centre")
    txt(x + w / 2, y + h + 47, "Wellenlänge λ (nm)", 16, muted, just = "centre")
  }
}
mini <- function(x, y, w, h, values, ymax = 1, col = ink, spectral = FALSE, dash = "solid", pale = FALSE) {
  if (spectral) fill_spectrum(x, y, w, h, values, ymax, pale)
  plot_xy(x, y, w, h, values, ymax, col, dash, 2.5)
  line(c(x, x + w), rep(y + h, 2), border, 1)
}

spectrum_graphic <- function() {
  header("Ein Spektrum lesen.", "Die Höhe gehört zu einer Wellenlänge. Die Fläche fasst einen Wellenlängenbereich zusammen.")
  box(48, 174, 720, 326)
  txt(82, 204, if (language == "en") expression(E[lambda]~"· spectral irradiance (mW/m²/nm)") else expression(E[lambda]~"· spektrale Bestrahlungsstärke (mW/m²/nm)"), 18, face = "bold")
  fill_spectrum(105, 239, 618, 185, source_curve)
  axes(105, 239, 618, 185)
  plot_xy(105, 239, 618, 185, source_curve)
  peak <- which.max(source_curve)
  px <- 105 + (wl[peak] - 380) / 400 * 618
  py <- 239 + 185 - source_curve[peak] * 185
  line(c(px, px), c(py, 424), ochre, 1.8, "dotted")
  circle(px, py, 4.5, ochre)
  txt(px + 17, py - 9, "Höhe bei λ", 17, ochre, "bold")
  box(369, 378, 244, 32, "#FFFFFFDE", NA, 8)
  txt(491, 394, "Fläche unter der Kurve", 18, ink, "bold", "centre")
  box(792, 174, 360, 326)
  chip(818, 198, 116, "HÖHE", "#FFF5BA", ochre)
  txt(818, 267, "Ein hoher, schmaler Peak\nkann wenig Fläche beitragen.", 20)
  chip(818, 320, 126, "FLÄCHE", "#E6F3F0", teal)
  txt(818, 391, "Über 380–780 nm integriert:\nBestrahlungsstärke in mW/m².", 20)
  txt(818, 459, "Einheit und Achsenskalierung mitlesen.", 16.5, muted)
  box(48, 524, 1104, 164)
  txt(74, 552, "VOM SPEKTRUM ZUR GEWICHTETEN KENNGRÖSSE", 14.5, muted, "bold")
  mini(78, 591, 180, 51, source_curve, spectral = TRUE)
  txt(169, 665, "Spektrum", 16, just = "centre")
  txt(296, 617, "×", 30, muted, just = "centre")
  mini(334, 589, 180, 53, v_lambda, col = teal)
  txt(424, 665, if (language == "en") expression(V(lambda)~"as an example") else expression(V(lambda)~"als Beispiel"), 16, just = "centre")
  txt(551, 617, "=", 28, muted, just = "centre")
  mini(589, 589, 180, 53, source_curve * v_lambda, col = teal, spectral = TRUE)
  txt(679, 665, "Gewichtetes Spektrum", 16, just = "centre")
  arr(795, 616, 834, 616)
  txt(864, 614, "Integrieren und\nEinheit umrechnen", 21, face = "bold")
  footer(c("Spektrum: künstliches Beispiel. V(λ): photopische Empfindlichkeit. Sie ist keine zusätzliche Strahlung.",
    "Spektralfarben dienen der Orientierung; ein Bildschirm gibt monochromatisches Licht nicht farbgetreu wieder."))
}

quantities_graphic <- function() {
  header("Ein Spektrum. Mehrere Bewertungen.", "Bestrahlungsstärke, Beleuchtungsstärke und EDI beschreiben unterschiedliche Aspekte desselben Lichts.")
  box(48, 176, 272, 347)
  txt(184, 214, "LICHT AM EMPFANGSORT", 14.5, muted, "bold", "centre")
  mini(80, 272, 207, 120, source_curve, spectral = TRUE)
  txt(184, 431, expression(E[lambda]), 32, just = "centre")
  txt(184, 483, "Dasselbe Spektrum", 19, just = "centre")
  line(c(321, 356, 356), c(350, 350, 230), "#99A3A9", 2)
  line(c(356, 356), c(350, 467), "#99A3A9", 2)
  ys <- c(176, 294, 412)
  fills <- c("#F9FAFB", "#FFF9D9", "#EAF5F3")
  titles <- c("OHNE EMPFINDLICHKEITSGEWICHTUNG", "GEWICHTET MIT V(λ)", "MELANOPISCH GEWICHTET")
  methods <- c("Über die Wellenlängen integrieren", "Integrieren und in Lux umrechnen", "Auf Referenztageslicht D65 beziehen")
  results <- c("Bestrahlungsstärke", "Beleuchtungsstärke Ev", "Melanopische EDI")
  units <- c("mW/m²", "lx", "lx")
  for (i in 1:3) {
    arr(356, ys[i] + 55, 391, ys[i] + 55, "#99A3A9", 2)
    box(402, ys[i], 750, 109, fills[i])
    txt(425, ys[i] + 24, titles[i], 13.8, muted, "bold")
    txt(425, ys[i] + 58, methods[i], 18)
    if (i == 3) txt(425, ys[i] + 85, "D65 mit gleicher melanopischer Bestrahlungsstärke", 14.6, muted)
    line(c(862, 862), c(ys[i] + 21, ys[i] + 88), border, 1)
    txt(890, ys[i] + 42, results[i], 19.5, face = "bold")
    txt(890, ys[i] + 77, units[i], 25, if (i == 3) teal else ink, "bold")
  }
  box(48, 547, 1104, 140, "#FFFFFF")
  txt(78, 579, "Lichtniveau verdoppeln", 23, face = "bold")
  txt(78, 615, "Spektralform bleibt gleich.", 18, muted)
  txt(78, 655, "DER = melanopische EDI / Ev", 20, teal, "bold")
  ev <- c(100, 200); der <- .6; edi <- ev * der
  txt(593, 580, sprintf("%d lx × %s = %d lx", ev[1], format(der, decimal.mark = ","), edi[1]), 27, face = "bold", just = "centre")
  txt(593, 619, "Ev        DER        EDI", 17, muted, just = "centre")
  arr(771, 603, 813, 603, muted)
  txt(970, 580, sprintf("%d lx × %s = %d lx", ev[2], format(der, decimal.mark = ","), edi[2]), 27, face = "bold", just = "centre")
  txt(970, 619, "Ev        DER        EDI", 17, muted, just = "centre")
  chip(488, 642, 604, "DER bleibt 0,6 · dimensionslos", "#E6F3F0", teal)
  footer(c("Rechenbeispiel, keine Messwerte. Bei Ev = 0 ist DER nicht definiert. Keine Vorhersage einer individuellen Wirkung.",
    "EDI und DER gibt es für alle fünf α-opischen Bewertungen: melanopisch, rhodopisch sowie S-, M- und L-Zapfen."))
}

colour_graphic <- function() {
  header("Lichtfarbe und Farbwiedergabe.", "CCT ordnet die Lichtfarbe ein. Ra und Ri vergleichen die Wiedergabe von Testfarben mit einer Referenz.")
  box(48, 176, 530, 356)
  chip(76, 198, 68, "CCT")
  txt(161, 217, "Wie erscheint das Licht?", 24, face = "bold")
  lamp_cols <- c("#F5D197", "#F3EEDC", "#DFEAF4")
  xs <- c(156, 310, 464)
  for (i in 1:3) {
    circle(xs[i], 319, 45, lamp_cols[i], "#BDC5CB")
    box(xs[i] - 20, 368, 40, 9, "#8B979F", NA, 2)
    txt(xs[i], 408, c("warmweiß", "neutralweiß", "kaltweiß")[i], 19, just = "centre")
  }
  arr(116, 451, 508, 451, ochre, 2.2)
  txt(116, 481, "niedrigere CCT", 17, muted)
  txt(508, 481, "höhere CCT (K)", 17, muted, just = "right")
  box(602, 176, 550, 356)
  chip(630, 198, 106, "Ra / Ri", "#E6F3F0", teal)
  txt(754, 217, "Wie erscheinen Farben?", 24, face = "bold")
  txt(630, 269, "Dieselben Oberflächen, verschieden beleuchtet", 17, muted)
  cols <- c("#B97C77", "#BBA763", "#69A19A", "#8193B2")
  cols_test <- c("#AA8B78", "#AEA874", "#83A299", "#8794A3")
  txt(630, 322, "Referenz", 17, face = "bold")
  txt(630, 397, "Testlicht", 17, face = "bold")
  for (i in 1:4) {
    box(751 + 88 * (i - 1), 291, 62, 62, cols[i], NA, 7)
    box(751 + 88 * (i - 1), 366, 62, 62, cols_test[i], NA, 7)
  }
  txt(630, 477, "Ri: Vergleich für jede einzelne Testfarbe", 20, teal, "bold")
  box(48, 556, 1104, 132)
  txt(75, 585, "Ra fasst R1 bis R8 zusammen", 23, face = "bold")
  samples <- c("#B68D8B", "#B4A17A", "#A4AB73", "#80A08B", "#81A3A4", "#8194B0", "#A28AAA", "#B68D9D")
  for (i in 1:8) {
    box(75 + (i - 1) * 68, 608, 50, 32, samples[i], NA, 4)
    txt(100 + (i - 1) * 68, 660, paste0("R", i), 15.5, muted, just = "centre")
  }
  arr(640, 625, 678, 625, teal)
  txt(742, 615, "Mittelwert", 18, muted, just = "centre")
  txt(742, 650, "Ra", 29, teal, "bold", "centre")
  line(c(831, 831), c(581, 664), border, 1)
  box(861, 600, 52, 43, "#B74849", NA, 5)
  txt(935, 612, "R9: gesättigtes Rot", 18, face = "bold")
  txt(935, 644, "nicht in Ra enthalten", 17, muted)
  footer(c("Farben sind schematisch, keine farbmetrische Simulation. Gleiche CCT bedeutet weder gleiche Spektren noch gleiche DER.",
    "CCT ist nahe dem Planckschen Kurvenzug sinnvoll. Außerhalb der CIE-Anwendungsgrenzen können Werte fehlen."))
}

tau32 <- prerecep_filter(wl, 32)
tau65 <- prerecep_filter(wl, 65)
pupil_ratio <- sqrt(k_pup_fun(65))
stopifnot(all(tau32 > 0 & tau32 <= 1), all(tau65 > 0 & tau65 <= 1),
  isTRUE(all.equal(k_pup_fun(32), 1)), pupil_ratio > 0, pupil_ratio < 1,
  all(Tau_rel_fun(32) == 1))
age_graphic <- function() {
  header("Alter und Auge: zwei Einflüsse.", "Spectran verbindet die spektrale Transmission der Augenmedien mit einem Modell der Pupillengröße.")
  box(48, 176, 550, 352)
  chip(75, 198, 182, "AUGENMEDIEN", "#E6F3F0", teal)
  txt(75, 258, "Durchlässigkeit hängt von λ und Alter ab.", 21, face = "bold")
  txt(96, 296, "Transmission", 16, muted)
  axes(99, 318, 435, 143, ylabels = TRUE)
  plot_xy(99, 318, 435, 143, tau32, col = blue, dash = "longdash")
  plot_xy(99, 318, 435, 143, tau65, col = teal)
  line(c(357, 392), c(300, 300), blue, 2.5, "longdash")
  txt(402, 300, "32 Jahre", 15.5, blue)
  line(c(357, 392), c(325, 325), teal, 2.5)
  txt(402, 325, "65 Jahre", 15.5, teal)
  box(622, 176, 530, 352)
  chip(649, 198, 111, "PUPILLE", "#FFF5BA", ochre)
  txt(649, 258, "Die Öffnung beeinflusst die Lichtmenge.", 21, face = "bold")
  circle(760, 366, 58, "#E5E9EB", "#BDC5CB")
  circle(760, 366, 35, ink)
  circle(1006, 366, 58, "#E5E9EB", "#BDC5CB")
  circle(1006, 366, 35 * pupil_ratio, ink)
  arr(850, 366, 916, 366, ochre)
  txt(760, 451, "32 Jahre", 21, face = "bold", just = "centre")
  txt(1006, 451, "65 Jahre", 21, face = "bold", just = "centre")
  txt(887, 494, "Modellbeispiel mit kleinerer Pupillenfläche", 17, muted, just = "centre")
  box(48, 552, 1104, 136)
  txt(75, 580, "DIE BEIDEN FAKTOREN WERDEN VERKNÜPFT", 14.5, muted, "bold")
  txt(177, 624, "Referenzbewertung\nmit 32 Jahren", 21, face = "bold", just = "centre")
  txt(333, 624, "×", 31, muted, just = "centre")
  txt(483, 624, "Transmissions-\nfaktor", 21, teal, "bold", "centre")
  txt(628, 624, "×", 31, muted, just = "centre")
  txt(754, 624, "Pupillen-\nfaktor", 21, ochre, "bold", "centre")
  txt(884, 624, "=", 28, muted, just = "centre")
  txt(1023, 624, "Altersbezogene\nBewertung", 21, face = "bold", just = "centre")
  footer(c("Modellbeispiel für 65 Jahre, keine Messung eines individuellen Auges. Beide Korrekturfaktoren sind mit 32 Jahren gleich 1.",
    "Der melanopische Transmissionsfaktor hängt zusätzlich vom untersuchten Lichtspektrum ab. Modell: DIN/TS 5031-100."))
}

workflow_graphic <- function() {
  header("Vom Import zum vergleichbaren Ergebnis.", "Lichtquelle wählen, Bezugsgröße festlegen und die passende Auswertung als Datei behalten.")
  xs <- c(48, 432, 816)
  for (i in 1:3) {
    box(xs[i], 176, 336, 376)
    circle(xs[i] + 38, 214, 16, yellow)
    txt(xs[i] + 38, 214, i, 17, face = "bold", just = "centre")
    txt(xs[i] + 65, 214, c("Lichtquelle", "Lichtniveau", "Ausgabe")[i], 25, face = "bold")
  }
  arr(394, 365, 423, 365)
  arr(778, 365, 807, 365)
  txt(76, 271, "CSV  ·  Beispiel  ·  Konstruktion", 18, face = "bold")
  box(76, 306, 83, 103, bg, border, 6)
  txt(117, 330, "λ      Eλ", 14.5, muted, "bold", "centre")
  for (j in 0:3) line(c(90, 145), rep(353 + j * 12, 2), "#BAC4CA", 2)
  mini(188, 318, 164, 90, source_curve, spectral = TRUE)
  txt(76, 466, "Spalten und Einheiten prüfen.\nImporthinweise beachten.", 20)
  mini(464, 291, 271, 134, source_curve, ymax = 2, col = "#849199", dash = "dashed")
  plot_xy(464, 291, 271, 134, source_curve * 2, ymax = 2, col = teal)
  chip(645, 280, 76, "× 2", "#E6F3F0", teal)
  txt(461, 466, "Alle Wellenlängen erhalten\ndenselben Skalierungsfaktor.", 20)
  txt(461, 522, "Spektralform bleibt erhalten.", 17, teal, "bold")
  mini(851, 295, 114, 80, source_curve, spectral = TRUE)
  box(997, 295, 117, 80, "#F6F8F9", border, 5)
  for (j in 0:3) line(c(1010, 1101), rep(310 + 16 * j, 2), "#BAC4CA", 1.3)
  line(c(1063, 1063), c(307, 363), "#BAC4CA", 1.3)
  arr(983, 394, 983, 427, teal)
  chip(864, 442, 238, "Abbildungen + Tabellen", "#E6F3F0", teal, 17)
  txt(983, 518, "Export als Datei", 20, face = "bold", just = "centre")
  box(48, 576, 1104, 111, "#FFF9D9")
  txt(76, 608, "Vor einem Vergleich festlegen:", 22, face = "bold")
  txt(76, 653, "gleiches Ev", 22, ochre, "bold")
  txt(327, 653, "gleiche melanopische EDI", 22, teal, "bold")
  txt(777, 653, "Originalwerte", 22, ink, "bold")
  line(c(282, 282), c(637, 669), "#D8CD92", 1)
  line(c(731, 731), c(637, 669), "#D8CD92", 1)
  footer(c("Die drei Bezugsgrößen beantworten unterschiedliche Fragen. Die Skalierung oben ist ein künstliches Beispiel.",
    "Im Materialmodul kann das Zielniveau als Ev oder melanopische EDI vorgegeben und das Ergebnis für den Export gewählt werden."))
}

material1 <- .80 + .07 * (wl - 380) / 400
material2 <- .30 + .43 * (wl - 380) / 400
material3 <- .40 + .46 / (1 + exp(-(wl - 540) / 35))
path1 <- source_curve * material1
path2 <- path1 * material2
path3 <- path2 * material3
stopifnot(all(path3 >= 0), all(path3 <= path2), all(path2 <= path1), all(path1 <= source_curve),
  isTRUE(all.equal(path3, source_curve * material1 * material2 * material3)))
path_graphic <- function() {
  header("Einzelschritt und Gesamtwirkung unterscheiden.", "Ein Lichtpfad behält die Ursprungsquelle und die gespeicherten Materialschritte zusammen.", "MATERIAL UND LICHTPFAD")
  xs <- c(48, 332, 616, 900)
  cols <- c("#778792", blue, purple, ink)
  labs <- c("Ursprungsquelle", "Schritt 1", "Schritt 2", "Schritt 3")
  sublabs <- c("Start", "nach Material 1", "nach Material 2", "ausgewählt")
  for (i in 1:4) {
    box(xs[i], 176, 252, 84, if (i == 4) "#FFF5BA" else "white", if (i == 4) ochre else border)
    circle(xs[i] + 29, 204, 11, cols[i])
    txt(xs[i] + 50, 205, labs[i], 22, face = "bold")
    txt(xs[i] + 50, 237, sublabs[i], 16.5, muted)
    if (i < 4) arr(xs[i] + 257, 218, xs[i] + 276, 218)
  }
  line(c(126, 126, 1024, 1024), c(272, 292, 292, 272), teal, 2)
  box(357, 277, 436, 30, bg, NA, 0)
  txt(575, 291, "Gesamtwirkung: Start → ausgewählter Schritt", 18, teal, "bold", "centre")
  box(48, 329, 688, 358)
  txt(76, 358, "SPEKTREN ENTLANG DIESES PFADS", 14.5, muted, "bold")
  txt(95, 387, "Spektrale Bestrahlungsstärke (rel. Skala)", 16, muted)
  fill_spectrum(99, 408, 592, 185, source_curve, pale = TRUE)
  fill_spectrum(99, 408, 592, 185, path3)
  axes(99, 408, 592, 185, xlabel = FALSE)
  plot_xy(99, 408, 592, 185, source_curve, col = cols[1], dash = "longdash")
  plot_xy(99, 408, 592, 185, path1, col = cols[2], dash = "dashed")
  plot_xy(99, 408, 592, 185, path2, col = cols[3], dash = "dotdash")
  plot_xy(99, 408, 592, 185, path3, col = ink, width = 3)
  txt(99, 615, "380 nm", 14.5, muted)
  txt(691, 615, "780 nm", 14.5, muted, just = "right")
  txt(395, 615, "Wellenlänge λ", 16, muted, just = "centre")
  legend_x <- c(77, 226, 390, 554)
  for (i in 1:4) {
    line(c(legend_x[i], legend_x[i] + 28), c(657, 657), cols[i], 2.5,
      c("longdash", "dashed", "dotdash", "solid")[i])
    txt(legend_x[i] + 36, 657, c("Start", "Schritt 1", "Schritt 2", "Schritt 3")[i], 16)
  }
  box(760, 329, 392, 167)
  chip(786, 348, 171, "EINZELSCHRITT", "#F5EAF2", purple)
  txt(786, 414, "Eingang von Schritt 3 → Schritt 3", 20, face = "bold")
  txt(786, 463, "Hier: Schritt 2 mit Schritt 3 vergleichen.", 17, muted)
  box(760, 516, 392, 171)
  chip(786, 536, 190, "GESAMTWIRKUNG", "#E6F3F0", teal)
  txt(786, 601, "Ursprungsquelle → Schritt 3", 21, face = "bold")
  txt(786, 650, "Bei Verzweigungen nur diesen Pfad zeigen.", 17, muted)
  footer(c("Künstliches Beispiel: 3 passive Schritte, keine Neuskalierung, F = 1. Neuskalieren verändert den Vergleich vom Start aus.",
    "Berechnen → Ergebnis speichern / Material hinzufügen. Gespeichert wird in der Sitzung; für eine Datei den Export verwenden."))
}

figures <- list(spectrum_graphic, quantities_graphic, colour_graphic, age_graphic, workflow_graphic, path_graphic)
files <- c("05-spektrum-lesen", "06-beleuchtungsstaerke-edi-der", "07-lichtfarbe-farbwiedergabe", "08-alter-und-auge", "09-import-skalierung-export", "10-schritte-und-gesamtwirkung")
for (language in c("de", "en")) for (i in seq_along(figures)) {
  svglite::svglite(file.path(out, paste0(files[i], "-", language, ".svg")), width = 12, height = 8, bg = bg)
  figures[[i]]()
  dev.off()
}
write.csv(data.frame(wavelength_nm = wl, synthetic_source = source_curve, photopic_V = v_lambda, model_transmission_age32 = tau32, model_transmission_age65 = tau65, synthetic_material1 = material1, synthetic_material2 = material2, synthetic_material3 = material3, synthetic_step1 = path1, synthetic_step2 = path2, synthetic_step3 = path3), "data-raw/learning-illustrative-curves.csv", row.names = FALSE)
writeLines(c(capture.output(sessionInfo()), "Command: source('data-raw/learning-explanations.R')", "Inputs: data/Specs.rda, data/ColorP.rda, R/analysis_helpers_age.R.", capture.output(tools::md5sum(c("data/Specs.rda", "data/ColorP.rda", "R/analysis_helpers_age.R"))), "Synthetic spectrum/material examples; no measurement data. Age curves/pupil ratio use existing Spectran R functions. All calculations occur at development time only.", "German design approved by human after LEARN-G3-e60b4ff7951a, 2026-10-05.", "Checks: passive path product identity and nonnegative decreasing curves; bounded age transmission; reference factors equal 1 at 32 years.", "Scientific references: CIE S026:2018, CIE TN013:2022, CIE13.3-1995; DIN/TS5031-100 model in existing Spectran functions."), "data-raw/learning-illustrations-provenance.txt")
