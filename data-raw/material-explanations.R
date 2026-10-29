# Run from the package root with svglite available. These static, original
# illustrations are generated at development time, never during an app session.
library(grid)
out <- "inst/app/www/explanations"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
language <- "de"
english <- c(
  "SPECTRAN  /  MATERIALWIRKUNG" = "SPECTRAN  /  MATERIAL EFFECTS",
  "Licht durchlassen. Licht zurückwerfen." = "Light passing through. Light reflected back.",
  "Zwei Wechselwirkungen, mit klar erkennbarer Eingangs- und Ausgangsgröße." = "Two interactions, with distinct incident and outgoing quantities.",
  "Licht durch das Material" = "Light passing through the material",
  "Licht zurück in den einfallenden Halbraum" = "Light returned to the incident hemisphere",
  "Reflexion" = "Reflection", "REFLEXION" = "REFLECTION",
  "Einfallend" = "Incident", "Transmittiert" = "Transmitted", "Reflektiert" = "Reflected",
  "Eλ, E′λ: spektrale Bestrahlungsstärke · Mλ: reflektierte spektrale spezifische Ausstrahlung." = "Eλ, E′λ: spectral irradiance · Mλ: reflected spectral radiant exitance.",
  "Schematische Richtungen. Reflexion diffus dargestellt. Empfangsgrößen in Spectran: Szenario F = 1." = "Schematic directions; diffuse reflection shown. Receiver quantities in Spectran: F = 1 scenario.",
  "Das Material verändert das Spektrum." = "The material changes the spectrum.",
  "Der Koeffizient wirkt für jede Wellenlänge auf das einfallende Licht." = "The coefficient acts on the incident light at each wavelength.",
  "EINFALLENDES SPEKTRUM" = "INCIDENT SPECTRUM",
  "MATERIALKURVE" = "MATERIAL CURVE", "AUSGANGSSPEKTRUM" = "OUTGOING SPECTRUM",
  "Transmissionsgrad τλ" = "Transmittance τλ", "Reflexionsgrad ρλ" = "Reflectance ρλ",
  "Bestrahlungsstärke E′λ" = "Irradiance E′λ", "Ausstrahlung Mλ" = "Radiant exitance Mλ",
  "Kurven schematisch, keine Messdaten. Eingangs- und Ausgangsspektren sind gleich skaliert." = "Illustrative curves, not measurements. Incident and outgoing spectra share one scale.",
  "Mλ bezeichnet die reflektierte spezifische Ausstrahlung am Material. Empfangsgrößen: Szenario F = 1." = "Mλ is the reflected radiant exitance at the material. Receiver quantities: F = 1 scenario.",
  "Ein Material. Zwei mögliche Lichtwege." = "One material. Two possible light paths.",
  "Transmission und Reflexion in einer gemeinsamen räumlichen Skizze." = "Transmission and reflection in a shared spatial sketch.",
  "Zurückgeworfenes Licht" = "Reflected light", "Durchgelassenes Licht" = "Transmitted light",
  "Einfallendes Licht" = "Incident light",
  "Im Modul wird jeweils ein Lichtweg ausgewählt." = "Choose one of these paths in the module.",
  "Richtungen schematisch; Absorption nicht dargestellt. Die Pfeile zeigen keine berechnete Winkelverteilung." = "Schematic directions; absorption omitted. The arrows do not show a calculated angular distribution.",
  "Vom Material zum Empfänger." = "From material to receiver.",
  "Materialwirkung und geometrische Kopplung als getrennte Schritte im Rechenweg." = "Material effects and geometrical coupling are separate steps in the calculation.",
  "MATERIALWIRKUNG" = "MATERIAL EFFECT", "EMPFANGSSZENARIO" = "RECEIVER SCENARIO",
  "Einfallende\nBestrahlungsstärke" = "Incident\nirradiance", "Material-\nkoeffizient" = "Material\ncoefficient",
  "Transmittierte\nBestrahlungsstärke" = "Transmitted\nirradiance",
  "Reflektierte spektrale\nspezifische Ausstrahlung" = "Reflected spectral\nradiant exitance",
  "Bestrahlungsstärke\nam Empfänger" = "Irradiance\nat the receiver",
  "F = 1: idealisierte geometrische Kopplung. Spectran berechnet keine Raumgeometrie." = "F = 1: idealised geometrical coupling. Spectran does not calculate room geometry.",
  "Die Reflexionsbeziehung zum Empfänger setzt eine homogen beleuchtete, diffus reflektierende Fläche voraus." = "The reflection-to-receiver relationship assumes a uniformly illuminated, diffusely reflecting surface."
)
ink <- "#252C32"
muted <- "#59616A"
line_col <- "#DDE1E3"
yellow <- "#F8E350"
incident <- "#9E8300"
trans <- "#127C77"
refl <- "#9A4C83"
bg <- "#F4F6F7"
u <- function(x) unit(x, "native")
txt <- function(x, y, label, size = 20, col = ink, face = "plain", just = "left", lineheight = 1.25) {
  if (language == "en" && is.character(label) && label %in% names(english)) label <- unname(english[[label]])
  grid.text(label, x = u(x), y = u(y), just = just,
    gp = gpar(fontfamily = "Arial", fontsize = size * .72,
      col = col, fontface = face, lineheight = lineheight))
}
rect <- function(x, y, w, h, fill = "white", border = NA, radius = 12, lw = 1) {
  grid.roundrect(x = u(x), y = u(y), width = u(w), height = u(-h), just = c("left", "top"),
    r = unit(radius * .72, "pt"), gp = gpar(fill = fill, col = border, lwd = lw * .72))
}
seg <- function(x, y, col = ink, width = 2, dash = "solid", arrow = FALSE, head = 11) {
  grid.lines(x = u(x), y = u(y),
    arrow = if (arrow) grid::arrow(type = "closed", angle = 25, length = unit(head * .72, "pt")),
    gp = gpar(col = col, fill = col, lwd = width * .72, lty = dash, lineend = "round", linejoin = "round"))
}
arr <- function(x1, y1, x2, y2, col = ink, width = 4, dash = "solid", head = 12) {
  seg(c(x1, x2), c(y1, y2), col, width, dash, TRUE, head)
}
circle <- function(x, y, r, fill, border = NA) {
  grid.circle(x = u(x), y = u(y), r = u(r), gp = gpar(fill = fill, col = border))
}
chip <- function(x, y, w, label, fill = "#FFF5BA", col = ink) {
  rect(x, y, w, 32, fill, radius = 16)
  txt(x + w / 2, y + 16, label, 15, col, face = "bold", just = "centre")
}
header <- function(number, title, subtitle) {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, 1200), yscale = c(800, 0)))
  rect(0, 0, 1200, 800, bg, radius = 0)
  rect(48, 36, 28, 7, yellow, radius = 0)
  txt(88, 40, "SPECTRAN  /  MATERIALWIRKUNG", 14, muted, "bold")
  txt(48, 92, title, 36, face = "bold")
  txt(48, 132, subtitle, 19, muted)
}
footer <- function(lines) {
  seg(c(48, 1152), c(707, 707), line_col, 1)
  for (i in seq_along(lines)) txt(48, 733 + 23 * (i - 1), lines[[i]], 15.5, muted)
}
el <- expression(E[lambda])
et <- expression(E[lambda]^"\u2032")
ml <- expression(M[lambda])
tau <- expression(tau[lambda])
rho <- expression(rho[lambda])
eqt <- expression(E[lambda]^"\u2032" == tau[lambda] %.% E[lambda])
eqr <- expression(M[lambda] == rho[lambda] %.% E[lambda])
eqf <- expression(E[lambda*","*plain(Empf)] == F %.% M[lambda])
eqft <- expression(E[lambda*","*plain(Empf)] == F %.% E[lambda]^"\u2032")

v1 <- function() {
  header("01", "Licht durchlassen. Licht zurückwerfen.", "Zwei Wechselwirkungen, mit klar erkennbarer Eingangs- und Ausgangsgröße.")
  rect(48, 180, 540, 486, "white", line_col)
  rect(612, 180, 540, 486, "white", line_col)
  chip(76, 205, 38, "T", "#E6F3F0", trans)
  txt(130, 223, "Transmission", 28, face = "bold")
  txt(76, 261, "Licht durch das Material", 19, muted)
  chip(640, 205, 38, "R", "#F5EAF2", refl)
  txt(694, 223, "Reflexion", 28, face = "bold")
  txt(640, 261, "Licht zurück in den einfallenden Halbraum", 19, muted)
  txt(162, 322, "Einfallend", 20, just = "centre")
  txt(162, 358, el, 30, incident, just = "centre")
  txt(456, 322, "Transmittiert", 20, just = "centre")
  txt(456, 358, et, 30, trans, just = "centre")
  rect(280, 356, 68, 175, "#E6F3F0", "#8CB8B4", radius = 4, lw = 2)
  seg(c(292, 323), c(367, 367), "#B6D8D4", 2)
  seg(c(292, 323), c(520, 520), "#B6D8D4", 2)
  arr(90, 436, 274, 436, incident, 7, head = 15)
  seg(c(285, 343), c(436, 436), trans, 4, "dotted")
  arr(355, 436, 548, 436, trans, 7, head = 15)
  txt(314, 561, "Material", 20, just = "centre")
  txt(314, 604, eqt, 28, just = "centre")
  txt(715, 322, "Einfallend", 20, just = "centre")
  txt(715, 358, el, 30, incident, just = "centre")
  txt(1030, 322, "Reflektiert", 20, just = "centre")
  txt(1030, 358, ml, 30, refl, just = "centre")
  rect(763, 497, 338, 34, "#E5E7EA", radius = 3)
  seg(c(763, 1101), c(497, 497), "#7D858C", 3)
  for (x in seq(780, 1080, 22)) seg(c(x, x - 12), c(510, 523), "#BBC1C5", 1)
  arr(677, 388, 872, 490, incident, 7, head = 15)
  arr(880, 490, 956, 388, refl, 5)
  arr(883, 491, 1040, 405, refl, 5)
  arr(886, 492, 1098, 460, refl, 5)
  txt(934, 561, "Material", 20, just = "centre")
  txt(888, 604, eqr, 28, just = "centre")
  footer(c(
    "Eλ, E′λ: spektrale Bestrahlungsstärke · Mλ: reflektierte spektrale spezifische Ausstrahlung.",
    "Schematische Richtungen. Reflexion diffus dargestellt. Empfangsgrößen in Spectran: Szenario F = 1."
  ))
}

# Deliberately synthetic illustration, calculated only in R. The input and
# material-output miniatures share one relative vertical scale.
wavelength <- seq(380, 780, length.out = 101)
source_curve <- .26 + .46 * exp(-((wavelength - 480) / 65)^2) +
  .3 * exp(-((wavelength - 625) / 85)^2)
trans_curve <- .12 + .66 / (1 + exp(-(wavelength - 535) / 35))
refl_curve <- .12 + .32 * exp(-((wavelength - 500) / 95)^2)
trans_output <- source_curve * trans_curve
refl_output <- source_curve * refl_curve
stopifnot(all(trans_output >= 0 & trans_output <= source_curve),
  all(refl_output >= 0 & refl_output <= source_curve),
  all(trans_curve + refl_curve <= 1))
spectral_colours <- grDevices::colorRampPalette(
  c("#7645A6", "#2873B8", "#2AA6B0", "#8DC55D", "#F1D44A", "#ED8B39", "#B92E3E")
)(length(wavelength) - 1)
spectrum <- function(x, y, w, h, values, curve_col = ink, spectral = FALSE, fill = "#EBEFF2") {
  xs <- seq(x, x + w, length.out = length(values))
  ys <- y + h - values * h
  if (spectral) {
    for (i in seq_len(length(values) - 1L)) grid.polygon(
      x = u(c(xs[i] - .1, xs[i] - .1, xs[i + 1L] + .1, xs[i + 1L] + .1)),
      y = u(c(y + h, ys[i], ys[i + 1L], y + h)),
      gp = gpar(col = NA, fill = spectral_colours[i]))
  } else {
    grid.polygon(x = u(c(x, xs, x + w)), y = u(c(y + h, ys, y + h)),
      gp = gpar(col = NA, fill = fill))
  }
  seg(xs, ys, curve_col, 2.2)
  seg(c(x, x + w + 6), c(y + h, y + h), "#ABB2B8", 1)
  seg(c(x, x), c(y + h, y + 5), "#ABB2B8", 1)
  txt(x + w + 13, y + h + 8, "λ", 16, muted, just = "centre")
}
v2 <- function() {
  header("02", "Das Material verändert das Spektrum.", "Der Koeffizient wirkt für jede Wellenlänge auf das einfallende Licht.")
  txt(82, 184, "EINFALLENDES SPEKTRUM", 14, muted, "bold")
  txt(460, 184, "MATERIALKURVE", 14, muted, "bold")
  txt(833, 184, "AUSGANGSSPEKTRUM", 14, muted, "bold")
  for (row in 1:2) {
    y <- if (row == 1) 202 else 447
    col <- if (row == 1) trans else refl
    coeff <- if (row == 1) trans_curve else refl_curve
    values <- if (row == 1) trans_output else refl_output
    rect(48, y, 1104, 224, "white", line_col)
    txt(76, y + 31, if (row == 1) "Transmission" else "Reflexion", 24, col, "bold")
    txt(460, y + 31, if (row == 1) "Transmissionsgrad τλ" else "Reflexionsgrad ρλ", 20)
    txt(833, y + 31, if (row == 1) "Bestrahlungsstärke E′λ" else "Ausstrahlung Mλ", 20)
    txt(365, y + 74, el, 22, incident, just = "centre")
    spectrum(88, y + 64, 240, 102, source_curve, fill = "#FFF2A4", curve_col = incident)
    spectrum(467, y + 64, 240, 102, coeff, curve_col = col,
      fill = if (row == 1) "#E6F3F0" else "#F5EAF2")
    spectrum(844, y + 64, 240, 102, values, spectral = TRUE)
    txt(393, y + 112, "×", 40, muted, just = "centre")
    txt(775, y + 112, "=", 36, muted, just = "centre")
    txt(590, y + 199, if (row == 1) eqt else eqr, 23, just = "centre")
  }
  footer(c(
    "Kurven schematisch, keine Messdaten. Eingangs- und Ausgangsspektren sind gleich skaliert.",
    "Mλ bezeichnet die reflektierte spezifische Ausstrahlung am Material. Empfangsgrößen: Szenario F = 1."
  ))
}

v3 <- function() {
  header("03", "Ein Material. Zwei mögliche Lichtwege.", "Transmission und Reflexion in einer gemeinsamen räumlichen Skizze.")
  rect(48, 180, 1104, 494, "white", line_col)
  rect(103, 206, 419, 121, "#F8F0F6", radius = 10)
  txt(128, 237, "REFLEXION", 15, refl, "bold")
  txt(128, 273, "Zurückgeworfenes Licht", 24, face = "bold")
  txt(485, 307, eqr, 25, just = "right")
  rect(747, 244, 365, 122, "#ECF7F4", radius = 10)
  txt(771, 276, "TRANSMISSION", 15, trans, "bold")
  txt(771, 312, "Durchgelassenes Licht", 24, face = "bold")
  txt(1080, 347, eqt, 25, just = "right")
  rect(619, 341, 75, 219, "#E5EAEC", "#8D989F", radius = 4, lw = 2)
  rect(632, 354, 10, 193, "#F8E350", radius = 0)
  for (yy in seq(370, 530, 23)) seg(c(661, 681), c(yy, yy - 14), "#BEC8CD", 1.4)
  arr(606, 444, 383, 340, refl, 5)
  arr(606, 448, 329, 371, refl, 5)
  arr(606, 452, 361, 549, refl, 5)
  arr(136, 451, 604, 451, incident, 8, head = 16)
  txt(132, 408, "Einfallendes Licht", 22, face = "bold")
  txt(254, 497, el, 30, incident, just = "centre")
  seg(c(626, 687), c(451, 451), trans, 4, "dotted")
  arr(704, 451, 1076, 451, trans, 8, head = 16)
  txt(901, 500, et, 30, trans, just = "centre")
  txt(657, 592, "Material", 22, face = "bold", just = "centre")
  txt(657, 623, "τλ  ·  ρλ", 23, muted, just = "centre")
  txt(108, 618, "Im Modul wird jeweils ein Lichtweg ausgewählt.", 17, muted)
  footer(c(
    "Eλ, E′λ: spektrale Bestrahlungsstärke · Mλ: reflektierte spektrale spezifische Ausstrahlung.",
    "Richtungen schematisch; Absorption nicht dargestellt. Die Pfeile zeigen keine berechnete Winkelverteilung."
  ))
}

v4 <- function() {
  header("04", "Vom Material zum Empfänger.", "Materialwirkung und geometrische Kopplung als getrennte Schritte im Rechenweg.")
  rect(48, 182, 719, 482, "white", line_col)
  rect(842, 182, 310, 482, "#FFF9DA", "#E2D78B")
  txt(74, 214, "MATERIALWIRKUNG", 14, muted, "bold")
  txt(868, 214, "EMPFANGSSZENARIO", 14, muted, "bold")
  chip(890, 239, 213, "SPECTRAN: F = 1", yellow)
  for (row in 1:2) {
    y <- if (row == 1) 343 else 539
    col <- if (row == 1) trans else refl
    txt(76, y - 67, if (row == 1) "Transmission" else "Reflexion", 24, col, "bold")
    txt(126, y, el, 35, incident, just = "centre")
    arr(179, y, 260, y, "#9AA3AA", 2.5)
    rect(273, y - 36, 125, 73, if (row == 1) "#E6F3F0" else "#F5EAF2", radius = 9)
    txt(335, y, if (row == 1) tau else rho, 36, col, just = "centre")
    arr(412, y, 494, y, "#9AA3AA", 2.5)
    txt(583, y, if (row == 1) et else ml, 35, col, just = "centre")
    arr(654, y, 879, y, muted, 2.2, "dashed", 10)
    rect(771, y - 22, 59, 44, bg, radius = 22)
    txt(800, y, "× F", 21, just = "centre")
    txt(996, y - 2, if (row == 1) eqft else eqf, 24, just = "centre")
    txt(126, y + 60, "Einfallende\nBestrahlungsstärke", 16.5, muted, just = "centre")
    txt(335, y + 60, "Material-\nkoeffizient", 16.5, muted, just = "centre")
    txt(583, y + 60, if (row == 1) "Transmittierte\nBestrahlungsstärke" else "Reflektierte spektrale\nspezifische Ausstrahlung", 16.5, muted, just = "centre")
    txt(996, y + 60, "Bestrahlungsstärke\nam Empfänger", 17.5, just = "centre")
  }
  seg(c(73, 741), c(447, 447), line_col, 1)
  footer(c(
    "F = 1: idealisierte geometrische Kopplung. Spectran berechnet keine Raumgeometrie.",
    "Die Reflexionsbeziehung zum Empfänger setzt eine homogen beleuchtete, diffus reflektierende Fläche voraus."
  ))
}

plots <- list(v1, v2, v3, v4)
files <- c("01-strahlenwege", "02-spektren", "03-gemeinsamer-lichtweg", "04-material-und-empfaenger")
for (language in c("de", "en")) {
  if (language == "en") {
    eqf <- expression(E[lambda*","*plain(rec)] == F %.% M[lambda])
    eqft <- expression(E[lambda*","*plain(rec)] == F %.% E[lambda]^"\u2032")
  }
  for (i in seq_along(plots)) {
    svglite::svglite(file.path(out, paste0(files[i], "-", language, ".svg")), width = 12, height = 8, bg = bg)
    plots[[i]]()
    dev.off()
  }
}
write.csv(data.frame(wavelength_nm = wavelength, synthetic_incident = source_curve,
  illustrative_transmittance = trans_curve, illustrative_reflectance = refl_curve,
  synthetic_transmitted = trans_output, synthetic_reflected = refl_output),
  file.path("data-raw", "material-illustrative-curves.csv"), row.names = FALSE)
writeLines(c(capture.output(sessionInfo()), "",
  "Command (project R environment): source('data-raw/material-explanations.R')",
  "Inputs: synthetic curves defined in that R script; no measurement files.",
  "Checks: outgoing curves are nonnegative and do not exceed input; tau + rho <= 1.",
  "Synthetic illustrative curves, generated only in R. These are not measurements.",
  "App-model reference: R/material_core.R and R/material_language.R in the Spectran project.",
  "Radiometric background:",
  "https://doc.comsol.com/6.3/doc/com.comsol.help.heat/heat_ug_theory.07.051.html",
  "https://www.comsol.com/blogs/computing-view-factors-with-the-heat-transfer-module/"),
  file.path("data-raw", "material-illustrations-provenance.txt"))
cat("Wrote four bilingual SVG illustrations to", out, "\n")
