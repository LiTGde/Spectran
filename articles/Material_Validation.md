# Material validation

## Preface

This article extends the [spectral
validation](https://litgde.github.io/Spectran/articles/Validation.md) to
Spectran’s **Transmission / Reflection** module. It compares photopic
and melanopic material coefficients with **DIN/TS 67600:2022-08, Tables
6 to 10**. The article shows the calculations, reference values and
signed differences.

The scope follows the DIN material tables: transmission under **D65**,
and reflection under **EE, A, D65, FL11, LED-B1, LED-B2, LED-B3 and
LED-B5**. Each material is compared only under the illuminants for which
its DIN table supplies coefficients. Comparisons use documented TUB
sample assignments. Materials without an assigned spectrum remain in the
reference inventory.

The [TUB dataset, version
2](https://doi.org/10.14279/depositonce-11893.2), is explicitly
referenced in DIN/TS 67600, Section 6.2.4.1 and reference \[43\]. Its
[documentation](https://depositonce.tu-berlin.de/bitstreams/fb974995-09e8-4448-a0ee-95ddc66230f6/download),
pp. 2, 6 and 9, describes the measurement programme, specimens and
transmission data. This source relationship does not provide a complete
specimen mapping for DIN Tables 7 and 8.

## Table preparation

### Setup and reference input

All scientific calculations use R and the current Spectran source.
Loading the project with
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
ensures that the article tests the module in the working copy. Internal
module functions are called deliberately here because this article
checks their implementation.

Setup

``` r

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(gt)
})

root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
pkgload::load_all(root, quiet = TRUE)
data_dir <- file.path(root, "vignettes/articles/material-validation-data")
response_labels <- c(vis = "Photopic", mel = "Melanopic")
theme_set(cowplot::theme_cowplot(font_size = 11, font_family = "sans"))

fmt <- function(x, digits = 3) formatC(x, digits = digits, format = "f")
sci_prose <- function(x, digits = 2) {
  if (x == 0) return("0")
  parts <- strsplit(formatC(x, format = "e", digits = digits), "e", fixed = TRUE)[[1]]
  paste0("$", parts[1], "\\times 10^{", as.integer(parts[2]), "}$")
}
```

The [prepared DIN coefficient
CSV](https://litgde.github.io/Spectran/articles/material-validation-data/din-67600-material-coefficients.csv)
contains one row per material and evaluating illuminant. It records the
original German designation, an English translation, transmission or
reflection, the photopic coefficient `din_vis`, the melanopic
coefficient `din_mel`, and the DIN table and printed page. Both
coefficients are dimensionless fractions. The nonvisual quantity
considered here is the melanopic coefficient.

The 260 rows cover all 71 materials in Tables 6 to 10. The unglazed
reference in Table 6 is not an additional material. CSV preparation is a
separate, documented step; rendering this article requires no PDF
extraction or table parsing. The [data
notes](https://litgde.github.io/Spectran/articles/material-validation-data/data-notes.txt)
describe the fields and preparation script.

Calculation

``` r

din_file <- file.path(data_dir, "din-67600-material-coefficients.csv")
assignment_file <- file.path(data_dir, "tub-assignments.csv")

din_coefficients <- read.csv(din_file, fileEncoding = "UTF-8") |> as_tibble()
assignments <- read.csv(assignment_file, fileEncoding = "UTF-8") |> as_tibble()

stopifnot(
  nrow(din_coefficients) == 260L,
  n_distinct(din_coefficients$din_id) == 71L,
  !anyDuplicated(din_coefficients[c("din_id", "illuminant")]),
  !anyNA(din_coefficients[c("din_vis", "din_mel")])
)
```

Table and thumbnail styling

``` r

style_table <- function(x) {
  x |>
    opt_align_table_header(align = "left") |>
    tab_options(
      table.font.names = c("Arial", "Helvetica", "sans-serif"),
      table.font.size = px(13), table.width = pct(100),
      data_row.padding = px(6), column_labels.font.weight = "bold",
      table.border.top.width = px(2), table.border.bottom.width = px(2),
      container.overflow.x = "auto"
    )
}

thumbnail <- function(data, alt) {
  plot <- ggplot(data, aes(wavelength, value)) +
    geom_rect(aes(xmin = wavelength - 0.5, xmax = wavelength + 0.5,
      ymin = 0, ymax = value, fill = colour), colour = NA) +
    geom_line(colour = "#222222", linewidth = 0.3) +
    scale_fill_identity() +
    scale_x_continuous(breaks = c(380, 780), expand = expansion(mult = 0)) +
    scale_y_continuous(breaks = c(0, 1), expand = expansion(mult = 0)) +
    coord_cartesian(xlim = c(380, 780), ylim = c(0, 1.03)) +
    labs(x = NULL, y = NULL) +
    theme_classic(base_size = 6) +
    theme(
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      axis.ticks.length = grid::unit(1, "pt"),
      plot.margin = margin(2, 2, 2, 2)
    )

  image_file <- tempfile(fileext = ".png")
  on.exit(unlink(image_file))
  ggsave(image_file, plot = plot, width = 93 / 96, height = 60 / 96,
    units = "in", dpi = 192, bg = "white", limitsize = FALSE)
  tag <- as.character(gt::local_image(image_file, height = 60))
  sub("<img ", paste0('<img alt="', htmltools::htmlEscape(alt, attribute = TRUE),
    '" '), tag, fixed = TRUE)
}
```

### DIN materials and TUB assignments

The DIN tables contain 12 glazing samples, 11 multi-skin sheets, 21
plastics or filters, and 27 reflecting surfaces. Tables 9 and 10 reuse
the same surface samples.

The [assignment
CSV](https://litgde.github.io/Spectran/articles/material-validation-data/tub-assignments.csv)
links 39 DIN materials to TUB catalogue records. Wood identifiers follow
the specimen order in the TUB documentation because the original CSV
repeats a wood heading. For S1, DIN describes yellow brick whereas TUB
describes beige brick. The comparison tables retain the TUB glazing
constructions and this difference in the brick description. Numeric
agreement is not used to select specimens.

Calculation

``` r

records <- Spectran:::tub_material_records |>
  select(catalogue_id, display_name_en, source_file, source_column, source_column_header)

materials <- din_coefficients |>
  distinct(din_id, din_name_en, mode, group) |>
  left_join(assignments, by = "din_id", relationship = "one-to-one") |>
  left_join(records, by = "catalogue_id", relationship = "many-to-one") |>
  mutate(
    material = coalesce(sub("^[^·]+ · ", "", display_name_en), din_name_en),
    material = if_else(din_id == 416L, "Yellow brick (TUB: beige)", material),
    curve_available = !is.na(catalogue_id)
  )

stopifnot(nrow(materials) == 71L, !anyNA(materials$material),
  sum(materials$curve_available) == 39L)

coverage <- materials |>
  summarise(Materials = n(), Assigned = sum(curve_available),
    Missing = sum(!curve_available), .by = group)
```

Format the coverage table

``` r

coverage |>
  gt(rowname_col = "group") |>
  cols_label(Assigned = "Assigned spectra", Missing = "No confirmed spectrum") |>
  fmt_integer(columns = where(is.numeric)) |>
  style_table()
```

|                      | Materials | Assigned spectra | No confirmed spectrum |
|----------------------|-----------|------------------|-----------------------|
| Glazing              | 12        | 12               | 0                     |
| Multi-skin sheets    | 11        | 0                | 11                    |
| Plastics and filters | 21        | 0                | 21                    |
| Surfaces             | 27        | 27               | 0                     |

Table 1: Coverage of the DIN material examples by documented TUB
assignments.

### Preparing the DIN illuminants

The source key `CIE:E` represents the equal-energy illuminant labelled
**EE** in DIN. Each source is linearly interpolated onto 380–780 nm at 1
nm intervals and scaled to 100 lx using the material module. A scalar
change in source intensity cancels in a material coefficient. The
thumbnail copies are separately divided by their peak solely to display
spectral shape on a common 0–1 scale.

Calculation

``` r

sources <- din_coefficients |>
  distinct(illuminant) |>
  mutate(
    source_name = if_else(illuminant == "EE", "E", gsub("-", "_", illuminant)),
    source_id = paste0("CIE:", source_name)
  ) |>
  arrange(match(illuminant, c("EE", "A", "D65", "FL11", "LED-B1", "LED-B2", "LED-B3", "LED-B5")))

din_reference <- din_coefficients |>
  select(din_id, illuminant, din_table_ref, page, din_vis, din_mel) |>
  left_join(select(sources, illuminant, source_id),
    by = "illuminant", relationship = "many-to-one") |>
  pivot_longer(c(din_vis, din_mel), names_to = "response",
    names_prefix = "din_", values_to = "din") |>
  select(-illuminant)

source_spectra <- map(sources$source_name, function(name) {
  raw <- Spectran::examplespectra$CIE |>
    filter(between(Wellenlaenge, 380, 780)) |>
    select(Wellenlaenge, all_of(name))
  stopifnot(all(is.finite(as.matrix(raw))), !anyDuplicated(raw[[1]]),
    min(raw[[1]]) == 380, max(raw[[1]]) == 780)

  spectrum <- tibble(
    Wellenlaenge = 380:780,
    Bestrahlungsstaerke = approx(raw[[1]], raw[[2]], xout = 380:780)$y
  )
  Spectran:::material_scale_light(spectrum, 100)
}) |>
  setNames(sources$source_id)

stopifnot(all(map_lgl(source_spectra, function(s) {
  all(is.finite(s$Bestrahlungsstaerke)) && all(s$Bestrahlungsstaerke >= 0)
})))
```

## Calculation

For incident spectrum $`S(\lambda)`$, material spectrum $`c(\lambda)`$
and weighting function $`w(\lambda)`$, the coefficient is

``` math
c_w(S)=\frac{\int_{380}^{780} S(\lambda)c(\lambda)w(\lambda)\,\mathrm d\lambda}
{\int_{380}^{780} S(\lambda)w(\lambda)\,\mathrm d\lambda}.
 \qquad(1)
```

Here $`c=\tau`$ for transmission and $`c=\rho`$ for reflection. The
photopic weighting is $`V(\lambda)`$; the melanopic weighting is
$`s_{\mathrm{mel}}(\lambda)`$. Both are taken from `Specs$AS_wide`. For
illuminants other than D65, DIN also uses the designation
$`k_{\mathrm{mel}}`$ for the melanopic material factor. We compare the
weighted material coefficients, not MDER or effective MDER.

Spectran linearly interpolates the TUB material samples from 5 nm to 1
nm and sums over 380–780 nm, including both endpoints. The common
wavelength increment cancels in the quotient. The outgoing spectrum is
not renormalised when calculating a material coefficient.

Calculation

``` r

mapped <- filter(materials, curve_available)
material_preparation <- map(mapped$catalogue_id, function(id) {
  curve <- Spectran:::transmission_catalogue_record(id)$curve
  stopifnot(identical(as.numeric(curve$wavelength_nm), seq(380, 780, 5)))

  raw <- data.frame(wavelength_nm = curve$wavelength_nm, value = curve$transmittance)
  prepared <- Spectran:::prepare_transmission_curve(raw, scale = "fraction")$completed
  stopifnot(all(is.finite(prepared$transmittance)),
    all(prepared$status %in% c("supplied", "interpolated")))
  prepared
}) |>
  setNames(mapped$catalogue_id)

weights <- list(
  vis = Spectran::Specs$AS_wide[["V(lambda)"]],
  mel = Spectran::Specs$AS_wide$Melanopsin
)
stopifnot(identical(as.numeric(Spectran::Specs$AS_wide$Wellenlaenge), as.numeric(380:780)))
```

### Worked example: DIN 106 / TUB G6 under D65

The numerator describes the transmitted, weighted spectrum. The
denominator describes the incident, weighted spectrum. Their ratio is
dimensionless.

Calculation

``` r

s <- source_spectra[["CIE:D65"]]$Bestrahlungsstaerke
c_material <- material_preparation[["tub:transmission:G6"]]$transmittance

example_terms <- imap(weights, function(w, response) {
  numerator <- sum(s * c_material * w)
  denominator <- sum(s * w)
  tibble(
    Response = unname(response_labels[response]),
    Numerator = numerator, Denominator = denominator,
    Coefficient = numerator / denominator
  )
}) |>
  list_rbind()
```

Format the worked example

``` r

example_terms |>
  gt(rowname_col = "Response") |>
  fmt_number(columns = where(is.numeric), decimals = 6) |>
  style_table()
```

|           | Numerator | Denominator | Coefficient |
|-----------|-----------|-------------|-------------|
| Photopic  | 0.105421  | 0.146413    | 0.720027    |
| Melanopic | 0.093267  | 0.132621    | 0.703261    |

Table 2: Explicit weighted sums for DIN 106 / TUB G6 under D65.

The resulting photopic coefficient is 0.720027 and the melanopic
coefficient is 0.703261.

### Checking the material module

For every confirmed material–illuminant pair in the DIN tables, we
execute both the module and the explicit weighted sums. The module field
`active_metrics$comparison_value` supplies the outgoing-to-incident
ratio. This is an implementation check, not an independent test of the
input spectra.

Calculation

``` r

coefficient_values <- function(catalogue_id, source_id, mode) {
  spectrum <- source_spectra[[source_id]]
  curve <- material_preparation[[catalogue_id]]
  result <- Spectran:::calculate_material_result(spectrum, curve, tolower(mode))
  metric_ids <- c(vis = "photopic_illuminance", mel = "melanopic_irradiance")

  imap(weights, function(w, response) {
    numerator <- sum(spectrum$Bestrahlungsstaerke * curve$transmittance * w)
    denominator <- sum(spectrum$Bestrahlungsstaerke * w)
    value <- result$active_metrics$comparison_value[
      result$active_metrics$metric_id == metric_ids[[response]]
    ]
    stopifnot(length(value) == 1L, is.finite(value), denominator > 0)

    tibble(
      catalogue_id, source_id, response,
      spectran = value, numerator, denominator,
      explicit = numerator / denominator
    )
  }) |>
    list_rbind()
}

pairs <- din_reference |>
  distinct(din_id, source_id) |>
  inner_join(select(mapped, din_id, catalogue_id, mode),
    by = "din_id", relationship = "many-to-one")

calculated <- pmap(pairs, function(din_id, source_id, catalogue_id, mode) {
  coefficient_values(catalogue_id, source_id, mode) |>
    mutate(din_id = din_id)
}) |>
  list_rbind()

internal_max <- max(abs(calculated$spectran - calculated$explicit))
stopifnot(internal_max <= 1e-12)
```

The module and the explicit sums agree within $`10^{-12}`$ for all 456
coefficients (228 material–illuminant pairs). The maximum absolute
difference is 0.

### Differences and rounding

We report $`\Delta=c_{\mathrm{Spectran}}-c_{\mathrm{DIN}}`$, its
percentage-point form $`100\Delta`$, and the relative difference
$`100\Delta/c_{\mathrm{DIN}}`$. Positive values mean that Spectran is
higher. Relative differences are undefined for a zero DIN value.

The strict rounding interval is **±0.0005** for a DIN coefficient
printed to three decimals. A second, project-defined **reproduction
limit of ±0.001** (±0.10 percentage points) also allows for TUB spectral
coefficients rounded to three decimals. With nonnegative weights,
rounding each input coefficient to the nearest 0.001 can change the
weighted mean by at most 0.0005. Adding the two rounding bounds gives
0.001. This is neither a DIN conformity tolerance nor measurement
uncertainty, and it does not explain the cause of any individual
difference.

Calculation

``` r

material_details <- materials |>
  select(din_id, material, mode, group, catalogue_id, curve_available)
calculation_details <- calculated |>
  select(din_id, source_id, response, spectran, numerator, denominator, explicit)

comparison <- din_reference |>
  left_join(material_details, by = "din_id", relationship = "many-to-one") |>
  left_join(calculation_details,
    by = c("din_id", "source_id", "response"), relationship = "one-to-one") |>
  left_join(select(sources, source_id, illuminant),
    by = "source_id", relationship = "many-to-one") |>
  mutate(
    difference = spectran - din,
    difference_pp = 100 * difference,
    relative_difference_percent = if_else(din != 0, 100 * difference / din, NA_real_),
    within_rounding = abs(difference) <= 0.0005,
    within_budget = abs(difference) <= 0.001,
    status = case_when(
      !curve_available ~ "No confirmed spectrum",
      within_rounding ~ "Within rounding",
      within_budget ~ "Within reproduction limit",
      TRUE ~ "Outside reproduction limit"
    )
  )

stopifnot(nrow(comparison) == 520L)
paired <- filter(comparison, curve_available)
wide <- comparison |>
  select(din_id, material, mode, catalogue_id, curve_available, source_id,
    illuminant, response, din, spectran, difference_pp, relative_difference_percent) |>
  pivot_wider(
    names_from = response,
    values_from = c(din, spectran, difference_pp, relative_difference_percent)
  )
```

## Results

The overview includes only confirmed assignments with both DIN and
Spectran values. The result tables below pair each material curve with
the applicable illuminant curve; the material plots retain absolute
coefficients on a 0–1 scale, whereas illuminant plots show
peak-normalised spectral power. All thumbnails span 380–780 nm. Their
filled areas use Spectran’s `Lang` wavelength palette. These colours
indicate wavelength and do not predict the perceived colour of a
material or reproduce monochromatic light faithfully on a screen.

Calculation

``` r

summary_by_response <- paired |>
  summarise(
    Coefficients = n(), Rounding = sum(within_rounding),
    Reproduced = sum(within_budget), Outside = sum(!within_budget),
    median_abs_pp = median(abs(difference_pp)),
    max_abs_pp = max(abs(difference_pp)),
    .by = c(mode, response)
  )

summary_by_source <- paired |>
  summarise(
    Coefficients = n(), Outside = sum(!within_budget),
    max_abs_pp = max(abs(difference_pp)),
    .by = c(mode, illuminant, response)
  )
```

Format the overview

``` r

summary_by_response |>
  mutate(response = unname(response_labels[response])) |>
  gt(groupname_col = "mode", rowname_col = "response") |>
  cols_label(Coefficients = "n", Rounding = html("Within<br>±0.0005"),
    Reproduced = html("Within<br>±0.001"), Outside = "Outside",
    median_abs_pp = html("Median |Δ|<br>(pp)"), max_abs_pp = html("Max. |Δ|<br>(pp)")) |>
  fmt_integer(columns = c(Coefficients, Rounding, Reproduced, Outside)) |>
  fmt_number(columns = c(median_abs_pp, max_abs_pp), decimals = 4) |>
  style_table() |>
  tab_source_note("pp = percentage points.")
```

[TABLE]

Table 3: Agreement for confirmed assignments. The reproduction count
includes the stricter rounding count.

Calculation

``` r

figure_overview_data <- paired |>
  mutate(
    illuminant = factor(illuminant, levels = sources$illuminant),
    response = factor(response, levels = names(response_labels), labels = response_labels)
  )

ggplot(figure_overview_data, aes(illuminant, difference_pp, colour = mode, shape = mode)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed",
    colour = "grey45", linewidth = 0.35) +
  geom_point(size = 1.7, alpha = 0.65, position = position_dodge(width = 0.3)) +
  facet_wrap(~ response, nrow = 1) +
  scale_colour_manual(values = c(Reflection = "#264E86", Transmission = "#B04A32")) +
  scale_shape_manual(values = c(Reflection = 17, Transmission = 16)) +
  labs(x = "Illuminant", y = "Spectran − DIN (percentage points)", colour = NULL, shape = NULL) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
```

![Two panels show photopic and melanopic Spectran-minus-DIN differences
by illuminant. Circles identify transmission and triangles reflection.
Horizontal dashed lines mark plus and minus 0.10 percentage
points.](Material_Validation_files/figure-html/fig-overview-1.png)

Figure 1: Signed differences for confirmed assignments. Dashed lines
mark the project reproduction limit of ±0.10 percentage points; the two
panels use the same vertical scale.

Prepare the two spectral thumbnails and format the result tables

``` r

wavelength_colours <- tibble(
  wavelength = 380:780,
  colour = unname(Spectran::ColorP$Lang[as.character(380:780)])
)
stopifnot(!anyNA(wavelength_colours))

source_thumbnails <- map(source_spectra, function(s) {
  tibble(wavelength = s$Wellenlaenge,
    value = s$Bestrahlungsstaerke / max(s$Bestrahlungsstaerke)) |>
    left_join(wavelength_colours, by = "wavelength", relationship = "one-to-one")
})
material_thumbnails <- map(material_preparation, function(curve) {
  tibble(wavelength = curve$wavelength_nm, value = curve$transmittance) |>
    left_join(wavelength_colours, by = "wavelength", relationship = "one-to-one")
})

source_images <- imap_chr(source_thumbnails, function(data, id) {
  label <- sources$illuminant[match(id, sources$source_id)]
  image <- thumbnail(data, paste(label, "relative spectral power, 380–780 nm"))
  paste0(image, '<br><span class="spectrum-label">', label, '</span>')
})
material_images <- imap_chr(material_thumbnails, function(data, id) {
  label <- materials$material[match(id, materials$catalogue_id)]
  thumbnail(data, paste(label, "material coefficient, 380–780 nm"))
})

signed_cell <- function(x, digits = 3, mark_limit = FALSE) {
  map_chr(x, function(value) {
    if (is.na(value)) return("NA")
    # Suppress a negative sign only when the displayed value rounds to zero.
    displayed <- if (round(value, digits) == 0) 0 else value
    label <- formatC(displayed, format = "f", digits = digits, flag = "+")
    if (mark_limit && abs(value) > 0.1) {
      paste0('<span class="outside-limit"><strong>', label, '</strong></span>')
    } else {
      label
    }
  })
}

result_table <- function(data) {
  table_data <- data |>
    mutate(
      identity = paste0(din_id, "<br><span class='sample-id'>",
        sub("tub:(transmission|reflection):", "", catalogue_id), "</span>"),
      material_plot = unname(material_images[catalogue_id]),
      source_plot = unname(source_images[source_id]),
      response = "vis<br>mel"
    ) |>
    select(identity, material, material_plot, source_plot, response,
      din_vis, din_mel, spectran_vis, spectran_mel, difference_pp_vis,
      difference_pp_mel, relative_difference_percent_vis, relative_difference_percent_mel)

  table_data |>
    gt() |>
    fmt_number(columns = c(din_vis, din_mel), decimals = 3) |>
    fmt_number(columns = c(spectran_vis, spectran_mel), decimals = 5) |>
    gt::fmt(columns = c(difference_pp_vis, difference_pp_mel),
      fns = function(x) signed_cell(x, mark_limit = TRUE)) |>
    gt::fmt(columns = c(relative_difference_percent_vis, relative_difference_percent_mel),
      fns = signed_cell) |>
    sub_missing(missing_text = "NA") |>
    cols_merge(columns = c(din_vis, din_mel), pattern = "{1}<br>{2}") |>
    cols_merge(columns = c(spectran_vis, spectran_mel), pattern = "{1}<br>{2}") |>
    cols_merge(columns = c(difference_pp_vis, difference_pp_mel), pattern = "{1}<br>{2}") |>
    cols_merge(columns = c(relative_difference_percent_vis, relative_difference_percent_mel),
      pattern = "{1}<br>{2}") |>
    fmt_markdown(columns = c(identity, material_plot, source_plot, response)) |>
    cols_label(
      identity = html("DIN<br>TUB"), material = "Material description",
      material_plot = "Material", source_plot = "Illuminant", response = "Weight",
      din_vis = "DIN", spectran_vis = "Spectran",
      difference_pp_vis = html("Δ<br>(pp)"),
      relative_difference_percent_vis = html("Δ<br>(%)")
    ) |>
    cols_align(align = "left", columns = material) |>
    cols_align(align = "center", columns = c(identity, material_plot, source_plot, response)) |>
    cols_align(align = "right",
      columns = c(din_vis, spectran_vis, difference_pp_vis, relative_difference_percent_vis)) |>
    cols_width(
      identity ~ px(42), material ~ px(154), material_plot ~ px(80), source_plot ~ px(80),
      response ~ px(45), din_vis ~ px(62), spectran_vis ~ px(69),
      difference_pp_vis ~ px(62), relative_difference_percent_vis ~ px(63)
    ) |>
    style_table() |>
    # Preserve paired cell formatting and column widths through Quarto/pkgdown.
    tab_options(table.font.size = px(12), data_row.padding = px(4),
      quarto.disable_processing = TRUE) |>
    tab_source_note(html(paste0(
      "Within each cell: <b>vis</b> (photopic), then <b>mel</b> (melanopic). ",
      "Δ = Spectran − DIN. pp = percentage points; % = relative difference. ",
      "Bold red Δ values exceed ±0.10 pp. ",
      "Thumbnails: 380–780 nm; material 0–1; illuminant peak = 1."
    )))
}
```

### Transmission

#### Confirmed glazing assignments

Table 6 supplies D65 coefficients for the 12 glazing samples. These are
the confirmed transmission comparisons. Five decimal places are retained
for Spectran to make small differences visible; they do not imply
equivalent measurement accuracy.

Each numeric cell lists **photopic (vis)** above **melanopic (mel)**
values. Δ = Spectran − DIN; **pp** = percentage points; **%** = relative
difference. Bold red Δ values exceed ±0.10 pp.

Scroll the table sideways to compare all columns. The compact DIN/TUB
identifiers stay visible.

R code: transmission table

``` r

wide |>
  filter(mode == "Transmission", curve_available) |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 4: Transmission under D65: DIN Table 6 and the assigned TUB
glazing spectra.

#### Materials without an assigned spectrum

No confirmed TUB spectrum is assigned to the following 32 DIN materials.
A missing assignment can mean that the named material is absent from the
source or that the available descriptions do not establish specimen
identity. All entries below are evaluated under D65 in DIN; no
substitute spectra are used.

Calculation

``` r

missing_inventory <- wide |>
  filter(!curve_available) |>
  left_join(select(materials, din_id, group), by = "din_id", relationship = "one-to-one") |>
  arrange(din_id) |>
  select(group, din_id, material, din_vis, din_mel)

stopifnot(nrow(missing_inventory) == 32L)
```

Scroll the table sideways to compare both coefficients. The DIN IDs stay
visible.

Format the missing-material inventory

``` r

missing_inventory |>
  gt(groupname_col = "group") |>
  cols_label(din_id = "DIN ID", material = "Material",
    din_vis = "DIN photopic", din_mel = "DIN melanopic") |>
  fmt_number(columns = c(din_vis, din_mel), decimals = 3) |>
  style_table()
```

| DIN ID | Material | DIN photopic | DIN melanopic |
|----|----|----|----|
| Multi-skin sheets |  |  |  |
| 201 | Type 1, 16 mm, clear | 0.735 | 0.733 |
| 202 | Type 1, 16 mm, diffusing | 0.618 | 0.623 |
| 203 | Type 1, 16 mm, clear, tinted | 0.297 | 0.264 |
| 204 | Type 2, 16 mm, clear | 0.635 | 0.630 |
| 205 | Type 2, 16 mm, diffusing | 0.547 | 0.555 |
| 206 | Type 2, 16 mm, clear, tinted | 0.268 | 0.232 |
| 207 | Type 3, 25 mm, clear | 0.485 | 0.482 |
| 208 | Type 3, 25 mm, diffusing | 0.404 | 0.409 |
| 209 | Type 3, 25 mm, clear, tinted | 0.133 | 0.114 |
| 210 | Type 4, 20 mm, clear | 0.589 | 0.580 |
| 211 | Type 4, 20 mm, diffusing | 0.467 | 0.474 |
| Plastics and filters |  |  |  |
| 301 | Plexiglas GS233 | 0.924 | 0.923 |
| 302 | Plexiglas XT | 0.924 | 0.923 |
| 303 | Plexiglas 7H | 0.921 | 0.920 |
| 304 | Plexiglas 8N | 0.923 | 0.921 |
| 305 | RT95 sun-protection filter | 0.106 | 0.001 |
| 306 | Plexiglas GS blue 602, 3 mm | 0.158 | 0.437 |
| 307 | Plexiglas GS blue 625, 3 mm | 0.617 | 0.750 |
| 308 | Plexiglas GS blue 627, 3 mm | 0.041 | 0.232 |
| 309 | Plexiglas GS brown 400, 3 mm | 0.190 | 0.105 |
| 310 | Plexiglas GS yellow 303, 3 mm | 0.739 | 0.194 |
| 311 | Plexiglas GS grey 837, 3 mm | 0.092 | 0.141 |
| 312 | Plexiglas GS grey 838, 3 mm | 0.210 | 0.232 |
| 313 | Plexiglas GS grey 882, 3 mm | 0.452 | 0.522 |
| 314 | Plexiglas GS green 700, 3 mm | 0.096 | 0.131 |
| 315 | Plexiglas GS green 701, 3 mm | 0.222 | 0.463 |
| 316 | Plexiglas GS green 777, 3 mm | 0.701 | 0.737 |
| 317 | Plexiglas GS orange 478, 3 mm | 0.474 | 0.046 |
| 318 | Plexiglas GS red 501, 3 mm | 0.134 | 0.013 |
| 319 | Plexiglas GS red 502, 3 mm | 0.056 | 0.005 |
| 320 | Plexiglas GS umbra 802, 3 mm | 0.551 | 0.497 |
| 321 | Plexiglas GS umbra 807, 3 mm | 0.471 | 0.526 |

Table 5: DIN materials without an assigned TUB spectrum, with their D65
reference coefficients. Multi-skin sheets are from Table 7; plastics and
filters are from Table 8.

### Reflection

Tables 9 and 10 provide eight illuminants for the same 27 reflecting
surfaces. The tabs follow the DIN order. Each table reports both
weighting functions and retains all samples, including the FL11
differences.

Each numeric cell lists **photopic (vis)** above **melanopic (mel)**
values. Δ = Spectran − DIN; **pp** = percentage points; **%** = relative
difference. Bold red Δ values exceed ±0.10 pp.

Scroll the table sideways to compare all columns. The compact DIN/TUB
identifiers stay visible.

- EE
- A
- D65
- FL11
- LED-B1
- LED-B2
- LED-B3
- LED-B5

R code: EE reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:E") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 6: Reflection under EE, DIN Table 9.

R code: A reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:A") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 7: Reflection under A, DIN Table 9.

R code: D65 reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:D65") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 8: Reflection under D65, DIN Table 9.

R code: FL11 reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:FL11") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 9: Reflection under FL11, DIN Table 9. Bold red differences exceed
the reproduction limit.

R code: LED-B1 reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:LED_B1") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 10: Reflection under LED-B1, DIN Table 10.

R code: LED-B2 reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:LED_B2") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 11: Reflection under LED-B2, DIN Table 10.

R code: LED-B3 reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:LED_B3") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 12: Reflection under LED-B3, DIN Table 10.

R code: LED-B5 reflection table

``` r

wide |>
  filter(mode == "Reflection", source_id == "CIE:LED_B5") |>
  arrange(din_id) |>
  result_table()
```

[TABLE]

Table 13: Reflection under LED-B5, DIN Table 10.

## Conclusion

Derive the conclusion from this run

``` r

outside <- filter(paired, !within_budget)
cat(sprintf(paste0(
  "**%d of %d confirmed coefficients** lie within the project reproduction limit of ±0.001; ",
  "%d already lie within the stricter rounding interval of ±0.0005. ",
  "The remaining %d coefficients exceed the reproduction limit.\n\n"
), sum(paired$within_budget), nrow(paired), sum(paired$within_rounding), nrow(outside)))
```

**431 of 456 confirmed coefficients** lie within the project
reproduction limit of ±0.001; 417 already lie within the stricter
rounding interval of ±0.0005. The remaining 25 coefficients exceed the
reproduction limit.

Derive the conclusion from this run

``` r

if (nrow(outside)) {
  affected <- outside |> distinct(mode, response, illuminant)
  if (nrow(affected) == 1L) {
    cat(sprintf("All exceedances concern **%s %s under %s**.\n\n",
      tolower(response_labels[affected$response]), tolower(affected$mode), affected$illuminant))
  }

  worst <- slice_max(outside, abs(difference_pp), n = 1, with_ties = FALSE)
  cat(sprintf(paste0(
    "The largest absolute difference is **DIN %d, %s, %s**: ",
    "DIN %s, Spectran %s, Δ %s percentage points.\n\n"
  ), worst$din_id, worst$material, worst$illuminant,
    fmt(worst$din, 3), fmt(worst$spectran, 6), fmt(worst$difference_pp, 4)))
}
```

All exceedances concern **photopic reflection under FL11**.

The largest absolute difference is **DIN 424, Beech, oiled, FL11**: DIN
0.438, Spectran 0.416790, Δ -2.1210 percentage points.

The module and the explicit weighted sums agree for all tested inputs.

The DIN photopic reflection coefficients for FL11 have a known deviation
from the correct values. According to the author of the DIN source,
Spectran calculates these coefficients correctly, and a correction of
the DIN values is planned for the next edition of DIN/TS 67600.[^1]

For the remaining **32 DIN materials**, no confirmed spectrum is
assigned. Two integrated coefficients are insufficient to reconstruct a
unique material spectrum. A complete spectral validation of all 71 DIN
materials is therefore not supported by the available specimen
documentation.

These checks assess numerical reproduction for documented spectra and
calculation conventions. They do not replace independent material
measurements. Applying the values to an actual room or product still
requires the relevant specimen properties and measurement geometry.

## Reproduction

### Inputs and exports

This article runs from the Spectran source tree. Supplying `output-dir`,
or the environment variable `SPECTRAN_MATERIAL_OUTPUT_DIR`, writes the
unrounded results as CSV and an R object. `comparison-long.csv` includes
weighted numerators and denominators; `comparison-wide.csv` places the
two responses side by side. `material-map.csv` records the source column
for every confirmed assignment. The data behind the overview figure and
both sets of thumbnails are also exported as CSV. Without an explicit
output directory, these files are written to
`data-raw/intermediate/material-validation/`.

Export results and record input checksums

``` r

input_files <- file.path(root, c(
  "DESCRIPTION", "renv.lock", "R/sysdata.rda", "data/Specs.rda", "data/ColorP.rda",
  "data/examplespectra.rda", "data/examplespectra_descriptor.rda",
  "R/spectral_metrics.R", "R/transmission_core.R", "R/material_core.R",
  "R/material_light_level.R", "R/transmission_catalogue.R", "R/import_verifier.R",
  "inst/extdata/tub/Spectral_transmittance.csv", "inst/extdata/tub/Spectral_reflectance.csv",
  "vignettes/articles/Material_Validation.qmd", "vignettes/articles/Material_Validation.css"
))
input_files <- c(input_files, din_file, assignment_file)
stopifnot(all(file.exists(input_files)))
input_hashes <- tibble(
  File = substring(input_files, nchar(root) + 2L),
  MD5 = unname(tools::md5sum(input_files))
)

output_dir <- params[["output-dir"]]
if (!nzchar(output_dir)) output_dir <- Sys.getenv("SPECTRAN_MATERIAL_OUTPUT_DIR", "")
if (!nzchar(output_dir)) output_dir <- file.path(root, "data-raw/intermediate/material-validation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

material_thumbnail_data <- list_rbind(material_thumbnails, names_to = "catalogue_id")
source_thumbnail_data <- list_rbind(source_thumbnails, names_to = "source_id")
outputs <- list(
  "comparison-long" = comparison, "comparison-wide" = wide,
  "summary-by-response" = summary_by_response, "summary-by-source" = summary_by_source,
  "material-map" = materials, "source-inventory" = sources,
  "missing-materials" = missing_inventory, "input-hashes" = input_hashes,
  "figure-overview-data" = figure_overview_data,
  "material-thumbnails-data" = material_thumbnail_data,
  "source-thumbnails-data" = source_thumbnail_data
)
iwalk(outputs, function(data, name) {
  write.csv(data, file.path(output_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
})

saveRDS(list(calculated = calculated, comparison = comparison,
  example_terms = example_terms, inputs = input_hashes),
  file.path(output_dir, "validation-results.rds"))
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
```

Input files and checksums for this render

Format the input manifest

``` r

input_hashes |>
  gt() |>
  style_table() |>
  tab_options(table.font.size = px(11))
```

| File | MD5 |
|----|----|
| DESCRIPTION | 9bd5f03dd03e88361c51dabaf08ad9e0 |
| renv.lock | 69cfe4789487d2c8e49dac6c306574bb |
| R/sysdata.rda | cffd8b740f522fc84af73913ab0edb11 |
| data/Specs.rda | 90a9bbcef9bf7ccbb1429cac2145f98e |
| data/ColorP.rda | 8016fb41e1d02c8e07e0ad42f512424e |
| data/examplespectra.rda | edb897f3f8d7a0d845e8741f9481b296 |
| data/examplespectra_descriptor.rda | 4fd7a8a462bc893b42db0ff1eb12e58f |
| R/spectral_metrics.R | f074ef6ca76f4588ef28ac960f035815 |
| R/transmission_core.R | 7d2030d7a2f2ab7ad4961c668563ef7f |
| R/material_core.R | 05b12b1d69551516739c8eb0f2362e89 |
| R/material_light_level.R | 337c99eb6f91b2f197986216246a1773 |
| R/transmission_catalogue.R | 7ad87b23814786d94da9055788fff5da |
| R/import_verifier.R | 40cb873dd14e0ebb253e1c86f5a26aa4 |
| inst/extdata/tub/Spectral_transmittance.csv | 5dbe247ebda24796eac44e2405f00bb7 |
| inst/extdata/tub/Spectral_reflectance.csv | 8b9e994d68d0327428a86b1ed536f772 |
| vignettes/articles/Material_Validation.qmd | 7cc84ec760ac56016281f931e80c1778 |
| vignettes/articles/Material_Validation.css | 38db57d9f51ba12ffd6bf24731ee2892 |
| vignettes/articles/material-validation-data/din-67600-material-coefficients.csv | dbd293e23ec5d33b6f521e2d55fe4f9c |
| vignettes/articles/material-validation-data/tub-assignments.csv | 5dfd2f6c4393572e81edd0285ab5bcb5 |

Table 14: Input provenance for this render. Paths are relative to the
package root where possible.

### Render commands

The prepared CSV is included with the article. From the package root,
render the standalone article with:

``` bash
quarto render vignettes/articles/Material_Validation.qmd
```

To choose a directory for the unrounded results and plot data:

``` bash
quarto render vignettes/articles/Material_Validation.qmd \
  -P output-dir:/path/material-validation
```

The same Quarto source is registered in pkgdown’s Articles menu. The
website build requires **pkgdown ≥ 2.2.1**, the R package **quarto**,
and a Quarto CLI:

``` r

pkgdown::build_site(preview = FALSE)
```

CSV preparation can be repeated separately using
`data-raw/prepare_din_material_coefficients.R` and a local, licensed
text extraction. Neither the licensed PDF nor its extracted text is
needed to render the article.

### Software

The version table describes the R environment and Quarto tools used for
this render.

Collect the software versions

``` r

packages <- c("Spectran", "pkgload", "dplyr", "tidyr", "purrr", "ggplot2", "gt",
  "knitr", "rmarkdown", "pkgdown", "quarto")
software_versions <- tibble(
  Component = c("R", "Quarto CLI", packages),
  Version = c(as.character(getRversion()), system2("quarto", "--version", stdout = TRUE),
    map_chr(packages, function(p) as.character(packageVersion(p))))
)
write.csv(software_versions, file.path(output_dir, "software-versions.csv"), row.names = FALSE)
```

Format the software record

``` r

software_versions |>
  gt() |>
  style_table()
```

| Component  | Version    |
|------------|------------|
| R          | 4.6.1      |
| Quarto CLI | 1.9.37     |
| Spectran   | 2.0.0.9000 |
| pkgload    | 1.5.3      |
| dplyr      | 1.2.1      |
| tidyr      | 1.3.2      |
| purrr      | 1.2.2      |
| ggplot2    | 4.0.3      |
| gt         | 1.4.0      |
| knitr      | 1.52       |
| rmarkdown  | 2.32       |
| pkgdown    | 2.2.1      |
| quarto     | 1.5.1      |

Table 15: Software versions used to render this article.

The reference basis is DIN/TS 67600:2022-08, Section 6.2.4 and Tables
6–10, and the [TUB dataset, version
2](https://doi.org/10.14279/depositonce-11893.2). The weighting
functions and bundled illuminants are also described in [Underlying
values](https://litgde.github.io/Spectran/articles/Values.md).

[^1]: Private correspondence between Johannes Zauner and Dieter Lang, 8
    October 2026.
