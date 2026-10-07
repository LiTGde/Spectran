test_that("spectral axes are explicit while integrated metric labels stay unchanged", {
  old_language <- the$language
  old_palette <- the$palette
  withr::defer({ the$language <- old_language; the$palette <- old_palette })
  the$palette <- "Lang"
  source <- transmission_source_fixture("d65")
  for (locale in c("English", "Deutsch")) {
    the$language <- locale
    plot <- Plot_hull(source, "Source", 5)
    age <- Plot_age_basis(Spectrum = source, Spectrum_Name = "Source", maxE = 5,
      plot_multiplier = 1, subtitle = "Age")
    expect_identical(plot$labels$y, age$labels$y)
    expect_match(plot$labels$y, if (locale == "English") "Spectral irradiance" else "Spektrale Bestrahlungsstärke", fixed = TRUE)
    expect_match(plot$labels$y, "\n(mW/m²/nm)", fixed = TRUE)
    expect_false(grepl("pectral|pektral", lang$server(40)))
  }
})

test_that("the age transmission inset can be disabled at screen and export widths", {
  old_language <- the$language
  old_palette <- the$palette
  withr::defer({ the$language <- old_language; the$palette <- old_palette })
  the$language <- "English"
  the$palette <- "Lang"
  source <- transmission_source_fixture("d65")
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  for (width in c(3, 6.5)) for (inset in c(FALSE, TRUE)) {
    plot <- Plot_age_trans(Spectrum = source, Spectrum_Name = "Age comparison",
      maxE = max(source$Bestrahlungsstaerke) * 1000, plot_multiplier = 1,
      subtitle = "Seventy-year-old compared with a 32-year-old observer", Age = 70,
      alpha = .85, Spectrum_mel_wtd = source$Bestrahlungsstaerke * .5,
      Alter_mel = TRUE, Alter_inset = inset, Alter_rel = FALSE, age_scale = NULL,
      font_size = 9, plot_width = width)
    expect_no_error(print(plot))
  }
})

test_that("radiometric and weighted foreground spectra use identical path strokes", {
  old_language <- the$language
  old_palette <- the$palette
  withr::defer({ the$language <- old_language; the$palette <- old_palette })
  the$language <- "English"
  the$palette <- "Lang"
  source <- transmission_source_fixture("d65")
  radio <- Plot_Main(source, "Source", 5, "Melanopsin", subtitle = "Radiometry")
  weighted <- Plot_Main(source, "Source", 5, "Melanopsin", subtitle = "Weighted",
    Sensitivity_Spectrum = source$Bestrahlungsstaerke * .5, alpha = .85)
  paths <- function(plot) {
    built <- ggplot2::ggplot_build(plot)
    built$data[vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPath"), logical(1))]
  }
  expect_equal(unique(paths(radio)[[1]]$linewidth), 1.2)
  expect_equal(unique(paths(weighted)[[2]]$linewidth), unique(paths(radio)[[1]]$linewidth))
  expect_equal(unique(paths(weighted)[[1]]$linewidth), .5)
})
