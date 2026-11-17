age_label_test_plot <- function(age, mode, multiplier = 1, width = 6.5) {
  source <- transmission_source_fixture("d65")
  args <- list(
    Spectrum = source,
    Spectrum_Name = "Age comparison",
    maxE = max(source$Bestrahlungsstaerke) * 1000,
    plot_multiplier = multiplier,
    subtitle = "Age-adjusted melanopsin sensitivity",
    Age = age,
    alpha = 0.85,
    Spectrum_mel_wtd = Spec_wtd(source, "Melanopsin"),
    Alter_mel = TRUE,
    font_size = 9,
    plot_width = width
  )
  if (mode == "transmission") {
    args$Alter_rel <- FALSE
    args$age_scale <- NULL
    do.call(Plot_age_trans_p1, args)
  } else {
    do.call(Plot_age_tot, args)
  }
}

test_that("age labels follow the plotted peak and stay within vertical limits", {
  old_language <- the$language
  old_palette <- the$palette
  withr::defer({
    the$language <- old_language
    the$palette <- old_palette
  })
  the$language <- "English"
  the$palette <- "Lang"

  for (mode in c("total", "transmission")) {
    for (age in c(0, 10, 32, 50, 100)) {
      for (multiplier in c(1, 2)) {
        plot <- age_label_test_plot(age, mode, multiplier)
        built <- ggplot2::ggplot_build(plot)
        curve <- built$data[[length(built$data) - 1L]]
        label <- built$data[[length(built$data)]]
        peak <- curve[which.max(curve$y), ]
        limit <- max(plot$data$Bestrahlungsstaerke) * 1000 * multiplier

        expect_equal(label$x, peak$x)
        expect_equal(label$x_orig, peak$x)
        expect_equal(label$y_orig, peak$y)
        expect_gte(label$y, 0)
        expect_lte(label$y, limit)
        if (peak$y > limit) {
          expect_lt(label$y, peak$y)
        } else {
          expect_gte(label$y, peak$y)
        }
      }
    }
  }
})

test_that("rendered age labels stay aligned and visible when young peaks are high", {
  old_language <- the$language
  old_palette <- the$palette
  withr::defer({
    the$language <- old_language
    the$palette <- old_palette
  })
  the$language <- "English"
  the$palette <- "Lang"

  for (width in c(3, 6.5)) {
    grDevices::pdf(NULL, width = width, height = 3.2)
    tryCatch(
      {
        for (mode in c("total", "transmission")) {
          plot <- age_label_test_plot(0, mode, width = width)
          built <- ggplot2::ggplot_build(plot)
          curve <- built$data[[length(built$data) - 1L]]
          peak <- curve[which.max(curve$y), ]
          panel <- built$layout$panel_params[[1L]]

          grid::grid.newpage()
          grid::grid.draw(ggplot2::ggplotGrob(plot))
          grid::grid.force()
          viewports <- grid::grid.ls(
            viewports = TRUE,
            grobs = FALSE,
            print = FALSE
          )
          panel_viewport <- viewports$name[grepl("^panel[.]", viewports$name)]
          grid::seekViewport(panel_viewport[[1L]])
          label <- grid::grid.get("textrepelgrob1")
          box <- grid::grid.get("rectrepelgrob1")
          expect_false(is.null(label))
          expect_false(is.null(box))
          expect_equal(
            grid::convertX(label$x, "npc", valueOnly = TRUE),
            (peak$x - panel$x.range[[1L]]) / diff(panel$x.range),
            tolerance = 1e-7
          )
          expect_gt(grid::convertY(label$y, "npc", valueOnly = TRUE), 0)
          expect_lt(grid::convertY(label$y, "npc", valueOnly = TRUE), 1)
          expect_lte(
            grid::convertY(grid::grobY(box, "north"), "npc", valueOnly = TRUE),
            1
          )
          grid::upViewport(0)
        }
      },
      finally = grDevices::dev.off()
    )
  }
})
