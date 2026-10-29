test_that("the development notice and hosted language switch are localized", {
  old_language <- the$language
  withr::defer(the$language <- old_language)
  targets <- c(
    Deutsch = "https://litg-spectran-en.share.connect.posit.cloud/",
    English = "https://litg-spectran-de.share.connect.posit.cloud/"
  )
  for (setting in names(targets)) {
    the$language <- setting
    notice <- as.character(spectran_startup_notice(setting))
    expect_match(notice,
      "https://litgde.github.io/Spectran/articles/Material_Validation.html", fixed = TRUE)
    expect_match(notice, 'aria-labelledby="spectran-prototype-title"', fixed = TRUE)
    expect_match(notice, if (setting == "Deutsch") "Entwicklungs-Prototyp" else "development prototype", fixed = TRUE)
    expect_match(notice, if (setting == "Deutsch") "Spectran starten" else "Start Spectran", fixed = TRUE)
    expect_match(as.character(UI_Header(TRUE)), targets[[setting]], fixed = TRUE)
    expect_false(grepl(targets[[setting]], as.character(UI_Header(FALSE)), fixed = TRUE))
  }
})

test_that("each app session shows the startup notice once across navigation", {
  old_language <- the$language
  old_palette <- the$palette
  withr::defer(the$language <- old_language)
  withr::defer(the$palette <- old_palette)
  shown <- list()
  testthat::local_mocked_bindings(
    showModal = function(ui, session = shiny::getDefaultReactiveDomain()) {
      shown[[length(shown) + 1L]] <<- as.character(ui)
    },
    .package = "shiny"
  )

  for (setting in c("Deutsch", "English")) {
    shown <- list()
    shiny::testServer(Spectran(lang_setting = setting), {
      # testServer does not populate UI defaults for the two spectrum builders.
      session$setInputs(
        inTabset = "tutorial",
        `import-eigen-sensitivitaeten` = unname(Specs$Plot$Names[[1L]]),
        `material_source-picker-eigen-sensitivitaeten` = unname(Specs$Plot$Names[[1L]])
      )
      expect_length(shown, 1L)
      expect_match(shown[[1L]], if (setting == "Deutsch") "Entwicklungs-Prototyp" else "development prototype", fixed = TRUE)
      session$setInputs(inTabset = "import")
      session$setInputs(inTabset = "validity")
      session$setInputs(inTabset = "tutorial")
      session$flushReact()
      expect_length(shown, 1L)
    })
  }
})
