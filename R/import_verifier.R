# UI ----------------------------------------------------------------------

# Server ------------------------------------------------------------------

show_import_verifier_alert <- function(notification) {
  if (!is.list(notification)) {
    return(invisible(NULL))
  }
  shinyalert::shinyalert(
    notification$title,
    notification$message,
    type = notification$type,
    showConfirmButton = isTRUE(notification$show_confirm_button)
  )
  invisible(NULL)
}

import_verifierServer <-
  function(id, Spectrum = NULL) {
    shiny::moduleServer(id, function(input, output, session) {
      #Adjusting the Spectrum after import
      shiny::observeEvent(
        Spectrum$import_attempt,
        {
          Spectrum_raw <- Spectrum$Spectrum_raw
          shiny::req(Spectrum_raw)

          #Very easy spectral setup, should there be 1nm steps between 380-780
          if (
            all(
              identical(Spectrum_raw[[1]], 380:780),
              (Spectrum_raw[, 2] %>% is.na() %>% sum()) == 0
            )
          ) {
            notification <- list(
              title = "OK",
              message = lang$server(28),
              type = "success",
              show_confirm_button = identical(
                Spectrum$Destination,
                lang$ui(94)
              )
            )

            normalized_spectrum <-
              tibble::tibble(
                Wellenlaenge = 380:780,
                Bestrahlungsstaerke = Spectrum_raw[[2]]
              )
          } else {
            #Slightly more work to to, if not
            shiny::req(Spectrum$Spectrum_raw)
            notification <- list(
              title = lang$server(29),
              message = lang$server(30),
              type = "info",
              show_confirm_button = identical(
                Spectrum$Destination,
                lang$ui(94)
              )
            )

            temp <-
              tibble::tibble(
                Wellenlaenge = Spectrum_raw[[1]],
                Bestrahlungsstaerke = Spectrum_raw[[2]]
              )
            temp <-
              temp %>%
              tibble::add_row(
                Wellenlaenge = c(379, temp$Wellenlaenge %>% min() - 1),
                Bestrahlungsstaerke = c(0, 0),
                .before = 1
              )
            temp <-
              temp %>%
              tibble::add_row(
                Wellenlaenge = c(temp$Wellenlaenge %>% max() + 1, 781),
                Bestrahlungsstaerke = c(0, 0)
              )
            r_fun <- stats::approxfun(x = temp[[1]], y = temp[[2]])
            normalized_spectrum <-
              tibble::tibble(
                Wellenlaenge = 380:780,
                Bestrahlungsstaerke = r_fun(380:780)
              )
          }

          import_request <- list(
            source_id = paste0(
              "spectran-import-",
              as.integer(shiny::isolate(Spectrum$revision) %||% 0L) + 1L
            ),
            source_name = Spectrum$Name,
            spectrum = normalized_spectrum,
            origin = Spectrum$Origin %||% "Import",
            destination = Spectrum$Destination,
            provenance = list(
              origin = Spectrum$Origin %||% "Import",
              imported_name = Spectrum$Name
            ),
            notification = notification
          )
          import_guard <- shiny::isolate(Spectrum$import_guard)
          if (is.function(import_guard)) {
            restore_spectran_committed_state(Spectrum)
            import_guard(import_request)
            return(invisible(NULL))
          }

          activate_spectran_spectrum(
            Spectrum = Spectrum,
            spectrum = import_request$spectrum,
            name = import_request$source_name,
            origin = import_request$origin,
            change_type = "import",
            node_id = "node-1",
            provenance = import_request$provenance,
            destination = import_request$destination
          )
          show_import_verifier_alert(import_request$notification)
        },
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )
    })
  }

# App ---------------------------------------------------------------------
