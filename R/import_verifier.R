
# UI ----------------------------------------------------------------------

# Server ------------------------------------------------------------------

import_verifierServer <-
  function(id,
           Spectrum = NULL
           ) {

    shiny::moduleServer(id, function(input, output, session) {

      #Adjusting the Spectrum after import
      shiny::observe({
        
        Spectrum_raw <- Spectrum$Spectrum_raw
        
          #Very easy spectral setup, should there be 1nm steps between 380-780
            if(
              all(
              identical(Spectrum_raw[[1]],380:780),
              (Spectrum_raw[,2] %>% is.na() %>% sum()) == 0)
              ) {
              shinyalert::shinyalert(
                "OK", 
                lang$server(28), 
                type = "success",
                # timer = 10000,
                showConfirmButton = if(Spectrum$Destination == lang$ui(94)) {
                  TRUE
                }
                else FALSE
              )
              
              Spectrum$Destination
              
              Spectrum$Spectrum <- 
                tibble::tibble(
                  Wellenlaenge = 380:780,
                  Bestrahlungsstaerke = Spectrum_raw[[2]]
                )
            }
          #Slightly more work to to, if not
            else {
              shiny::req(Spectrum$Spectrum_raw)
              shinyalert::shinyalert(
                lang$server(29),
                lang$server(30),
                type = "info",
                showConfirmButton = if(Spectrum$Destination == lang$ui(94)) {
                  TRUE
                }
                )

              Spectrum$Destination
              
              temp <-
                tibble::tibble(
                  Wellenlaenge = Spectrum_raw[[1]],
                  Bestrahlungsstaerke = Spectrum_raw[[2]]
                  )
              temp <-
                temp %>%
                tibble::add_row(
                  Wellenlaenge = c(379, temp$Wellenlaenge %>% min()-1),
                  Bestrahlungsstaerke = c(0,0), .before=1
                  )
              temp <-
                temp %>%
                tibble::add_row(
                  Wellenlaenge = c(temp$Wellenlaenge %>% max()+1, 781),
                  Bestrahlungsstaerke = c(0,0)
                  )
              r_fun <- stats::approxfun(x = temp[[1]], y = temp[[2]])
              Spectrum$Spectrum <- 
                tibble::tibble(
                  Wellenlaenge = 380:780,
                  Bestrahlungsstaerke = r_fun(380:780)
                )
            }
        
        Spectrum$Illu <-         
          Spectrum$Spectrum$Bestrahlungsstaerke %>% 
          Calc_lux(Specs$AS_wide, Specs$Efficacy)
        Spectrum$Analysis <- Spectrum$Analysis + 1
      })
    })
  }

# App ---------------------------------------------------------------------
