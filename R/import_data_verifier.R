
# UI ----------------------------------------------------------------------

import_data_verifierUI <-
  function(id, 
           label,
           icon,
           class) {
    htmltools::tagList(
      #Importbutton
      shiny::actionButton(
        shiny::NS(id, "import"),
        label = label,
        icon = icon,
        class = class),
      )
  }

# Server ------------------------------------------------------------------

import_data_verifierServer <-
  function(id,
           Data_ok,
           dat,
           csv_settings,
           Spectrum = NULL,
           Destination = lang$ui(69),
           Name
           ) {
    stopifnot(Data_ok %>% shiny::is.reactive())
    stopifnot(dat %>% shiny::is.reactive())

    shiny::moduleServer(id, function(input, output, session) {

      #Set up a container for the spectra to go into, if it isn´t already
      #defined
      if (is.null(Spectrum)){
        Spectrum <- 
          shiny::reactiveValues(
            Spectrum_raw = NULL, Name = NULL, Destination = NULL
            )
      }
      
      #Error Message, should the Data be not ok, but people want to proceed:
      shiny::observe({
        if(!isTRUE(Data_ok())) {
          # obs$suspend()
          shinyalert::shinyalert(
            lang$server(21), 
            lang$server(22),
            timer = 10000,
            type = "error",
            closeOnClickOutside = TRUE)
        }
        #Data preparation steps should the Data be ok
        else {
            if(identical(dat()[, c(csv_settings()$x_y, csv_settings()$x_y2)],
                         Spectrum$Spectrum_raw) &
               identical(Spectrum$Name, Name()) &
               identical(Spectrum$Destination, Destination)) {
              shinyalert::shinyalert(
                "OK", 
                lang$server(135), 
                type = "info"
              )
            }
          else{
            shiny::showNotification(lang$server(23), type = "message", 
                           duration = 4)
          
          Spectrum$Spectrum_raw <- 
            dat()[, c(csv_settings()$x_y, csv_settings()$x_y2)]
          Spectrum$Destination <- Destination
          Spectrum$Name <- Name_suffix(Spectrum$Origin, Name())
          }
        }
        
      }) %>% shiny::bindEvent(input$import)
     
    })
  }

# App ---------------------------------------------------------------------
