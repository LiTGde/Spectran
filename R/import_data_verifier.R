
# UI ----------------------------------------------------------------------

import_data_verifierUI <-
  function(id, 
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           label,
           icon,
           class) {
    tagList(
      #Importbutton
      actionButton(
        NS(id, "import"),
        label = label,
        icon = icon,
        class = class),
      )
  }

# Server ------------------------------------------------------------------

import_data_verifierServer <-
  function(id,
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           Data_ok,
           dat,
           csv_settings,
           Spectrum = NULL,
           Destination = lang$ui(69),
           Name
           ) {
    stopifnot(Data_ok %>% is.reactive())
    stopifnot(dat %>% is.reactive())
    # stopifnot(csv_settings %>% is.reactive())
    
    moduleServer(id, function(input, output, session) {

      #Set up a container for the spectra to go into, if it isn´t already
      #defined
      if (is.null(Spectrum)){
        Spectrum <- 
          reactiveValues(Spectrum_raw = NULL, Name = NULL, Destination = NULL)
      }
      
      #Error Message, should the Data be not ok, but people want to proceed:
      observe({
        if(!isTRUE(Data_ok())) {
          # obs$suspend()
          shinyalert(
            lang$server(21), 
            lang$server(22),
            timer = 10000,
            type = "error",
            closeOnClickOutside = TRUE)
        }
        #Data preparation steps should the Data be ok
        else {
            if(identical(Spectrum$Spectrum, Spectrum$Spectrum_raw) &
               identical(Spectrum$Name, Name()) &
               identical(Spectrum$Destination, Destination)) {
              shinyalert(
                "OK", 
                "Spectrum is already imported", 
                type = "info"
              )
            }
          else{
          showNotification(lang$server(23), type = "message", 
                           duration = 4)
          
          Spectrum$Spectrum_raw <- 
            dat()[, c(csv_settings()$x_y, csv_settings()$x_y2)]
          Spectrum$Destination <- Destination
          Spectrum$Name <- Name()
          }
        }
        
      }) %>% bindEvent(input$import)
     
    })
  }

# App ---------------------------------------------------------------------
