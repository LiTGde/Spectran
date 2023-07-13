
# UI ----------------------------------------------------------------------

export_general_settingsUI1 <- 
  function(id) {
    ns <- shiny::NS(id)
    htmltools::tagList(
            #General Settings
            htmltools::h4(htmltools::strong(lang$ui(127))),
            shiny::checkboxInput(ns("export_wirk"), 
                                 label = lang$ui(136)),
            htmltools::div(
              style = "display:inline-block",
              shiny::numericInput(
                ns("export_alter_wert"),
                label = lang$ui(137),
                min = 0,
                max = 100,
                value = 50,
                width = "100%"
              )
            ),
            htmltools::div(
              style = "display:inline-block",
              shiny::actionButton(ns("export_alter_but"), 
                                  lang$ui(138), 
                                  inline = TRUE
              )
            ))
    }

export_general_settingsUI2 <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
            htmltools::div(
              style = "display:inline-block",
              shiny::textInput(
                ns("export_Name"),
                label = lang$ui(139),
                value = lang$ui(61),
                width = "100%"
              )
            ),
            htmltools::div(
              style = "display:inline-block",
              shiny::actionButton(ns("export_Name_but"), 
                                  lang$ui(140), 
                                  inline = TRUE)
            )
    )
  }

# Server ------------------------------------------------------------------

export_general_settingsServer <- 
  function(
    id, 
    Analysis,
    Spectrum,
    Tabactive
  ) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #changing the presence of action spectra
      shiny::observe({
        Analysis$action_spectra <- input$export_wirk
      }) %>% shiny::bindEvent(input$export_wirk, ignoreInit = TRUE)
      
      #change the name, when pressing the button
      shiny::observe({
        Spectrum$Name <- Name_suffix(Spectrum$Origin, input$export_Name)
      }) %>% shiny::bindEvent(input$export_Name_but)
      
      #put any new name into the field
      shiny::observe({
        shiny::updateTextInput(session, "export_Name", value = Spectrum$Name)
      })
      
      #change the age, when pressing the button
      shiny::observe({
        if (Analysis$Age != input$export_alter_wert) {
          shiny::showNotification(
            htmltools::HTML(
              paste0(
                lang$server(103), 
                htmltools::strong(
                  paste0(
                    input$export_alter_wert, 
                    lang$server(104)
                    )
                  ), 
                lang$server(105)
                )
              ),
            type = "message")
          
          Analysis$Age <- input$export_alter_wert
        }
        
      }) %>% shiny::bindEvent(input$export_alter_but)
      
      #put any new age into the field
      shiny::observe({
        shiny::updateNumericInput(
          session, "export_alter_wert", value = Analysis$Age)
      })
      
    })
  }

# App ---------------------------------------------------------------------
