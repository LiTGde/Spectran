
# UI ----------------------------------------------------------------------

export_define_OutputUI <- 
  function(id) {
    ns <- shiny::NS(id)
    htmltools::tagList(
      htmltools::h4(htmltools::strong(lang$ui(129))),
      shiny::checkboxInput(ns("export_all"), 
                           label = htmltools::strong(lang$ui(130))
      ),
      shiny::checkboxInput(ns("export_rad"), 
                           label = (lang$ui(131))),
      shiny::checkboxInput(ns("export_vis"), 
                           label = (lang$ui(132))),
      shiny::checkboxGroupInput(
        ns("export_alpha"),
        label = paste0(Specs$Alpha.ico, lang$ui(133)),
        choices = Specs$Alpha$names,
        inline = TRUE
      ),
      shiny::checkboxInput(ns("export_vergl"), 
                           label = lang$ui(134)),
      shiny::checkboxGroupInput(
        ns("export_alter"),
        label = (lang$ui(135)),
        choices = c(lang$ui(118), lang$ui(117), "Transmission"),
        inline = TRUE
      )
      )
    }

# Server ------------------------------------------------------------------

export_define_OutputServer <- 
  function(
    id) {
    
    shiny::moduleServer(id, function(input, output, session) {

      #global Tracker for the checked Boxes
      export_all_check <- shiny::reactiveVal(FALSE)
      
      #if all has been checked, all will be set to true
      shiny::observe({
        test <- sum(
          input$export_rad,
          input$export_vis,
          input$export_vergl,
          length(input$export_alpha) == length(Specs$Alpha$names),
          length(input$export_alter) == 3
        )
        if(test != 4) {
          
        #for single checkboxes
        c("export_rad", "export_vis", "export_vergl") %>% 
          purrr::map(
          shiny::updateCheckboxInput,
          session = session,
          value = input$export_all
          )
        #for the alphaopic checkboxgroup
        shiny::updateCheckboxGroupInput(
          session, "export_alpha", selected = 
            if(input$export_all) Specs$Alpha$names
          else NA
        )
        #for the alphaopic checkboxgroup
        shiny::updateCheckboxGroupInput(
          session, "export_alter", selected = 
            if(input$export_all) c(lang$ui(118), lang$ui(117), "Transmission")
          else NA
        )
        }
        
      }) %>% shiny::bindEvent(input$export_all, ignoreInit = TRUE)
    
      #if even one is deselected, the all button should uncheck
      shiny::observe({
        test <- all(
          input$export_rad,
          input$export_vis,
          input$export_vergl,
          length(input$export_alpha) == length(Specs$Alpha$names),
          length(input$export_alter) == 3
        )
          shiny::updateCheckboxInput(session, "export_all", value = test)
      })
      
      #Return Value
      output_size <- shiny::reactiveValues()
      
      shiny::observe({
        output_size$exp[[lang$server(39)]] <- sum(input$export_rad)
        output_size$exp[[lang$server(63)]] = sum(input$export_vis)
        output_size$exp$Melanopsin = sum(input$export_alpha %in% "Melanopsin")
        output_size$exp$Erythropsin = sum(input$export_alpha %in% "Erythropsin")
        output_size$exp$Chloropsin = sum(input$export_alpha %in% "Chloropsin")
        output_size$exp$Cyanopsin = sum(input$export_alpha %in% "Cyanopsin")
        output_size$exp$Rhodopsin = sum(input$export_alpha %in% "Rhodopsin")
        output_size$exp[[lang$server(126)]] = 
          (sum(length(input$export_alpha)) >= 1)
        output_size$exp[[lang$server(129)]] = 
          sum(input$export_vergl)
        output_size$exp[[lang$server(127)]] = 
          sum(input$export_alter %in% lang$ui(117))
        output_size$exp[[lang$server(125)]] = 
          sum(lang$ui(118) %in% input$export_alter | 
                sum(length(input$export_alter)) == 2)
        output_size$exp[[lang$server(128)]] = 
          sum(input$export_alter %in% "Transmission")

        
      })
        
      output_size
      
    })
  }

# App ---------------------------------------------------------------------
