
# UI ----------------------------------------------------------------------

export_Output_SettingsUI <- 
  function(id) {
    ns <- shiny::NS(id)
    htmltools::tagList(
      htmltools::h4(htmltools::strong(lang$ui(141))),
      shiny::checkboxGroupInput(
        ns("export_typ"),
        label = lang$ui(142),
        choices = c(
          lang$ui(143),
          lang$ui(144),
          lang$ui(145),
          lang$ui(146)
        ),
        selected = lang$ui(143),
        inline = TRUE
      ),
      shiny::checkboxInput(
        ns("export_tab"), label = lang$ui(147)),
      htmltools::h5(htmltools::strong(lang$ui(148))),
      shiny::splitLayout(
        shiny::numericInput(
          ns("plot_width"),
          label = lang$ui(149),
          min = 1,
          value = 6.5
        ),
        shiny::numericInput(
          ns("plot_height"),
          label = lang$ui(150),
          min = 1,
          value = 3.2
        )
      ),
      shiny::numericInput(
        ns("font_size"),
        label = lang$ui(151),
        min = 1,
        value = 9
      ),
      shinyFeedback::useShinyFeedback(),
      shiny::textInput(ns("scale_max"),
                       label = paste0(lang$ui(152), " (mW/m\u00b2*nm)"),
      )
    )
  }

# Server ------------------------------------------------------------------

export_Output_SettingsServer <- 
  function(
    id,
    Analysis,
    outputs,
    Tabactive) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #Check wether a CIE spectrum is to be downloaded
      shiny::observe({
        shiny::req(Analysis$Settings$Origin,
            Tabactive() == "export")
        if(
          Analysis$Settings$Origin == "CIE" &
        lang$ui(146) %in% input$export_typ) {

          shinyalert::shinyalert(
            lang$server(134),
            lang$server(133),
            type = "info",
            # timer = 10000,
            html = TRUE,
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonCol = "#F8E350",
            confirmButtonText = "Download",
            callbackR = function(x) {
            if(!x) {
              shiny::updateCheckboxGroupInput(
                session,
                "export_typ",
                selected = setdiff(input$export_typ, lang$ui(146)))
            }
            }
            )
        
        }
      }) %>% shiny::bindEvent(input$export_typ, ignoreInit = TRUE)
      
      #Creates a numeric out the text Input
      numConv <- shiny::reactive({as.numeric(input$scale_max)})
      
      #Warning, if the Maximum scale is neither numeric nor empty
      shiny::observeEvent(input$scale_max, {
        shinyFeedback::feedbackDanger(
          "scale_max",
          input$scale_max != "" & (is.na(numConv()) | numConv() <= 0),
          lang$server(102)
        )
      })
      
      #uncheck table for graphics, if no graphics are exported
      shiny::observe({
        if(!lang$ui(143) %in% input$export_typ) {
        shiny::updateCheckboxInput(session, "export_tab", value = FALSE)
        }
      }) %>% shiny::bindEvent(input$export_typ, input$export_tab)
      
      # Set a factor for the plotheight, if gets combined with a table
      Plot_multiplikator <- 0.80
      
      #create an export table
      Export <- shiny::reactiveValues(Plot = list())
      
      #create the table_settings
      shiny::observe({
        shiny::req(Analysis,
                   Tabactive() == "export")
        
        Export$Table_prep <- outputs$exp %>% purrr::imap(
          \(val, feed) {
            Age <- outputs$exp[9:11]
            
            #create a table count
            if(!any(feed %in% Specs$Alpha$names == TRUE,
                    feed == lang$server(129),
                    feed %in% c(lang$server(127), lang$server(128)) & 
                    Age[[lang$server(125)]] == 1,
                    feed %in% c(lang$server(127), lang$server(128)) & 
                    Age[[lang$server(127)]] == 1 &
                    Age[[lang$server(128)]] == 1) & 
               lang$ui(144) %in% input$export_typ){
              Export$Download_tables[[feed]] <- val}
            else Export$Download_tables[[feed]] <- 0
            
            if(input$export_tab | lang$ui(144) %in% input$export_typ) {

              #standard tables
              table_exp_args(
                plot_width = input$plot_width,
                plot_height = input$plot_height,
                export_tab = input$export_tab,
                export_typ = input$export_typ,
                val = val,
                feed = feed,
                Analysis = Analysis,
                Export = Export,
                Age = Age,
                Alpha = outputs$exp[3:7]
                )
            } 
          }
        )
      })
      
      #create the Plot-settings
      shiny::observe({
        shiny::req(Analysis, 
                   outputs$exp,
                   Tabactive() == "export")

        Export$Plot <- outputs$exp %>% purrr::imap(
          \(val, feed) {
            if(lang$ui(143) %in% input$export_typ) {
            plot_exp_args(
              numConv = numConv(),
              export_tab = input$export_tab,
              feed = feed,
              plot_height = input$plot_height,
              plot_width = input$plot_width,
              Plot_multiplikator = Plot_multiplikator,
              font_size = input$font_size,
              Analysis = Analysis,
              val = val,
              Spectrum_Name = Analysis$Settings$Spectrum_Name,
              Export = Export
            )
            }
          }
        )
      })
        
        #create a Excel File of the data
        shiny::observe({
          shiny::req(Analysis,
                     Tabactive() == "export")
          
          Export$Xlsx <- outputs$exp %>% purrr::imap(
            \(val, feed) {
              if(lang$ui(145) %in% input$export_typ) {
                tables <- dplyr::case_match (
                  feed,
                  c(lang$server(127), 
                    lang$server(125),
                    lang$server(128)) ~ "Age",
                  .default = feed,
                  .ptype = "list"
                )
                table <- table_export_prep(tables, val, Analysis)
                if(feed == lang$server(127) & 
                   !is.null(table)) {
                  if(outputs$exp[[lang$server(125)]] == 0) {
                    table %>% dplyr::slice(3) %>% xlsx_col_rename()
                  }
                }
                else if (feed == lang$server(128) & 
                         !is.null(table)) {
                  if(outputs$exp[[lang$server(125)]] == 0) {
                  table %>% dplyr::slice(4) %>% xlsx_col_rename()
                  }
                }
                else if (feed == lang$server(126) &
                         !is.null(table)) {
                  Alpha <- outputs$exp[3:7]
                  show <- Filter(\(x) {x}, Alpha) %>% unlist() %>% names()
                  table <- table %>% 
                    dplyr::select(c(Groesse, Formelzeichen, show, Einheit)) %>% 
                    dplyr::mutate(
                      Groesse = Groesse %>% stringr::str_replace_all(
                        pattern = "&|\\;", replacement = ""))
                  table %>% xlsx_col_rename()
                }
                else table %>% xlsx_col_rename()
              } 
            }
          )
          })
        
        #create a csv of the data
        shiny::observe({
          shiny::req(Analysis,
                     Tabactive() == "export")
          Export$CSV <- {
          if(lang$ui(146) %in% input$export_typ) {
            temp_data <- Analysis$Settings$Spectrum
            names(temp_data) <- c(lang$server(31), lang$server(32))
             temp_data
          }
          }
        })
      
      #Return value
      Export
      
    })
  }
 