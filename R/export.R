
# UI ----------------------------------------------------------------------

exportUI <- 
  function(
    id
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList(
      #header, then box that contains everything
        shiny::h3("Export"),
        shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          title = lang$ui(124),
          #Introduction
          htmltools::p(
                    lang$ui(125),
                    htmltools::em(.noWS = "outside", lang$ui(23)),
                    lang$ui(126),
                    htmltools::em(.noWS = "outside", lang$ui(127)),
                    lang$ui(128)
                  ),
                  shiny::column(
                    width = 6,
                    #Define the outputs
                    export_define_OutputUI(ns("Outputs")),
                    #General Settings
                    export_general_settingsUI1(ns("Analysis_Settings")),
                    htmltools::br()
                  ),
          shiny::column(
                    width = 6,
                    offset = 0,
                    export_general_settingsUI2(ns("Analysis_Settings")),
                    #Output Settings
                    export_Output_SettingsUI(ns("Export_Settings"))
                  ),
          #Download-Button
                  shinydashboard::box( class = "background-rect",
                    width = 12,
                    align = "center",
                    shiny::downloadButton(ns("download_button"), 
                                          label = 
                                            htmltools::strong("Downloads"),
                                          class = "btn-lg"
                                          )
                  )
          )
        )
    )
    }

# Server ------------------------------------------------------------------

exportServer <- 
  function(
    id, 
    Analysis,
    Spectrum
  ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    #what plots should be exported
    outputs <- export_define_OutputServer("Outputs")
    
    #manage general settings
    export_general_settingsServer("Analysis_Settings", Analysis, Spectrum)
    
    #create the output files
    Export <- export_Output_SettingsServer("Export_Settings", Analysis, outputs)
    
    #create a reactive value for Files
    Export_files <- shiny::reactiveVal(NULL)
    
    # shiny::observe({
    #   shiny::req(Export$Plot)
    #   
    #   Export$files <-
    #     Export$Plot
    # })
    
    #Download-Button
    output$download_button <- downloadHandler(
      
      #Filename
      filename = function() {
        paste(
          Analysis$Settings$Spectrum_Name, "_", Sys.Date(), ".zip", sep="")},
      #Content
      content = function(file) {
        
        #Spectrum Name
        Spectrum_Name <- Analysis$Settings$Spectrum_Name
        
        #Shinyalert with preparation message
        shinyalert::shinyalert(
          title = paste0("Export: ", Spectrum_Name),
          text = paste0(lang$server(114)),
          type = "success",
          timer = 5000,
          closeOnClickOutside = TRUE,
          showConfirmButton = FALSE
          )
        
        #Create a temporary directory that will overwrite the workin directory
        #on exit. Also reset the Export_files list.
        owd <- setwd(tempdir())
        on.exit({setwd(owd)})
        Export_files(NULL)
        Export$Tables <- NULL
        # Table_pics(NULL)
        
        #Save Tables
        purrr::map(Export$Table_prep, \(args) { 
          if(!is.null(args)) do.call("table_download", args = c(args))
        })
        
        #Export Plots
        purrr::map(Export$Plot, \(args) {           
          do.call("plot_download",
                  args = c(args, 
                           Export_files = Export_files))
        })
        
        
        
        # withProgress(message = lang$server(115), value = 0, {
          
        #   
        #   if(input$export_tab | lang$ui(144) %in% input$export_typ) {
        #     withProgress(message = lang$server(116), value = 0, {
        # 
        #       pmap(
        #         extracto_table(1:dim(Table_data$tabelle)[1]),
        #         tabelle_download
        #       )
        #     })
        #   }
        #         
        # Setting up all the plotfiles

        #   setProgress(length(Plot_data$tables)/Plot_data$n_export, detail = paste(lang$server(117), length(Plot_data$tables)))
        #   setProgress( (length(Plot_data$files) + length(Plot_data$tables))/Plot_data$n_export, detail = paste(lang$server(118), length(Plot_data$files)))
        # 
        #add relevant table files to the list
          # if(lang$ui(144) %in% input$export_typ) {
        Export_files(c(Export_files(), Export$Tables))
          # }
        # 
        #   setProgress(value = 1, detail = paste(lang$server(119), length(Plot_data$files)))
        
            #Export Excel
            wb <- NULL
            if(!purrr::every(Export$Xlsx, is.null)) {
              #Filename
              filename <- paste(Spectrum_Name, "_", Sys.Date(), ".xlsx", sep="")
              
              #create a new workbook
              wb <- openxlsx::createWorkbook(Spectrum_Name)
              
              #add worksheets
              excel_sheet(wb, Export$Xlsx[[1]], "Radiometrie")
              
              #save the workbook in a temporary file and write the filenames to list
              openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
              Export_files(c(Export_files(), filename))
            }
            
            
        # export a csv file 
          if(!is.null(Export$CSV)) {
            filename <- 
              paste(Spectrum_Name, 
                    "_", 
                    Sys.Date(), 
                    ".csv", 
                    sep=""
                    )
            readr::write_csv(Export$CSV, filename)
            Export_files(c(Export_files(), filename))
          }
        
        # give the files a sequential numbering
          file.rename(from = Export_files(), 
                      to = renaming(Export_files(),
                                    Spectrum_Name)
                      )
          
        # create the zip file for download
          utils::zip(file, renaming(Export_files(),
                                    Spectrum_Name)
                     )
        # })

      }
    )
    
    #Return Value
    Export
    
  })
}

# App ---------------------------------------------------------------------

exportApp <- function(lang_setting = "Deutsch", ...) {

    #add a resource path to the www folder
  shiny::addResourcePath(
    "extr", system.file("app/www", package = "Spectran"))
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)
  
  #set the language for the program
  the$language <- lang_setting
  
  #UI
  ui <-
    shinydashboard::dashboardPage( skin = "yellow",
                                   #Header
                                   UI_Header(),
                                   #Sidebar
                                   UI_Sidebar(),
                                   #Body
                                   shinydashboard::dashboardBody(
                                     #Add a link to the css resource
                                     htmltools::tags$link(
                                       rel = "stylesheet", type="text/css", 
                                       href="extr/style.css"),
                                     shiny::verbatimTextOutput("Data_ok"),
                                     gt::gt_output("alpha_table"),
                                     shinydashboard::tabItems(
                                       #add a tab for the introduction
                                       shinydashboard::tabItem(
                                         tabName = "tutorial",
                                         introductionUI("intro")),
                                       #add a tab for the import
                                       shinydashboard::tabItem(
                                         tabName = "import",
                                         importUI("import")),
                                       #add a tab for the analysis
                                       shinydashboard::tabItem(
                                         tabName = "analysis",
                                         analysisUI("analysis")),
                                       #add a tab for the export
                                       shinydashboard::tabItem(
                                         tabName = "export",
                                         exportUI("export")),       
                                       #add a tab for the validity
                                       shinydashboard::tabItem(
                                         tabName = "validity",
                                         validityUI("validity")),
                                       #add a tab for the impressum
                                       shinydashboard::tabItem(
                                         tabName = "impressum",
                                         impressumUI("impressum"))
                                     )
                                   )
    )
  
  #Server
  server <- function(input, output, session) {
    
    #Introduction
    zu_Import <- introductionServer("intro")
    
    #Import
    test.spectrum <-  
      examplespectra$Measurement %>% 
      dplyr::select(Wellenlaenge, equ) %>% 
      dplyr::rename(Bestrahlungsstaerke = 2)
    
    Spectrum <- 
      shiny::reactiveValues(Spectrum = test.spectrum, 
                            Name = "Test", 
                            Origin = "Import",
                            Destination = lang$ui(69))
    
    # Spectrum <- importServer("import")
    
    #Analysis
    Analysis <- analysisServer("analysis",
                               Spectrum = Spectrum)
    
    #Export
    Export <- exportServer("export", Analysis, Spectrum)
    
    # Delete Notifications between tab changes
    notification_remover(shiny::reactive(input$inTabset))
    
    #Update the Navbar, when the Introduction is finished
    shiny::observe({
      shiny::updateNavbarPage(
        session, inputId = "inTabset" ,  selected = "import")
    }) %>% shiny::bindEvent(
      zu_Import(), ignoreInit = TRUE
    )
    
    #Update the Navbar when a Spectrum is imported
    shiny::observe({
      shiny::req(Spectrum$Spectrum, Spectrum$Destination)
      if(Spectrum$Destination == lang$ui(69) &
         input$inTabset == "import") {
        shiny::updateNavbarPage(
          session,
          inputId = "inTabset",
          selected = "analysis")
      }
    }) %>% shiny::bindEvent(Spectrum$Spectrum, Spectrum$Destination,
                            Spectrum$Name)
    
    #Update the Navbar when hitting the Export-Button in Analysis
    shiny::observe({
        shiny::updateNavbarPage(
          session,
          inputId = "inTabset",
          selected = "export")
    }) %>% shiny::bindEvent(Analysis$to_export)
    
    # output$alpha_table <- gt::render_gt(
    #   Table_alpha(
    #     Analysis,
    #     show = Specs$Alpha$names
    #     )
    #   )
    
    output$Data_ok <- shiny::renderPrint({
      #     print("Developer Troubleshoot\n")
      # p990rint(list.files(path = "extr/"))
      # print(Export$CSV)
      # print(Analysis$table_Radiometrie$internal)
      print(Export$Table_prep)
      #     print(Spectrum$Other)
      #     print(Spectrum$Spectrum %>% utils::head())
      #     print(Spectrum$Spectrum %>% utils::tail())
    })
    
  }
  shiny::shinyApp(ui, server, ...)
}
