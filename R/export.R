
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
    Spectrum,
    Tabactive
  ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    #what plots should be exported
    outputs <- export_define_OutputServer("Outputs", Tabactive)
    
    #manage general settings
    export_general_settingsServer("Analysis_Settings", Analysis, Spectrum,
                                  Tabactive)
    
    #create the output files
    Export <- export_Output_SettingsServer("Export_Settings", Analysis, outputs,
                                           Tabactive)
    
    #create a reactive value for Files
    Export_files <- shiny::reactiveVal(NULL)
    
    #Download-Button: enable/disable
    shiny::observe({
      shiny::req(Tabactive() == "export", 
                 outputs$exp)
      #produce the total number of download files and set the button
      #accordingly
        #Tables
        Export$n_tables <- purrr::reduce(Export$Download_tables, `+`, .init = 0)
      
        #Plots
        Export$n_plots <-  purrr::map(Export$Plot, \(x) {!is.null(x)}) %>% 
        purrr::reduce(`+`, .init = 0) 
        
        #Excel
        Export$n_excel <-  (purrr::map(Export$Xlsx, \(x) {!is.null(x)}) %>% 
           purrr::reduce(`+`, .init = 0)) != 0
      
        #CSV Export
        Export$n_csv <-  !is.null(Export$CSV)
        
        #sum
        Export$n_total <- 
          Export$n_plots + Export$n_excel + Export$n_csv + Export$n_tables
        
      #update the downloadbutton
      down_button_update(
        "download_button",
        "download",
        htmltools::strong(paste0(" ", lang$server(109)," (n=%s)")),
        n = Export$n_total
        )
    })
    
    #Download-Button
    output$download_button <- downloadHandler(
      
      #Filename
      filename = function() {
        paste(
          Analysis$Settings$Spectrum_Name, "_", Sys.Date(), ".zip", sep="")},
      #Content
      content = function(file) {
        
        #package is needed for the table export to work properly
        # withr::local_package("chromote")
        
        #Spectrum Name
        Spectrum_Name <- Analysis$Settings$Spectrum_Name
        
        #Create a temporary directory that will overwrite the workin directory
        #on exit. Also reset the Export_files list.
        owd <- setwd(tempdir())
        on.exit({setwd(owd)})
        Export_files(NULL)
        Export$Tables <- NULL
        Export$Table_pics <- NULL
        
        #set up a general progress bar
        shiny::withProgress(message = lang$server(115), value = 0, {
        
        #Save Tables
          #set up a table progress bar
          shiny::withProgress(message = lang$server(116), value = 0, {
            purrr::map(Export$Table_prep, \(args) {
              if (!is.null(args))
                do.call("table_download", args = c(args))
            })
          })
        
          #setting the progress
          shiny::setProgress(
            length(Export$Tables)/Export$n_total, 
            detail = paste(lang$server(117), length(Export$n_tables))
            )
          
        #Export Plots
        purrr::map(Export$Plot, \(args) {           
          do.call("plot_download",
                  args = c(args, 
                           Export_files = Export_files))
        })
        
        #setting the progress
        shiny::setProgress(
          (Export$n_tables + Export$n_plots)/Export$n_total,
          detail = paste(lang$server(118), length(Export_files())))

        #add relevant table files to the list
        Export_files(c(Export_files(), Export$Tables))

        
        #Export Excel
        
        if (!purrr::every(Export$Xlsx, is.null)) {
          #Filename
          filename <-
            paste(Spectrum_Name, "_", Sys.Date(), ".xlsx", sep = "")
          
          #create a new workbook
          wb <- openxlsx::createWorkbook(Spectrum_Name)
          
          #add worksheets
          purrr::imap(Export$Xlsx, excel_sheet, wb = wb)
          # excel_sheet(wb, Export$Xlsx[[1]], "Radiometrie")
          
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
        
            #set a final progress
            shiny::setProgress(value = 1,
                               detail = paste(
                                 lang$server(119), length(Export$n_total))
                               )
            
        # give the files a sequential numbering
          file.rename(from = Export_files(), 
                      to = renaming(Export_files(),
                                    Spectrum_Name)
                      )
          
        # create the zip file for download
          utils::zip(file, renaming(Export_files(),
                                    Spectrum_Name)
                     )
        }
        )
        
        shinyalert::shinyalert(paste0("Download: ", Spectrum_Name), 
                   text = paste0(lang$server(114)), 
                   type = "success", 
                   timer = 5000, 
                   closeOnClickOutside = TRUE)
      })
    
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
                                   shinydashboard::dashboardSidebar(
                                     width = 190,
                                     shinydashboard::sidebarMenu(
                                       id = "inTabset",
                                       htmltools::br(),
                                       htmltools::p(
                                         htmltools::img(
                                           width = "135px", 
                                           src = "extr/Logo.png"),
                                         align = "center"),
                                       # htmltools::br(),
                                       shinydashboard::menuItem(
                                         lang$ui(1),
                                         tabName = "tutorial",
                                         icon = shiny::icon("circle-info")),
                                       htmltools::br(),
                                       shinydashboard::menuItem(
                                         "Import",
                                         tabName = "import",
                                         icon = shiny::icon("file-import")
                                       ),
                                       shinydashboard::menuItem(
                                         lang$ui(23),
                                         tabName = "analysis",
                                         icon = shiny::icon(
                                           "magnifying-glass-chart")
                                       ),
                                       shinydashboard::menuItem(
                                         "Export",
                                         tabName = "export",
                                         icon = shiny::icon("file-export"),
                                         selected = TRUE
                                       ), 
                                       htmltools::br(),
                                       shinydashboard::menuItem(
                                         lang$ui(153),
                                         tabName = "validity",
                                         icon = shiny::icon("file-circle-check")
                                       ),
                                       shinydashboard::menuItem(
                                         lang$ui(158),
                                         tabName = "impressum",
                                         icon = shiny::icon(
                                           "user", lib = "glyphicon")
                                       )
                                     )
                                   ),
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
                                     ),
                                     shiny::fluidPage(
                                       htmltools::p(shiny::plotOutput(
                                         "Plotbreite", height = "1px")))
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
                               Spectrum = Spectrum,
                               Tabactive = shiny::reactive(input$inTabset))
    
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

    
    output$Plotbreite <- shiny::renderPlot({
    }, bg = "white")
    
    
    output$Data_ok <- shiny::renderPrint({
      #     print("Developer Troubleshoot\n")
      # p990rint(list.files(path = "extr/"))
      # print(Export$CSV)
      # print(Analysis$table_Radiometrie$internal)
      # print(Export$Download_n)
      # print(Export$Download_tables)
      # print(Export$Table)
      # print(Export$Xlsx)
      # print(Export$Plot)
      # print(Spectrum$Origin)
      #     print(Spectrum$Spectrum %>% utils::head())
      #     print(Spectrum$Spectrum %>% utils::tail())
    })
    
  }
  shiny::shinyApp(ui, server, ...)
}
