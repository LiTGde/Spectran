
# UI ----------------------------------------------------------------------

export_downloadUI <- 
  function(
    id
    ) {
    ns <- shiny::NS(id)
    htmltools::tagList(
          #Download-Button
                  shinydashboard::box( class = "background-rect",
                    width = 12,
                    align = "center",
                    shiny::actionButton(ns("download_button"), 
                                          label = 
                                            htmltools::strong("Downloads"),
                                          class = "btn-lg"
                                          )
                  )
          )
    }

# Server ------------------------------------------------------------------

export_downloadServer <- 
  function(
    id, 
    Analysis,
    Export,
    Tabactive,
    outputs
  ) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
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
        htmltools::strong(paste0(" ", lang$server(136)," (n=%s)")),
        n = Export$n_total
        )
    })
    
    #Prepare the Files for Download
    shiny::observe({
      
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
        Export$dir <- getwd()
      }
      )
      
      shinyalert::shinyalert(paste0("Download: ", Spectrum_Name), 
                             text = shiny::tagList(
                               paste0(lang$server(114)),
                               htmltools::br(),
                               htmltools::br(),
                               htmltools::p(
                               shiny::downloadButton(session$ns("download"),
                                                     label = lang$server(109),
                                                     class = "btn-lg")
                               )), 
                             type = "success", 
                             html = TRUE,
                             # timer = 5000, 
                             closeOnClickOutside = TRUE,
                             showCancelButton = TRUE,
                             showConfirmButton = FALSE)
      
    }) %>% shiny::bindEvent(input$download_button)
    
    #Download-Button
    output$download <- shiny::downloadHandler(
      
      #Filename
      filename = function() {
        paste(
          Analysis$Settings$Spectrum_Name, "_", Sys.Date(), ".zip", sep="")},
      #Content
      content = function(file) {
        
        utils::zip(file, paste0(Export$dir, 
                                "/", 
                                renaming(Export_files(), 
                                         Analysis$Settings$Spectrum_Name)
                                ),
                   extras = '-j'
        )
      })
    
  })
}

# App ---------------------------------------------------------------------
