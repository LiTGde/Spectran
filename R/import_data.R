
# UI ----------------------------------------------------------------------

import_dataUI <- function(
    id) {
  htmltools::tagList(
    shinyFeedback::useShinyFeedback(),
    shiny::fluidRow(
    shinydashboard::box(
      #Introductory Text:
      #paragraph 1:
      htmltools::p(
        lang$ui(47),
        (htmltools::HTML(paste0(
          "<b>(W/m", htmltools::tags$sup("2"), "*nm)</b>"
        ))),
        lang$ui(48),
        htmltools::a(.noWS = "outside", 
          lang$ui(49), 
          href = paste0("extr/Beispiel.csv"), 
          target="_blank"),
        lang$ui(50),
        htmltools::em(.noWS = "outside", lang$ui(51)),
        lang$ui(52)
        ),
      #paragraph 2:
      htmltools::p(lang$ui(53),
                   htmltools::em(.noWS = "outside", lang$ui(54)),
        lang$ui(55),
        htmltools::em(.noWS = "outside", lang$ui(56)),
        "."
        ),
      #Upload section:
      shiny::fluidPage(
        #Upload individual file:
        shiny::column(width = 8,
                      shiny::fileInput(
                        shiny::NS(id,"in_file"),
                 lang$ui(57),
                 accept = c("csv", "comma-separated-values", ".csv"),
                 width = "100%",
                 placeholder = lang$ui(58)
                 )
               ),
        #Upload an example spectrum:
        shiny::column(width = 4, align = "center", 
                      shiny::verticalLayout(
                        shiny::actionButton(
                          shiny::NS(id,"jgtm"), 
                          htmltools::HTML(
                     paste0(
                       htmltools::img(
                         width = "100%", src = paste0("extr/", lang$ui(59)
                                                      ), 
                       align = "center")
                       )
                     ),
                   align = "center", width = "140px")
                 )
               )
        ),
      #Settings for the Import:
      shiny::column(width = 2, align = "center",
                    htmltools::br(),
               #Parameter settings for the CSV-import:
               shinyWidgets::dropdown(
                 import_csv_settingsUI(
                   shiny::NS(id, "import")),
                 #Dropdown-Settings
                 status = "danger",
                 up = TRUE,
                 icon = shiny::icon("gear"), 
                 width = "600px",
                 tooltip = shinyWidgets::tooltipOptions(title = lang$ui(46)),
                 animate = shinyWidgets::animateOptions(
                   enter = "fadeInLeft", exit = "fadeOutLeft", duration = 1
                 )
               )),
      # Setting a name of the spectrum:
      shiny::column(width = 10, 
                    shiny::textInput(shiny::NS(id,"name_id"), 
                       label = lang$ui(60), 
                       value = lang$ui(61)
                       )
             ),
      htmltools::br(),
      #Controlling how the spectrum looks:
      import_visual_checkUI(shiny::NS(id,"visual")),
      width = 12),
    htmltools::br(),
    htmltools::br(),
    #Importbutton:
    shiny::fluidRow(
      import_data_verifierUI(shiny::NS(id, "importbutn"), 
                        label = lang$ui(62),
                        icon = shiny::icon("play", lib = "glyphicon"),
                        class = "btn-lg"),
      #Adjusting the Spectrum:
      import_data_verifierUI(shiny::NS(id, "adjustbutn"), 
                        label = lang$ui(63),
                        icon = shiny::icon("wrench", lib = "glyphicon"),
                        class = "btn"),
      width = 12,
      align = "center"
    )
  )
  )
}

# Server ------------------------------------------------------------------

import_dataServer <- 
  function(id, 
           Spectrum = NULL
           ) {
  
    shiny::moduleServer(id, function(input, output, session) {
    
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        shiny::reactiveValues(
          Spectrum_raw = NULL, Name = NULL, Destination = NULL
          )
    }
    
    #Setting up a container for the import filepath:
    importfile <- shiny::reactiveVal()
    
    #Choosing the example spectrum:
    shiny::observe({
      importfile(paste0(shiny::resourcePaths()[["extr"]], "/Beispiel.csv"))
      Spectrum$Origin <- "File"
    }) %>% shiny::bindEvent(input$jgtm)
    
    #Uploading a file:
    shiny::observe({
      importfile(input$in_file$datapath)
      Spectrum$Origin <- "File"
    }) %>% shiny::bindEvent(input$in_file$datapath)
    
    #Setting a default name for the spectrum:
    spec_name <- shiny::reactiveVal(lang$ui(61))
    
    #Update the name of the light spectrum after choosing a new import file:
    shiny::observe({
      shiny::updateTextInput(
        session, "name_id", value = split_filename(input$in_file$name)
      )
    }) %>% shiny::bindEvent(input$in_file$datapath)
    
    shiny::observe({
      shiny::updateTextInput(
        session, "name_id", value = lang$ui(61)
      )
    }) %>% shiny::bindEvent(input$jgtm)
    
    #Import Data from a file:
    dat0 <- shiny::reactive({
      shiny::req(importfile(), csv_settings()$row_nr)
      temp <- 
        try(utils::read.csv(importfile(),
                     sep= csv_settings()$separator, 
                     dec = csv_settings()$decimal,
                     skip = csv_settings()$row_nr, 
                     header = csv_settings()$header
                     )
            )
      temp
    })
    
    dat <- shiny::reactiveVal()
    
    #Make adjustments to the Import when coming from a file:
    shiny::observe({
      input$jgtm
      dat({
        temp <- dat0()
        temp
        shiny::req(
        is.data.frame(dat0()), cancelOutput = TRUE
        )
      if(
        dplyr::between(csv_settings()$x_y2, 1, ncol(temp))
        ){
        if(shiny::isTruthy(
          is.numeric(temp[[csv_settings()$x_y2]]) &
          csv_settings()$multiplikator > 0)){
          #Multiplying irradiance with the multiplikator, if it is numeric
          temp[, csv_settings()$x_y2] <-
            temp[, csv_settings()$x_y2]*csv_settings()$multiplikator
          #Setting values < 0 to zero
          temp[(temp[[csv_settings()$x_y2]] < 0 & !is.na(temp[[2]])) , 
               csv_settings()$x_y2] <- 0
        }
      }
      temp
      })
    })
    
    #Make adjustments to the Import when coming from other sources
    shiny::observe({
      # shiny::req(dat())
      if(Spectrum$Destination == lang$ui(69) &
         Spectrum$Origin != "File"
         ) {
      dat(Spectrum$Spectrum)
      # importfile(Spectrum$Name)
      }
    }) %>% shiny::bindEvent(Spectrum$Spectrum, ignoreInit = TRUE)
    
    #Functions to set the csv-settings
    csv_settings <- 
      import_csv_settingsServer("import", dat0)
    
    #Preliminary visual representation of the import data
    import_visual_checkServer(
      "visual",
      dat,
      csv_settings,
      importfile,
      dat0
      )
    
    #Checks on the data when importing the file
    Data_ok <- import_data_checkServer(
      "check", 
      dat, 
      dat0,
      importfile,
      csv_settings
      )
    
    
    
    #Checks on the data when transfering the File for further analysis
    import_data_verifierServer("importbutn",
                          Data_ok = shiny::reactive(Data_ok$x),
                          dat = dat,
                          Spectrum = Spectrum,
                          csv_settings = csv_settings,
                          Name = shiny::reactive(input$name_id)
                          )
    
    #Checks on the data when transfering the File to adjustments
    import_data_verifierServer("adjustbutn",
                          Data_ok = shiny::reactive(Data_ok$x),
                          dat = dat,
                          Spectrum = Spectrum,
                          csv_settings = csv_settings,
                          Name = shiny::reactive(input$name_id),
                          Destination = lang$ui(94)
                          )
    
    #Set the name of the Spectrum depending on the global Name
    shiny::observe({
      shiny::updateTextInput(session, "name_id", value = Spectrum$Name)
    })
    
    shiny::reactive(dat())
  })
}

# App ---------------------------------------------------------------------

import_dataApp <- function(lang_setting = "Deutsch") {
  
  #add a resource path to the www folder
  shiny::addResourcePath(
    "extr", system.file("app/www", package = "Spectran"))
  # on.exit(shiny::removeResourcePath("extr"), add = TRUE)
  
  #set the language for the program
  the$language <- lang_setting
  
  ui <- shiny::fluidPage(
       import_dataUI("import"),
       shiny::verbatimTextOutput("Data_ok")
      )
  server <- function(input, output, session) {

    import <- import_dataServer("import")
    output$Data_ok <- shiny::renderPrint({
      {
        print(import())
        print(Spectrum$Destination)
        Spectrum$Spectrum_raw %>% utils::head()
      }
    })
    }
  shiny::shinyApp(ui, server)
}
