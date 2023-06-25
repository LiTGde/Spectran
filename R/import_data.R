
# UI ----------------------------------------------------------------------

import_dataUI <- function(
    id, lang_setting = get("lang_setting", envir = caller_env(n = 1))) {
  tagList(
    useShinyFeedback(),
    br(),
    box(
      #Introductory Text:
      #paragraph 1:
      p(
        lang$ui(47),
        (HTML(paste0(
          "(W/m", tags$sup("2"), "*nm)"
        ))),
        lang$ui(48),
        a(.noWS = "outside", 
          lang$ui(49), 
          href = "Beispiel.csv", 
          target="_blank"),
        lang$ui(50),
        em(.noWS = "outside", lang$ui(51)),
        lang$ui(52)
        ),
      #paragraph 2:
      p(lang$ui(53),
        em(.noWS = "outside", lang$ui(54)),
        lang$ui(55),
        em(.noWS = "outside", lang$ui(56)),
        "."
        ),
      #Upload section:
      fluidPage(
        #Upload individual file:
        column(width = 8,
               fileInput(
                 NS(id,"in_file"),
                 lang$ui(57),
                 accept = c("csv", "comma-separated-values", ".csv"),
                 width = "100%",
                 placeholder = lang$ui(58)
                 )
               ),
        #Upload an example spectrum:
        column(width = 4, align = "center", 
               verticalLayout(
                 actionButton(
                   NS(id,"jgtm"), 
                   HTML(
                     paste0(
                       img(width = "100%", src = lang$ui(59), 
                       align = "center")
                       )
                     ),
                   align = "center", width = "140px")
                 )
               )
        ),
      #Settings for the Import:
        column(width = 2, align = "center",
               br(),
               #Parameter settings for the CSV-import:
               dropdown(
                 import_csv_settingsUI(
                   NS(id, "import"), lang_setting = lang_setting),
                 #Dropdown-Settings
                 status = "danger",
                 up = TRUE,
                 icon = icon("gear"), 
                 width = "600px",
                 tooltip = tooltipOptions(title = lang$ui(46)),
                 animate = animateOptions(
                   enter = "fadeInLeft", exit = "fadeOutLeft", duration = 1
                 )
               )),
      # Setting a name of the spectrum:
      column(width = 10, 
             textInput(NS(id,"name_id"), 
                       label = lang$ui(60), 
                       value = lang$ui(61)
                       )
             ),
      br(),
      #Controlling how the spectrum looks:
      import_visual_checkUI(NS(id,"visual")),
      width = 12),
    br(),
    br(),
    #Importbutton:
    fluidRow(
      import_data_verifierUI(NS(id, "importbutn"), 
                        label = lang$ui(62),
                        icon = icon("play", lib = "glyphicon"),
                        class = "btn-lg"),
      #Adjusting the Spectrum:
      import_data_verifierUI(NS(id, "adjustbutn"), 
                        label = lang$ui(63),
                        icon = icon("wrench", lib = "glyphicon"),
                        class = "btn"),
      width = 12,
      align = "center"
    )
  )
}

# Server ------------------------------------------------------------------

import_dataServer <- 
  function(id, 
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           Spectrum = NULL
           ) {
  
  moduleServer(id, function(input, output, session) {
    
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        reactiveValues(Spectrum_raw = NULL, Name = NULL, Destination = NULL)
    }
    
    #Setting up a container for the import filepath:
    importfile <- reactiveVal()
    
    #Choosing the example spectrum:
    observe({
      importfile("www/Beispiel.csv")
    }) %>% bindEvent(input$jgtm)
    
    #Uploading a file:
    observe({
      importfile(input$in_file$datapath)
    }) %>% bindEvent(input$in_file$datapath)
    
    #Setting a default name for the spectrum:
    spec_name <- reactiveVal(lang$ui(61))
    
    #Update the name of the light spectrum after choosing a new import file:
    observe({
      updateTextInput(
        session, "name_id", value = split_filename(input$in_file$name)
      )
    }) %>% bindEvent(input$in_file$datapath)
    
    #Import Data from a file:
    dat0 <- reactive({
      req(importfile(), csv_settings()$row_nr)
      temp <- 
        try(read.csv(importfile(),
                     sep= csv_settings()$separator, 
                     dec = csv_settings()$decimal,
                     skip = csv_settings()$row_nr, 
                     header = csv_settings()$header
                     ),
            silent = TRUE
            )
      temp
    })
    
    dat <- reactiveVal()
    
    #Make adjustments to the Import when coming from a file:
    observe({
      dat({
      req(
        dat0(), cancelOutput = TRUE
      )
      temp <- dat0()
      if(between(csv_settings()$x_y2, 1, ncol(temp))){
        if(isTruthy(
          is.numeric(temp[[csv_settings()$x_y2]]) & 
          csv_settings()$multiplikator > 0)){
          #Multiplying irradiance with the multiplikator, if it is numeric
          temp[, csv_settings()$x_y2] <- 
            temp[, csv_settings()$x_y2]*csv_settings()$multiplikator
          #Setting values < 0 to zero
          temp[temp[[csv_settings()$x_y2]] < 0 , csv_settings()$x_y2] <- 0
        }
      }
      temp
      })
    })
    
    #Make adjustments to the Import when coming from other sources
    observe({
      dat(Spectrum$Spectrum)
      importfile(Spectrum$Name)
    }) %>% bindEvent(Spectrum$Spectrum)
    
    #Functions to set the csv-settings
    csv_settings <- 
      import_csv_settingsServer("import", lang_setting = lang_setting, dat0)
    
    #Preliminary visual representation of the import data
    import_visual_checkServer(
      "visual",
      lang_setting = lang_setting,
      dat,
      csv_settings
      )
    
    #Checks on the data when importing the file
    Data_ok <- import_data_checkServer(
      "check", 
      lang_setting = lang_setting, 
      dat, 
      dat0,
      importfile,
      csv_settings
      )

    #Checks on the data when transfering the File for further analysis
    import_data_verifierServer("importbutn",
                          lang_setting = lang_setting,
                          Data_ok = reactive(Data_ok$x),
                          dat = dat,
                          Spectrum = Spectrum,
                          csv_settings = csv_settings,
                          Name = reactive(input$name_id))
    
    #Checks on the data when transfering the File to adjustments
    import_data_verifierServer("adjustbutn",
                          lang_setting = lang_setting,
                          Data_ok = reactive(Data_ok$x),
                          dat = dat,
                          Spectrum = Spectrum,
                          csv_settings = csv_settings,
                          Name = reactive(input$name_id),
                          Destination = lang$ui(94))
    
    #Set the name of the Spectrum depending on the global Name
    observe({
      updateTextInput(session, "name_id", value = Spectrum$Name)
    })
    
  })
}

# App ---------------------------------------------------------------------

import_dataApp <- function(lang_setting = "Deutsch") {
  
  ui <- fluidPage(
       import_dataUI("import"),
      verbatimTextOutput("Data_ok")
      )
  server <- function(input, output, session) {

    import_dataServer("import")
    output$Data_ok <- renderPrint({
      {
        print(Spectrum$Name)
        print(Spectrum$Destination)
        Spectrum$Spectrum_raw %>% head()
      }
    })
    }
  shinyApp(ui, server)
}
