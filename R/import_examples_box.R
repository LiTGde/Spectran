
# UI ----------------------------------------------------------------------

#deciding what to put into the box
box_filling <- 
  function(
    id, 
    filling, 
    lang_setting = get("lang_setting", envir = caller_env(n = 1)),
    title = NULL) {
    ns <- NS(id)
    #Give an empty frame if it is NULL or filling[[2]] is NA
    if (any(is.null(filling), is.na(filling[[2]]))) {}
    else {
      switch (filling[[1]],
              #paste 1(!) image, with the filename in filling[[2]]
              image = 
                img(
                  width = "100%", 
                  src = 
                    paste0(
                      "Examples/", lang_setting, "/", filling[[2]], ".png"
                      ), 
                  align = "center"
                  ),
              #paste 1(!) video, with the video URL in filling[[2]]
              video =
                tags$video(
                  src = filling[[2]],
                  width = "100%",
                  type = "video/mp4",
                  controls = "controls"
                  ),
              #paste as many Download buttons as there are in the named 
              #character vector of filling[[2]].
              download = {
                uiOutput(ns("download"))
                },
              #if nothing fits, then filling[[2]] is just put in - it needs to
              #be a list around tagList, if there is more than one element
              filling[[2]]
              )
    }
  }

import_examples_boxUI <-
  function(id, 
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           title = "",
           left = NULL,
           mid = NULL,
           right = NULL) {
    ns <- NS(id)
    #creating an empty box hull, where the user decides what goes into the parts
    tagList(
      box(
        width = 12,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE,
        title = title,
        column(
          width = 4,
          align = "center",
          box_filling(id, left)
        ),
        column(
          width = 4, 
          align = "center",
          box_filling(id, mid)
        ),
        column(
          align = "center",
          width = 4,
          box_filling(id, right, title = title)
        )
      )
    )
  }

# Server ------------------------------------------------------------------

import_examples_boxServer <-
  function(id,
           lang_setting = get("lang_setting", envir = caller_env(n = 1)),
           Spectrum = NULL,
           examplespectra = NULL,
           examplespectra_descriptor = NULL,
           illu_eigen,
           down_import,
           daylight_CCT = NULL
           ) {
    stopifnot(!(examplespectra_descriptor %>% is.reactive()))
    stopifnot(!(examplespectra %>% is.reactive()))
    stopifnot(illu_eigen %>% is.reactive())
    stopifnot(down_import %>% is.reactive())
    
    moduleServer(id, function(input, output, session) {

      #Set up a container for the spectra to go into, if it isn´t already
      #defined
      if (is.null(Spectrum)){
        Spectrum <- 
          reactiveValues(Spectrum_raw = NULL, Name = NULL, Destination = NULL)
      }
      
      #Data Preparation for the dynamic UI
      filling <- c("download", examplespectra_descriptor[["download"]])
      
      #Render a Downloadbutton or an Importbutton
      selector <- function(func) {
        output$download <- renderUI({
          ns <- session$ns
          if(length(filling[[2]]) <= 3) {
            pmap(
              list(
                paste0(
                  ns(filling[[2]])
                ),
                names(filling[[2]]),
                class = "butt"
              ),
              func
            ) %>% map(p)
          }
          else {
            tagList(
              pickerInput(
                inputId = ns("choose_from_many"),
                label = lang$ui(170),
                choices = filling[[2]],
                selected = isolate(input$choose_from_many)
              ),
              func(ns("save_from_many"),
                   label = 
                     inverse_lookup(filling[[2]])[input$choose_from_many],
                   class = "butt")
            )
          }
      })
      }

      #are CIE spectra used?
      CIE <-
        ifelse(
          examplespectra_descriptor[["embargo"]], "CIE", "Measurement"
        )
      
      #Apply the correct function for generating the buttons
      observe({
        # switch(down_import(),
        #      Import = selector(actionButton),
        #      Download = selector(downloadButton))
        switch(NS(down_import(), CIE),
             'Download-Measurement' = selector(downloadButton),
             Download = selector(downloadButton),
             selector(actionButton))
        })
 
            
      #Function that creates a dataset ready for import or Download
      dataset_creator <- function(name) {

        Data <- 
          examplespectra[[CIE]] %>%
          select(Wellenlaenge, .env$name)  %>% mutate(
            Wellenlaenge = as.integer(Wellenlaenge)
          )
        Data[[2]] <- Data[[2]]*illu_eigen()
        names(Data) <- c(lang$server(31), lang$server(32))
        Data
      }

      #Function that creates a shinyAlert when trying to Download a CIE Spectrum
      CIE_download <- function(name){
        shinyalert(
          lang$server(134),
          tagList(p(lang$server(133)),br(),
                  downloadButton(session$ns(name), class = "btn-lg")),
          type = "info",
          timer = 10000,
          html = TRUE,
          showCancelButton = TRUE,
          # cancelButtonCol = "#AEDEF4",
          showConfirmButton = FALSE) 
      }
      
      #function that handles downloads from individual buttons
      save_from_one <- function() {

        #Ready the Dataframe by expanding
        dataframe <- examplespectra_descriptor %>% unnest(
          c(Identifier, Button_Name, Dateinamen))
        
        #feeding the expanded Dataframe into a function that creats an observer
        #the observer decides what to do with a button input depending on 
        #whether it is a CIE Spectrum and Download or Imput are chosen.
        pmap(list(
          name = dataframe[["Identifier"]],
          filename = dataframe[["Dateinamen"]],
          Button_Name = dataframe[["Button_Name"]]),
          \(name, filename, Button_Name) {
            
            observe({
              #CIE Download:
              if(all(down_import() == "Download",
                     examplespectra_descriptor$embargo)) {
                CIE_download(name)
              }
              #All Import:
              else {
                Spectrum$Spectrum_raw <- dataset_creator(name)
                Spectrum$Name <- {
                  paste0(examplespectra_descriptor$Beschreibung,
                         ": ", Button_Name)
                }
                Spectrum$Destination <- lang$ui(69)
              }
            }) %>% bindEvent(input[[name]], ignoreInit = TRUE)
            #All Download
            output[[name]] <- downloadHandler(
              filename = paste0(filename, ".csv"),
              content = function(file) {
                write_csv(dataset_creator(name), file)
              }
            )
          })
      }
      
      #function that handles downloads from a selector
      save_from_many <- function() {

        #Ready the Dataframe by expanding
        dataframe <- examplespectra_descriptor %>% unnest(
          c(Identifier, Button_Name, Dateinamen))
        
        observe({
          #CIE Download:
          if(all(down_import() == "Download",
                 examplespectra_descriptor$embargo)) {
            CIE_download("save_from_many")
          }
          #All Import:
          else {
          Spectrum$Spectrum_raw <- dataset_creator(input$choose_from_many)
          Spectrum$Name <- 
            dataframe %>% 
            filter(Identifier == input$choose_from_many) %>%
            pull(Button_Name) %>% 
            {paste0(examplespectra_descriptor$Beschreibung, ": ", .)}
          Spectrum$Destination <- lang$ui(69)
          }
        }) %>% bindEvent(input[["save_from_many"]])
        
          output$save_from_many <- downloadHandler(
            filename = function() {
              paste0(dataframe %>% 
                       filter(Identifier == input$choose_from_many) %>%
                       pull(Dateinamen), ".csv")
              },
            content = function(file) {
              write_csv(dataset_creator(input$choose_from_many), file)
              }
          )
        }
      
      #choose which function is appropriate
      #Daylight:
      if(id == "norm") {
        # Creating the Daylight Spectrum:
        daylight_spectrum <- reactive({
          Bspdat <- tibble(
            Wellenlaenge = 380:780,
            Bestrahlungsstaerke =
              daylightSpectra(daylight_CCT(), wavelength = Wellenlaenge)
            ) %>%
            mutate(Bestrahlungsstaerke =
                     unclass(Bestrahlungsstaerke) /
                     Calc_lux(
                       Bestrahlungsstaerke,
                       Specs$AS_wide,
                       Specs$Efficacy)*illu_eigen())
          names(Bspdat) <- c(lang$server(31), lang$server(32))
          Bspdat
        })

        # Daylight Illuminant Download_Name:
        daylight_name <- reactive({
          paste0(lang$server(33), daylight_CCT(), "K.csv")
        })
        
        #Download for Daylight:
        observe({
        if(down_import() == "Download") {
        output$norm <- downloadHandler(
          filename = daylight_name,
          content = function(file) {
            write_csv(daylight_spectrum(), file)
          }    )
        }
          #Import for Daylight:
        else if (down_import() == "Import"){
          observe({
            Spectrum$Spectrum_raw <- daylight_spectrum()
            Spectrum$Name <-  paste0(lang$ui(71), " ",daylight_CCT(), "K")
            Spectrum$Destination <- lang$ui(69)
          }) %>% bindEvent(input$norm, ignoreInit = TRUE)
        }
        })
      }
      #Handling when there are many Spectra (Selector)
      else if(length(examplespectra_descriptor[["Identifier"]][[1]]) > 3) {
        save_from_many()
      }
      #Handling when there is one button per Spectrum
      else {
        save_from_one()
          }
    })
    }

# App ---------------------------------------------------------------------
