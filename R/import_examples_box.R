
# UI ----------------------------------------------------------------------

#deciding what to put into the box
box_filling <- 
  function(
    id, 
    filling, 
    title = NULL) {
    ns <- shiny::NS(id)
    #Give an empty frame if it is NULL or filling[[2]] is NA
    if (any(is.null(filling), is.na(filling[[2]]))) {}
    else {
      switch (filling[[1]],
              #paste 1(!) image, with the filename in filling[[2]]
              image = 
                htmltools::img(
                  width = "100%", 
                  src = 
                    paste0(
                      "Examples/", the$language, "/", filling[[2]], ".png"
                      ), 
                  align = "center"
                  ),
              #paste 1(!) video, with the video URL in filling[[2]]
              video =
                htmltools::tags$video(
                  src = filling[[2]],
                  width = "100%",
                  type = "video/mp4",
                  controls = "controls"
                  ),
              #paste as many Download buttons as there are in the named 
              #character vector of filling[[2]].
              download = {
                shiny::uiOutput(ns("download"))
                },
              #if a plot is to be put in, the spectrum should be shown
              plot = shiny::plotOutput(ns("plot"), height = "150px"),
              #if nothing fits, then filling[[2]] is just put in - it needs to
              #be a list around tagList, if there is more than one element
              filling[[2]]
              )
    }
  }

import_examples_boxUI <-
  function(id, 
           title = "",
           left = NULL,
           mid = NULL,
           right = NULL) {
    ns <- shiny::NS(id)
    #creating an empty box hull, where the user decides what goes into the parts
    htmltools::tagList(
      shinydashboard::box(
        width = 12,
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE,
        title = title,
        shiny::column(
          width = 4,
          align = "center",
          box_filling(id, left)
        ),
        shiny::column(
          width = 4, 
          align = "center",
          box_filling(id, mid)
        ),
        shiny::column(
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
           Spectrum = NULL,
           examplespectra = NULL,
           examplespectra_descriptor = NULL,
           illu_eigen,
           down_import,
           daylight_CCT = NULL
           ) {
    stopifnot(!(examplespectra_descriptor %>% shiny::is.reactive()))
    stopifnot(!(examplespectra %>% shiny::is.reactive()))
    stopifnot(illu_eigen %>% shiny::is.reactive())
    stopifnot(down_import %>% shiny::is.reactive())
    
    shiny::moduleServer(id, function(input, output, session) {

      #Set up a container for the spectra to go into, if it isn´t already
      #defined
      if (is.null(Spectrum)){
        Spectrum <- 
          shiny::reactiveValues(
            Spectrum_raw = NULL, Name = NULL, Destination = NULL
            )
      }
      
      #Data Preparation for the dynamic UI
      filling <- c("download", examplespectra_descriptor[["download"]])
      
      #Render a Downloadbutton or an Importbutton
      selector <- function(func) {
        output$download <- shiny::renderUI({
          ns <- session$ns
          if(length(filling[[2]]) <= 1) {
            purrr::pmap(
              list(
                paste0(
                  ns(filling[[2]])
                ),
                names(filling[[2]]),
                class = "butt"
              ),
              func
            ) %>% purrr::map(shiny::p)
          }
          else {
            htmltools::tagList(
              shinyWidgets::pickerInput(
                inputId = ns("choose_from_many"),
                label = lang$ui(170),
                choices = filling[[2]],
                selected = shiny::isolate(input$choose_from_many)
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
      shiny::observe({
        switch(shiny::NS(down_import(), CIE),
             'Download-Measurement' = selector(shiny::downloadButton),
             Download = selector(shiny::downloadButton),
             selector(shiny::actionButton))
        })
 
            
      #Function that creates a dataset ready for import or Download
      dataset_creator <- function(name) {

        Data <- 
          examplespectra[[CIE]] %>%
          dplyr::select(Wellenlaenge, .env$name)  %>% 
          dplyr::mutate(
            Wellenlaenge = as.integer(Wellenlaenge)
          )
        Data[[2]] <- Data[[2]]*illu_eigen()
        names(Data) <- c(lang$server(31), lang$server(32))
        Data
      }

      #Plotoutput in the middle
      output$plot <- shiny::renderPlot({
        
      if(id == "norm") {
        shiny::req(daylight_CCT() %>% dplyr::between(4000, 25000))
           data <- daylight_spectrum()
       }
      else {
        data <-  {if(length(filling[[2]]) <= 1) {
          filling[[2]][[1]]
        }
        else shiny::req(input$choose_from_many)
        }  %>% dataset_creator()
      }
       data <- data %>% stats::setNames(nm = c("x","y")) %>% 
         dplyr::filter(x %>% dplyr::between(380,780))
       
        shiny::req(data)
        
        ggplot2::ggplot(
          data = data,
          ggplot2::aes(x= x))+
          ggridges::geom_ridgeline_gradient(
            ggplot2::aes(
            y = 0,
            height = y,
            fill = x
          ), col = NA)+
          ggplot2::scale_fill_gradientn(
            colors = ColorP[[the$palette]], guide = "none")+
          cowplot::theme_cowplot()+
          ggplot2::scale_x_continuous(breaks = c(400, 500, 600, 700, 780)) +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, .1)))+
          ggplot2::labs(x=NULL, y=NULL)
      })

      #Function that creates a shinyAlert when trying to Download a CIE Spectrum
      CIE_download <- function(name){
        shinyalert::shinyalert(
          lang$server(134),
          htmltools::tagList(htmltools::p(lang$server(133)),htmltools::br(),
                             shiny::downloadButton(
                               session$ns(name), class = "btn-lg")
                             ),
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
        dataframe <- examplespectra_descriptor %>% tidyr::unnest(
          c(Identifier, Button_Name, Dateinamen))
        
        #feeding the expanded Dataframe into a function that creats an observer
        #the observer decides what to do with a button input depending on 
        #whether it is a CIE Spectrum and Download or Imput are chosen.
        purrr::pmap(list(
          name = dataframe[["Identifier"]],
          filename = dataframe[["Dateinamen"]],
          Button_Name = dataframe[["Button_Name"]]),
          \(name, filename, Button_Name) {
            
            shiny::observe({
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
                Spectrum$Origin <- "Example"
              }
            }) %>% shiny::bindEvent(input[[name]], ignoreInit = TRUE)
            #All Download
            output[[name]] <- shiny::downloadHandler(
              filename = paste0(filename, ".csv"),
              content = function(file) {
                readr::write_csv(dataset_creator(name), file)
              }
            )
          })
      }
      
      #function that handles downloads from a selector
      save_from_many <- function() {

        #Ready the Dataframe by expanding
        dataframe <- examplespectra_descriptor %>% tidyr::unnest(
          c(Identifier, Button_Name, Dateinamen))
        
        shiny::observe({
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
            dplyr::filter(Identifier == input$choose_from_many) %>%
            dplyr::pull(Button_Name) %>% 
            {paste0(examplespectra_descriptor$Beschreibung, ": ", .)}
          Spectrum$Destination <- lang$ui(69)
          Spectrum$Origin <- if(examplespectra_descriptor$embargo) "CIE"
            else "Example"
          }
        }) %>% shiny::bindEvent(input[["save_from_many"]])
        
          output$save_from_many <- shiny::downloadHandler(
            filename = function() {
              paste0(dataframe %>% 
                       dplyr::filter(Identifier == input$choose_from_many) %>%
                       dplyr::pull(Dateinamen), ".csv")
              },
            content = function(file) {
              readr::write_csv(dataset_creator(input$choose_from_many), file)
              }
          )
        }
      
      #choose which function is appropriate
      #Daylight:
      if(id == "norm") {
        # Creating the Daylight Spectrum:
        daylight_spectrum <- shiny::reactive({
          Bspdat <- tibble::tibble(
            Wellenlaenge = 380:780,
            Bestrahlungsstaerke =
              colorSpec::daylightSpectra(
                daylight_CCT(), wavelength = Wellenlaenge)
            ) %>%
            dplyr::mutate(Bestrahlungsstaerke =
                     unclass(Bestrahlungsstaerke) /
                     Calc_lux(
                       Bestrahlungsstaerke,
                       Specs$AS_wide,
                       Specs$Efficacy)*illu_eigen())
          names(Bspdat) <- c(lang$server(31), lang$server(32))
          Bspdat
        })

        # Daylight Illuminant Download_Name:
        daylight_name <- shiny::reactive({
          paste0(lang$server(33), daylight_CCT(), "K.csv")
        })
        
        #Download for Daylight:
        shiny::observe({
        if(down_import() == "Download") {
        output$norm <- shiny::downloadHandler(
          filename = daylight_name,
          content = function(file) {
            readr::write_csv(daylight_spectrum(), file)
          }    )
        }
          #Import for Daylight:
        else if (down_import() == "Import"){
          shiny::observe({
            Spectrum$Spectrum_raw <- daylight_spectrum()
            Spectrum$Name <-  paste0(lang$ui(71), " ",daylight_CCT(), "K")
            Spectrum$Destination <- lang$ui(69)
            Spectrum$Origin <- "Example"
          }) %>% shiny::bindEvent(input$norm, ignoreInit = TRUE)
        }
        })
      }
      #Handling when there are many Spectra (Selector)
      else if(length(examplespectra_descriptor[["Identifier"]][[1]]) > 1) {
        save_from_many()
      }
      #Handling when there is one button per Spectrum
      else {
        save_from_one()
          }
    })
    }

# App ---------------------------------------------------------------------
