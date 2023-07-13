
# UI ----------------------------------------------------------------------

#UI for all the Settings (put in a dropdown)
Settings_WidgetUI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
  htmltools::h4(lang$ui(109)),
  shiny::column(width = 6,
  shiny::checkboxInput(
    ns("Alter_mel"),
    label = htmltools::strong(lang$ui(110)),
    width = "100%",
    value = TRUE
  ),
  shiny::checkboxInput(
    ns("Hintergrund"),
    label = lang$ui(169),
    value = TRUE,
    width = "100%"
  ),
  shiny::sliderInput(
    ns("plot_multiplier"),
    label = lang$ui(111),
    min = 1,
    value = 1,
    max = 2,
    step = 0.1,
    width = "100%"
  )),
  shiny::column(width = 6,
  htmltools::strong(lang$ui(112)),
  shiny::checkboxInput(
    ns("Alter_inset"),
    label = htmltools::strong(lang$ui(113)),
    width = "100%",
    value = TRUE
  ),
  shiny::checkboxInput(
    ns("Alter_rel"),
    label = htmltools::strong(lang$ui(114)),
    width = "100%"
  )))
}


analysis_ageUI <- function(
    id) {

  ns <- shiny::NS(id)
  
  #main UI
  htmltools::tagList(
    #Inputs
    shiny::fluidRow(
      shinydashboard::box( width = 12,
      #Heading and explainer
      shinydashboard::box(
        width = 12,
        title = lang$ui(101),
        lang$ui(102),
        htmltools::strong(.noWS = "outside", lang$ui(103)),
        lang$ui(104),
        htmltools::strong(.noWS = "outside",
                          lang$ui(105)),
        lang$ui(106),
        htmltools::a(
          .noWS = "outside",
          "DIN/TS 5031-100:2021-11 ",
          href =
            URL_DIN,
          target = "_blank"
        ),
        lang$ui(107),
        htmltools::a(
          .noWS = "outside",
          "CIE S026 (2018)",
          href = URL_CIE,
          target = "_blank"
        ),
        lang$ui(108),
        collapsible = TRUE,
        solidHeader = FALSE,
        collapsed = TRUE
      ),
    #Settings for Age
    shiny::fluidPage(
      align = "center",
      shiny::column(
        width = 2,
        htmltools::br(),
        shinyWidgets::dropdown(
          Settings_WidgetUI(id),
          #Dropdown-Settings
          status = "danger",
          up = FALSE,
          icon = shiny::icon("gear"), 
          width = "600px",
          animate = shinyWidgets::animateOptions(
            enter = "fadeInLeft", exit = "fadeOutLeft", duration = 1
          ),
          tooltip = shinyWidgets::tooltipOptions(title = lang$ui(115))
        )
      ),
      #Age setting
      shiny::column(
        width = 10,
        shiny::sliderInput(
          ns("Alter"),
          label = lang$ui(116),
          min = 0,
          max = 100,
          value = 50,
          step = 1,
        width = "100%")
      )
    ), 
    #Tabsets with the age plots
    shiny::tabsetPanel(
      shiny::tabPanel( title = "Transmission",
        analysis_age2UI(id = ns(lang$server(128)), "450px")
      ),
      shiny::tabPanel( title = lang$ui(117),
                       analysis_age2UI(id = ns(lang$server(127)), "350px")
      ),
      shiny::tabPanel( title = lang$ui(118),
                       analysis_age2UI(id = ns(lang$server(125)), "450px")
      ),
      selected = lang$ui(118)
    ),

  ))    )
}

analysis_age2UI <- function(
    id, 
    plotheight) {
  
  ns <- shiny::NS(id)
  #Outputs
  htmltools::tagList(
    shiny::plotOutput(ns("plot"), height = plotheight),
    gt::gt_output(ns("table"))
  )
  
}

# Server ------------------------------------------------------------------

analysis_ageServer <- 
  function(id, 
           Analysis,
           Tabactive
  ) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #checking the sensitivity box, when export demands it
      shiny::observe({
        shiny::updateCheckboxInput(
          session, "Alter_mel", value = Analysis$action_spectra)
      }) %>% shiny::bindEvent(Analysis$action_spectra)
      
      #Set the Age as a global Variable
      shiny::observe({
        Analysis$Age <- input$Alter
      }) %>% shiny::bindEvent(input$Alter)
      
      #Set a new age coming from export
      shiny::observe({
        if(Analysis$Age != input$Alter) {
          shiny::updateSliderInput(session, "Alter", value = Analysis$Age)
        }
      }) %>% shiny::bindEvent(Analysis$Age)
      
      # create the module server for calculation
      analysis_age_graphicgeneratorServer(
        "generator",
        Analysis,
        shiny::reactive(input$Alter_mel),
        shiny::reactive(input$Hintergrund),
        shiny::reactive(input$plot_multiplier),
        shiny::reactive(input$Alter_inset),
        shiny::reactive(input$Alter_rel),
        shiny::reactive(input$Alter)
          )
      
      #create the module servers for outputs
      purrr::map2(c(lang$server(128), lang$server(127), lang$server(125)), 
                  c(450, 350, 450), 
                  \(i,h) {
      analysis_age2Server(
        id = i,
        Analysis = Analysis,
        feed = i,
        plotheight = h,
        Tabactive = Tabactive
      )
      })
    })
  }

analysis_age2Server <- 
  function(id, 
           Analysis,
           feed,
           plotheight,
           Tabactive
  ) {
    
    shiny::moduleServer(id, function(input, output, session) {
      
      #Plot generation
      output$plot <- shiny::renderPlot({
        shiny::req(Analysis[[ns_plot(feed)]])
        do.call(Analysis[[ns_plot(feed)]]$fun, Analysis[[ns_plot(feed)]]$args)

      } ,height = plotheight,
      width = \() {session$clientData$output_Plotbreite_width}
      )
      shiny::observe({
        shiny::outputOptions(
          output, 
          "plot", 
          suspendWhenHidden = if(Tabactive() == "analysis") FALSE else TRUE
        )
      })
      
      # Table (Output for Radiometry)
      output$table <- gt::render_gt({
        shiny::req(Analysis[[ns_table(feed)]])

        do.call(Analysis[[ns_table(feed)]]$fun,
                Analysis[[ns_table(feed)]]$output)
        
      })
      shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)
      
    })
  }

# App ---------------------------------------------------------------------
