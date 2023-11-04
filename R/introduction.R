
# UI ----------------------------------------------------------------------

introductionUI <- function(
    id
    ) {
  
  ns <- shiny::NS(id)
  
  #getting the images from the www/ folder
  images <- image_gallery()
  
  htmltools::tagList(
    #Title, Image, and skip-intro-button
    shiny::fluidRow(align = "center",
             
             (htmltools::h1("LiTG ", 
                            htmltools::strong("SPECTRAN"), 
                            class = "h1")
              ),
             htmltools::img(
               width = "500px", 
               src = "extr/Frontbild.png"
               ),
             htmltools::br(),
             htmltools::br(),
             shiny::actionButton(
               ns("zu_Import1"),
               lang$ui(9),
               class = "btn-lg",
               shiny::icon("play", lib = "glyphicon")
               )
             ),
    #Header for the intro, then container for the intro
    htmltools::h3(lang$ui(10)),
    shiny::fluidRow(
      shinydashboard::box(
        #Text introduction
        htmltools::p(
          lang$ui(11),
          htmltools::strong(.noWS = "outside", "Spectran. "),
          lang$ui(12), 
          htmltools::strong(lang$ui(13))
          ),
        htmltools::p(
          lang$ui(14), 
          htmltools::a(
            .noWS = "outside", 
            "CIE S026 Toolbox", 
            href = "https://files.cie.co.at/CIE%20S%20026%20alpha-opic%20Toolbox%20User%20Guide.pdf", 
            target="_blank"
            ), 
          lang$ui(15),
          htmltools::a(
            .noWS = "outside", 
            "luox", 
            href = "https://luox.app", 
            target="_blank"
            ), 
          lang$ui(16)
          ),
        htmltools::p(lang$ui(17)),
        #First box with import possibilities
        shinydashboard::box(
          title = "Import",
          width = 12,
          status = "primary",
          shiny::column(width = 5,
                          htmltools:: p(lang$ui(18)),
                          htmltools::p(lang$ui(19)),
                          htmltools::p(lang$ui(20)),
                          htmltools::p(lang$ui(21)),
                          htmltools::p(lang$ui(22))
                          ),
          shiny::column(width = 7,
                        spsComps::gallery(
                          images$images[1:3], 
                          images$image_path[1:3], 
                          images$image_path[1:3], 
                          title = NULL, 
                          enlarge = TRUE, 
                          enlarge_method = "modal")
                        )
          ),
        #Second Box with analysis possibilities
        shinydashboard::box(
          title = lang$ui(23),
          width = 12,
          status = "primary",
          shiny::column(width = 5,
                          htmltools::p(lang$ui(24)),
                          htmltools::p(lang$ui(25)),
                          htmltools::p(lang$ui(26)),
                          htmltools::p(lang$ui(27)),
                          htmltools::p(lang$ui(28))),
          shiny::column(width = 7,
                        spsComps::gallery(
                          images$images[4:6], 
                          images$image_path[4:6], 
                          images$image_path[4:6], 
                          title = NULL, 
                          enlarge = TRUE, 
                          enlarge_method = "modal")
                        )
          ),
        #Third Box with export capabilities
        shinydashboard::box(
          title = "Export",
          width = 12,
          status = "primary",
            shiny::column(width = 5,
                          htmltools::p(lang$ui(29)),
                          htmltools::p(lang$ui(30)),
                          htmltools::p(lang$ui(31)),
                          htmltools::p(lang$ui(32))),
            shiny::column(width = 7,
                          spsComps::gallery(
                            images$images[7:9], 
                            images$image_path[7:9], 
                            images$image_path[7:9], 
                            title = NULL, 
                            enlarge = TRUE, 
                            enlarge_method = "modal")
                          )
          ),
        htmltools::br(),
        htmltools::p(lang$ui(33),
                     htmltools::br(),
                     htmltools::br(),
          #Video
          shiny::fluidRow(align = "center",
          htmltools::tags$video(
            id = ns("video_tutorial"),
            src = lang$server(1),
            width = "50%",
            type = "video/mp4",
            controls = "controls"
          )
        )),
        width = 12,
      ),
      shiny::fluidRow(align = "center",
               shiny::actionButton(
                 ns("zu_Import2"),
                 lang$ui(35),
                 class = "btn-lg",
                 shiny::icon("play", lib = "glyphicon")
                 )
               )
  ))
}

# Server ------------------------------------------------------------------

introductionServer <- 
  function(id
           ) {
  
    shiny::moduleServer(id, function(input, output, session) {
    
      #Return_Value
    shiny::reactive(
      list(in1 = (input$zu_Import1),
         in2 = (input$zu_Import2)
         ))
      
  })
}

# App ---------------------------------------------------------------------