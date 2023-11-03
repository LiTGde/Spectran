
# UI ----------------------------------------------------------------------

import_eigen_plotUI <- 
  function(
    id) {

    ns <- shiny::NS(id)
    
    htmltools::tagList(
          #Main plot
      shiny::plotOutput(
            ns("eigenes_spektrum"),
            height = 400,
            click = ns("plot_click"),
            brush = ns("plot_brush")
          ),
          # Script so that you don´t click when brushing
          htmltools::tags$script(
            htmltools::HTML( paste0(
              "
              $('#", ns("eigenes_spektrum"), "').mousedown(function(e) {
              var parentOffset = $(this).offset();
              var relX = e.pageX - parentOffset.left;
              var relY = e.pageY - parentOffset.top;
              Shiny.setInputValue('", ns("x1"), "', relX);
              Shiny.setInputValue('", ns("y1"), "', relY);
              }).mouseup(function(e) {
              var parentOffset = $(this).offset();
              var relX = e.pageX - parentOffset.left;
              var relY = e.pageY - parentOffset.top;
              Shiny.setInputValue('", ns("x2"), "', relX);
              Shiny.setInputValue('", ns("y2"), "', relY);
              Shiny.setInputValue('", ns("action"), "', Math.random());
              });
              "
              )
              )
            )
          )
    }

# Server ------------------------------------------------------------------

import_eigen_plotServer <- 
  function(
    id, 
    Spectrum = NULL,
    sensitivitaeten,
    default,
    import
    ) {
  
    shiny::moduleServer(id, function(input, output, session) {
    
    #Set up a container for the spectra to go into, if it isn´t already defined
    if (is.null(Spectrum)){
      Spectrum <- 
        shiny::reactiveValues(
          Spectrum_raw = NULL, Name = NULL, Destination = NULL
          )
    }
    
    #Plottheme
    ggplot2::theme_set(
      cowplot::theme_cowplot(font_size = 15, font_family = "sans")
      )
    
    #Setup a raw spectrum
    Spec_raw <- tibble::tibble(Wellenlaenge = c(300, 379, 781, 850),
                       Bestrahlungsstaerke = 0,
                       keeprows = rep(TRUE, 4))
    
    eigen_Spektrum <- shiny::reactiveValues(Spec_raw = Spec_raw,
                                            original = Spec_raw)
    
    #Import a spectrum for adjustment
    shiny::observe({
      
      if(all((Spectrum$Destination) == lang$ui(94),
             !is.null((Spectrum$Spectrum)))) {
        
      eigen_Spektrum$Spec_raw <- Spectrum$Spectrum
      
      
      eigen_Spektrum$Spec_raw <- eigen_Spektrum$Spec_raw %>% 
        dplyr::mutate(
          Bestrahlungsstaerke =
            Bestrahlungsstaerke/(max(Bestrahlungsstaerke)),
               keeprows = TRUE)
      
      eigen_Spektrum$original <- eigen_Spektrum$Spec_raw
      
      eigen_Spektrum$Spec_raw <- eigen_Spektrum$Spec_raw %>% 
        rbind(Spec_raw)
      
      Spectrum$Emax_orig <- max(Spectrum$Spectrum[[2]])
      }
    }) %>% shiny::bindEvent(Spectrum$Destination, Spectrum$Spectrum)
    
    # Interpolate new wavelengths
    shiny::observe({
      r_fun <-
        stats::approxfun(
          x = eigen_Spektrum$Spec_raw$Wellenlaenge,
          y = eigen_Spektrum$Spec_raw$Bestrahlungsstaerke)
      eigen_Spektrum$gradient <-
        tibble::tibble(Wellenlaenge = 380:780,
               Bestrahlungsstaerke = r_fun(380:780)
               )
    })

    # Create a Spectral plot
    output$eigenes_spektrum <- shiny::renderPlot({

      # Create the color scale for the action spectra
      Colors <-
        Specs$Plot %>%
        dplyr::filter((Names %in% sensitivitaeten()))
      # Filter the action spectra
      Action_Spectra <- 
        Specs$AS_long %>%
        dplyr::filter(Type %in% sensitivitaeten())
      
      #Create the Hull of the Plot
      ggplot2::ggplot(data = eigen_Spektrum$Spec_raw,
             ggplot2::aes(x = Wellenlaenge, y = Bestrahlungsstaerke))+
        ggplot2::labs(x = lang$server(100), y = lang$server(36))+
        ggplot2::scale_x_continuous(breaks = c(400, 500, 600, 700, 780))+
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, .1)),
          breaks = c(0, 0.25, 0.5, 0.75, 1),
          labels = scales::percent_format(scale = 100))+
        ggplot2::coord_cartesian(ylim = c(0, 1), xlim = c(380, 780))+
        ggplot2::theme(legend.position = "none")+
        
        #Line with only the Chosen Points - for some reason that is connected to
        #the JS script used for click and brush, this cannot go below 
        #geom_ridgelinge, as the plot otherwise won´t create the correct labels
        ggplot2::geom_line(lwd = 1)+
        
        #Gradient fill under the curve - this needs the gradient-Spectrum data
        ggridges::geom_ridgeline_gradient(
          data = eigen_Spektrum$gradient,
          ggplot2::aes(
            y = 0, height =Bestrahlungsstaerke, fill = Wellenlaenge)
          )+
        ggplot2::scale_fill_gradientn(
          colors = ColorP[[the$palette]], guide = "none"
          )+
        
        #Points over the gradient
        ggplot2::geom_point(size = 2)+
        
        #Conditional dashed curve for imported spectra that get adjusted
        {if(all(
                !is.null((Spectrum$Spectrum)))) {
        ggplot2::geom_line(
          data = eigen_Spektrum$original,
          ggplot2::aes(y=Bestrahlungsstaerke), 
          lty = 5)
        }}+
        
        #Conditional action spectra along with their labels
        ggplot2::scale_color_manual(values = Specs$Plot$Col)+
        {if(length(sensitivitaeten()) != 0){
          ggplot2::geom_path(
            data = Specs$AS_long %>%
              dplyr::filter(Type %in% sensitivitaeten()),
            ggplot2::aes(x=Wellenlaenge, y= rel_Sens, col = Type),
            lwd = 0.75)}} + 
        {if(length(sensitivitaeten()) != 0){
          ggrepel::geom_label_repel(
            data = Colors,
            ggplot2::aes(x=Peak, y= 1, label = Names, col = Names),
            min.segment.length = 0,
            ylim = c(1, 1.1),
            alpha = 0.9,
            parse = TRUE)
      }}
    }, height = 400 )
    
    #Deals with click events. The inputs are defined in the UI script
    shiny::observe({
      #make shure that a brush is not registered as a click
      if(!(input$x1 == input$x2 && input$y1 == input$y2)) {
        return(NULL)
      }
      #collect all the near points
      Anzahl <- 
        shiny::nearPoints(
          eigen_Spektrum$Spec_raw, input$plot_click, threshold = 10
          ) 
      
      #create new points within reasonable limits
      if(
        ShinyTrue(all(
            nrow(Anzahl) == 0 & 
            input$plot_click$x %>% dplyr::between(380, 780) ,
            input$plot_click$y <= 1
            ))
        ){
        eigen_Spektrum$Spec_raw <- 
          eigen_Spektrum$Spec_raw %>% 
          rbind(c(input$plot_click$x, input$plot_click$y, keeprows = TRUE))
      } 
      #delete old points within reasonable limits
      else if (ShinyTrue(
        all(nrow(Anzahl) != 0
        )
        )
        ){try(
        eigen_Spektrum$Spec_raw <- 
          eigen_Spektrum$Spec_raw %>% 
          dplyr::filter(
            Wellenlaenge != 
              (Anzahl %>% dplyr::filter(
                dplyr::between(Wellenlaenge, 380, 780)
                ) %>% 
                 dplyr::pull(Wellenlaenge))))
      }
    }
    ) %>% shiny::bindEvent(input$action)
    
    #Deals with brush events. The inputs are defined in the UI script
    shiny::observe({
      #collect all the near points within reasonable limits
      Anzahl <- shiny::brushedPoints(eigen_Spektrum$Spec_raw, input$plot_brush)
      Anzahl <- Anzahl %>% dplyr::filter(dplyr::between(Wellenlaenge, 380, 780))
      
      #delete old points
      if(nrow(Anzahl) != 0) {
        eigen_Spektrum$Spec_raw <- 
          eigen_Spektrum$Spec_raw %>% 
          dplyr::filter( ! Wellenlaenge %in% Anzahl$Wellenlaenge)
      }
    }
    ) %>% shiny::bindEvent(input$plot_brush)

    #Reset the Spectrum
    shiny::observe({
      if(is.null(Spectrum$Spectrum)) {
        eigen_Spektrum$Spec_raw <- Spec_raw
        }
      else if (Spectrum$Destination == lang$ui(94)) {
        eigen_Spektrum$Spec_raw <- Spectrum$Spectrum %>%
          dplyr::mutate(Bestrahlungsstaerke =
                   Bestrahlungsstaerke/(max(Bestrahlungsstaerke)))
      }
      else eigen_Spektrum$Spec_raw <- eigen_Spektrum$original
      
    }) %>% shiny::bindEvent(default())
    
    #Return Value
    shiny::reactive(eigen_Spektrum$gradient)
  })
}

# App ---------------------------------------------------------------------
