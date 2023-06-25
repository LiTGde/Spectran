#creates the integral of the spectrum

Spec_int <- function(spectrum, sensitivity) {
  temp <- spectrum$Bestrahlungsstaerke * Specs$AS_wide[[sensitivity]]
  temp <- sum(temp)
  temp
}



# #Nimmt das Plot-Resizing wieder vor, sobald sich die Fensterbreite ändert
# observeEvent(input$dimension, {obs$resume()})

#Für eigenes Spektrum

# #Daten zur Anpassung übernehmen
# observe({#   
#   Ev <- sum(dat()[[as.numeric(input$x_y2)]]*Rezeptorkurven[[7]])*Strahlungsaqu[6]
#   updateNumericInput(session, inputId = "illu_eigen3", value = Ev)

#   eigen_Spektrum$Spec <- data.frame(Wellenlaenge = dat()[[as.numeric(input$x_y)]],
#                                     Bestrahlungsstaerke = dat()[[as.numeric(input$x_y2)]]/max(dat()[[as.numeric(input$x_y2)]]))
#   eigen_Spektrum$Spec3 <- eigen_Spektrum$Spec
#   updateCheckboxInput(session, "illu_eigen2", value = FALSE)
#   
# }) %>% bindEvent(input$zu_Aenderung)
# 
# 
# 
