#removing the filetype from a filename
split_filename <- function(name) {
  stringr::str_remove(name, pattern = "[.]...$")
}

#setting a notification to inform the user about parameters
import_structure_notifier <- function(requirement,
                                      test, 
                                      text_if,
                                      text_else,
                                      id, 
                                      remove_id = NULL
                                      ) {
  shiny::req(requirement)
    test <- rlang::enquo(test)
    
    if (test %>% rlang::eval_tidy() %>% try() %>% shiny::isTruthy()) {
      shiny::showNotification(text_if,
                       type = "message",
                       duration = NULL,
                       id = id)
    }
    else {
      if (!is.null(remove_id) & is.character(remove_id)) {
        purrr::map(remove_id, shiny::removeNotification)
      }
      shiny::showNotification(text_else,
                       type = "error",
                       duration = NULL,
                       id = id)
    }
}

#Failsafe TRUE/FAlSE for shiny
ShinyTrue <- function(statement) {
  shiny::isTruthy(try(statement))
}

#creates a list from a dataframe with one listentry per row in the dataframe
DF2list <- function(dataframe) {
  split(dataframe, seq(nrow(dataframe)))
}

#Calculate photopic lux for a Spectrum (380:780nm)
Calc_lux <- 
  function(spectrum_vec, Action_Spectra_vec, Efficacy, Stepwidth = 1) {
  (spectrum_vec*Action_Spectra_vec[["V(lambda)"]]) %>% 
    sum(na.rm = TRUE)*Efficacy[["photopic"]]*Stepwidth
}

#Inverse Lookup function
inverse_lookup <- function(object) {
  stats::setNames(names(object), object)
}

#Funktion die Prüft, ob der Wert auf der linken Seite "Null" ist - falls ja, nimmt er den Wert auf der rechten Seite, sonst links
`%||%` <- function(x, y) if (is.null(x)) y else x


#remove notifications, when a tab changes
notification_remover <- function(tabset) {
shiny::observe({
  c("is_sufficient", "is_integer", "is_numeric", "belowz", "success") %>% 
    purrr::map(shiny::removeNotification)
  
  shinyalert::closeAlert()
  
}) %>% shiny::bindEvent(tabset())
}

#Change the Spectrum Name, if it is changed
Name_suffix <- function(Origin, Name) {

  if(Origin != "Construction") {
    Name
  }
  else if (stringr::str_detect(Name, paste0("[(]", lang$ui(177), "[)]"))) {
    Name
  }
  else paste0(Name, " (", lang$ui(177), ")")
  
}
