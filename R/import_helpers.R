#removing the filetype from a filename
split_filename <- function(name) {
  str_remove(name, pattern = "[.]...$")
}

#setting a notification to inform the user about parameters
import_structure_notifier <- function(requirement,
                                      test, 
                                      text_if,
                                      text_else,
                                      id, 
                                      remove_id = NULL
                                      ) {
    req(requirement)
    test <- enquo(test)
    
    if (test %>% eval_tidy() %>% try() %>% isTruthy()) {
      showNotification(text_if,
                       type = "message",
                       duration = NULL,
                       id = id)
    }
    else {
      if (!is.null(remove_id) & is.character(remove_id)) {
        map(remove_id, removeNotification)
      }
      showNotification(text_else,
                       type = "error",
                       duration = NULL,
                       id = id)
    }
}

#Failsafe TRUE/FAlSE for shiny
ShinyTrue <- function(statement) {
  isTruthy(try(statement))
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
  setNames(names(object), object)
}

#Funktion die Prüft, ob der Wert auf der linken Seite "Null" ist - falls ja, nimmt er den Wert auf der rechten Seite, sonst links
`%||%` <- function(x, y) if (is.null(x)) y else x
