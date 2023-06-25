#Here, helper functions to fill in the text parts of the app are defined

#An expression to catch the two ways in which a language can be defined 
#(globally or per call) and generate warning messages

lang_sec <- rlang::expr({
  if (!is.null(language_direct)) {
    #coming from the function call
    lang_setting <- language_direct
  }
  else if (exists("lang_setting", envir = rlang::caller_env(n=3))) {
    #coming globally
    lang_setting <- get("lang_setting", envir = rlang::caller_env(n=3))
  }
  else
    warning("No language chosen") #warning
  
  if (!lang_setting %in% (language$global %>% names())) {
    warning("Chosen language not available") # warning
  }
})

#Helper function to subset the language data frame for global, ui, 
#and server text

language_func <- function() {
  
lang_global <- function(number, language_direct = NULL) {
  eval(lang_sec)
  language$global[[lang_setting]][[number]]
}

lang_ui <- function(number, language_direct = NULL) {
  eval(lang_sec)
  language$ui[[lang_setting]][[number]]
}

lang_server <- function(number, language_direct = NULL) {
  eval(lang_sec)
  language$server[[lang_setting]][[number]]
}

list(global = lang_global,
     ui = lang_ui,
     server = lang_server)

}

lang <- language_func()