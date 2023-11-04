table_exp <- function(f, ..., subtitle) {
  p1 <- f(..., subtitle = table_subtitle(subtitle))
  p1
}

#create a table with all the alpha values
Table_alpha <- function(
    Spectrum_Name,
    Table,
    subtitle = NULL,
    incl = "Zeichen",
    Breite = 100,
    Alpha){
  
  show <- Filter(\(x) {x}, Alpha) %>% unlist() %>% names()
  # Spectrum_Name <- Analysis$Settings$Spectrum_Name
  #data selection
  table <-
  Table %>% 
    dplyr::select(!c("Formelzeichen")) %>%
    #table generation
    gt::gt(rowname_col = "Groesse") %>% 
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_header(title = htmltools::strong(Spectrum_Name),
                   subtitle = gt::md(subtitle)) %>% 
    #formatting
    {Reduce(Number_formatting_tables, Specs$Alpha$names, init = .)} %>%
    {Reduce(\(data, x) {
      data %>% gt::tab_style(
                style = gt::cell_text(color = Specs$Plot$Col[[x]]),
                locations = gt::cells_column_labels(columns = x))
      },
      Specs$Alpha$names,
      init = .
    )} %>% 
      gt::fmt(
      columns = c("Zeichen", Groesse), fns = \(x) {purrr::map(x, gt::html)}) %>%
    gt::tab_options(
      table.width = gt::pct(Breite),
      container.padding.y = 0) %>% 
    #Source notes
    gt::tab_source_note(
      source_note = 
        gt::html(
          paste0(
        lang$server(72),
        lang$server(111),
        lang$server(73),
        htmltools::a(
          " CIE S026 (2018) ",
          href = URL_CIE,
          target="_blank"),
        lang$server(74)
        )
        )
      )
    
    if(length(show) != length(Specs$Alpha$names)) {
    table <- table %>% gt::cols_hide(!c(Groesse, incl, show, Einheit))
    }
      
    table %>% 
      gt::cols_label(Zeichen = "",
                     Groesse = "") %>% 
      gt::tab_style(
        style = gt::cell_borders(
          sides = c("left"),
          color = "grey70",
          weight = gt::px(2),
          style = "solid"
          ),
        locations = gt::cells_body(
          columns = Einheit,
          rows = gt::everything()
          )
        )

      
      
}