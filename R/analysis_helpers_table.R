#URLs for the hyperlinks
URL_colorSpec <- 'https://cran.r-project.org/web/packages/colorSpec/vignettes/colorSpec-guide.html'
URL_CIE <- 'https://cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0'
URL_DIN <- 'https://www.beuth.de/de/vornorm/din-ts-5031-100/343737176'

#Formats the Number-data in the tables
Number_formatting_tables <- function(data, column = Wert) {
  column <- rlang::ensym(column) %>% rlang::as_string()
  
  data %>%
    gt::fmt_number(
      columns = .data[[column]],
      rows = .data[[column]] >= 100,
      decimals = 0,
      sep_mark = " ",
      dec_mark = ","
    ) %>%
    gt::fmt_number(
      columns = .data[[column]],
      rows = .data[[column]] < 100 & .data[[column]] >= 10,
      decimals = 1,
      sep_mark = " ",
      dec_mark = ","
      ) %>% 
    gt::fmt_number(
      columns = (.data[[column]]),
      rows = .data[[column]] < 10 & .data[[column]] >= 1,
      decimals = 2,
      sep_mark = " ",
      dec_mark = ","
      ) %>%
    gt::fmt_number(
      columns = (.data[[column]]),
      rows = .data[[column]] < 1,
      n_sigfig = 3,
      sep_mark = " ",
      dec_mark = ","
      )
}

#Creating subtitles for tables (used for exporting)
table_subtitle <- function(subtitle) {
  beginning <- "<b style='float:left'>"
  ending <-
    paste0(
      "</b><span style='float:right'> ",
      lang$server(43) ,
      " <b>LiTG Spectran</b>",
      # htmltools::img(
      #   width = "17px",
      #   src = paste0(shiny::resourcePaths()[["extr"]], "/Logo.png")),
      # " (Zauner, 2023)",
      "</span>"
    )
  title <- paste0(beginning, subtitle, ending)
  gt::html(title)
}

#Creating a basic table
create_table <- function(
    Table,
    Spectrum_Name,
    subtitle,
    Breite = 100, 
    cols_scientific = NA){
  #data selection
  Table %>% dplyr::select(!c("Formelzeichen")) %>%
    #table generation
    gt::gt(rowname_col = "Groesse") %>% 
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_header(title = htmltools::strong(Spectrum_Name),
                   subtitle = gt::md(subtitle)) %>% 
    #formatting
    Number_formatting_tables() %>% 
    gt::fmt(
      columns = c("Zeichen"), fns = \(x) {purrr::map(x, gt::html)}) %>% 
    gt::fmt_scientific(
      columns = "Wert",
      rows = cols_scientific, 
      sep_mark = " ", 
      dec_mark = ",") %>% 
    gt::tab_options(
      column_labels.hidden = TRUE, table.width = gt::pct(Breite),
      container.padding.y = 0)
}

#Creates a Tablefunction for Radiometry
table_rad <- function(...){
  create_table(...) %>%
    gt::tab_footnote(
      footnote = lang$server(44),
      locations = gt::cells_stub(rows = lang$server(40))
    ) %>%
    gt::tab_footnote(
      footnote = lang$server(45),
      locations = gt::cells_stub(rows = lang$server(41))
    ) %>%
    gt::tab_footnote(
      footnote = lang$server(46),
      locations = gt::cells_stub(rows = lang$server(42))
    ) %>%
    gt::tab_source_note(
      source_note = lang$server(47)
    )
}

#Creates a Table for Photometry
table_phot <- function(..., CIE_grenzen){
  create_table(...) %>%
    gt::fmt_number(columns = "Wert",
                   rows = 4:5, decimals = 0,
                   sep_mark = " ", dec_mark = ",") %>%
    gt::tab_source_note(
      source_note = gt::html(lang$server(58))
    ) %>%
    gt::tab_source_note(
      source_note = gt::md(
        paste0(lang$server(59),
               htmltools::a(
                 " colorSpec. ",
                 href = URL_colorSpec,
                 target="_blank"),
               lang$server(60),
               ifelse(CIE_grenzen, " ",
                      lang$server(61)
               ),
               lang$server(62)
        )
      )
    )
}

#Creates a Table for Alphaopy
table_alph <- function(..., index = index){
  create_table(...) %>%
    gt::tab_source_note(
      source_note = gt::md(
        paste0(lang$server(72),
               Specs$Alpha$adjectives[[the$language]][[index]],
               lang$server(73),
               htmltools::a(
                 " CIE S026 (2018) ",
                 href = URL_CIE,
                 target="_blank"),
               lang$server(74))
      )
    )
}

#Fußnote für alle Tabellen mit den altersabhängigen Korrekturfaktoren
#Footnote for all tables with agedependent correction factors
Quelle_Alter <- rlang::expr(
  gt::md(
    paste0(lang$server(88), 
           htmltools::a(
             " DIN/TS 5031-100:2021-11. ",
             href = URL_DIN,
             target="_blank")
    )
  )
)

#Age table
table_age <- function(Table, ..., slice){
  Table %>% dplyr::slice(slice) %>% 
    create_table(...) %>% 
    gt::tab_footnote(
      footnote =  gt::html(lang$server(97)),
      locations = gt::cells_stub(rows = tidyselect::starts_with("MEDI"))
    ) %>%
    gt::tab_source_note(source_note = eval(Quelle_Alter)) %>%
    gt::tab_source_note(source_note = shiny::withMathJax("")) %>% 
    {if((identical(slice, 1:5))){
    gt::tab_style(.,
      style = gt::cell_fill(color = "grey95"), 
      locations = 
        list(
          gt::cells_body(columns = tidyselect::everything(), rows = 4:5),
          gt::cells_stub(rows = 4:5)
          )
      )
    }
      else .
      }
    
}
