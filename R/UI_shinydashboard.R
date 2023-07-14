#Sidebar
UI_Sidebar <- function() {
  shinydashboard::dashboardSidebar(
  width = 150,
  shinydashboard::sidebarMenu(
    id = "inTabset",
    htmltools::br(),
    htmltools::p(
      htmltools::img(
        width = "135px", src = "extr/Logo.png"),
      align = "center"),
    # htmltools::br(),
    shinydashboard::menuItem(lang$ui(1),
                             tabName = "tutorial",
                             icon = shiny::icon("circle-info")),
    htmltools::br(),
    shinydashboard::menuItem("Import",
                             tabName = "import",
                             icon = shiny::icon("file-import")
    ),
    shinydashboard::menuItemOutput("analysis"),
    shinydashboard::menuItemOutput("export"), 
    htmltools::br(),
    shinydashboard::menuItem(
      lang$ui(153),
      tabName = "validity",
      icon = shiny::icon("file-circle-check")
    ),
    shinydashboard::menuItem(
      lang$ui(158),
      tabName = "impressum",
      icon = shiny::icon("user", lib = "glyphicon")
    )
  )
  )
}

#Header
UI_Header <- function(lang_link) {
shinydashboard::dashboardHeader(
  title = "Spectran",
  titleWidth = 150,
  #conditional panel to change the language (only on shinyapps)
  htmltools::tags$li(
    class = "dropdown",
    if(lang_link)
      {
    htmltools::a(htmltools::HTML(
      paste0(lang$global(12), " ",
        shiny::icon(lang$global(13)))
    ),
    href = lang$global(14))}
    ),
  #feedback button
  htmltools::tags$li(
    class = "dropdown",
    htmltools::a(htmltools::HTML(
      paste0(
        shiny::icon("envelope"), "  Feedback")
    ),
    href = "mailto:spectran@litg.de")
  )
)
}