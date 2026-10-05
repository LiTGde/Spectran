# Source selection reuses the existing import server with task-specific UI.
material_source_example_box <- function(id, title, left = NULL, mid = NULL, right = NULL) {
  has_info <- !is.null(left) && !(is.atomic(left[[2]]) && all(is.na(left[[2]])))
  htmltools::tags$details(class = "material-source-example",
    htmltools::tags$summary(title),
    htmltools::div(class = paste("material-source-example-body", if (has_info) "has-info"),
      if (has_info) htmltools::div(class = "material-source-example-info", box_filling(id, left)),
      htmltools::div(class = "material-source-example-plot", box_filling(id, mid)),
      htmltools::div(class = "material-source-example-choice", box_filling(id, right))))
}

material_source_examples_ui <- function(id) {
  ns <- shiny::NS(id)
  examples <- examplespectra_descriptor[[the$language]] %>%
    dplyr::transmute(id = ns(Name), title = Beschreibung,
      left = list(list("video", URL)), mid = list(list("plot", Name)),
      right = list(list("download", download)))
  htmltools::tagList(
    htmltools::p(material_workspace_text("source_examples_help")),
    material_light_level_ui(ns),
    shiny::numericInput(ns("illu_eigen"), material_workspace_text("target_level"), 100, min = 0),
    material_source_example_box(ns("norm"), lang$ui(71),
      left = list("controls", htmltools::tagList(
        shiny::numericInput(ns("CCT_norm"), paste0(lang$ui(72), " (K)"),
          6500, min = 4000, max = 25000, step = 1000),
        htmltools::a("colorSpec", href = URL_colorSpec, target = "_blank", rel = "noopener noreferrer"))),
      mid = list("plot", "norm"), right = list("download", c(norm = "Import"))),
    purrr::pmap(examples, material_source_example_box))
}

material_source_file_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyFeedback::useShinyFeedback(),
    htmltools::p(material_workspace_text("file_help")),
    shiny::fileInput(ns("in_file"), lang$ui(57), accept = ".csv", width = "100%"),
    shiny::actionButton(ns("jgtm"), material_workspace_text("sample_source"), class = "btn-default"),
    htmltools::tags$details(class = "material-source-file-help",
      htmltools::tags$summary(material_workspace_text("csv_settings")),
      import_csv_settingsUI(ns("import")),
      htmltools::a(lang$ui(49), href = "extr/Beispiel.csv", download = "Beispiel.csv")),
    shiny::textInput(ns("name_id"), lang$ui(60), value = lang$ui(61), width = "100%"),
    import_visual_checkUI(ns("visual")),
    import_data_verifierUI(ns("importbutn"), label = material_workspace_text("use_source"),
      icon = shiny::icon("check"), class = "btn-primary"))
}

material_source_picker_ui <- function(id, history_ui = NULL) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::withMathJax(),
    htmltools::span(id = ns("heading"), tabindex = "-1"),
    shiny::tabsetPanel(id = ns("inTabset"),
      shiny::tabPanel(material_workspace_text("examples"), value = "examples",
        material_source_examples_ui(ns("examples"))),
      shiny::tabPanel(lang$ui(69), value = lang$ui(69), material_source_file_ui(ns("fileimport"))),
      shiny::tabPanel(lang$ui(94), value = "construction", import_eigenUI(ns("eigen"), workspace = TRUE)),
      if (!is.null(history_ui)) shiny::tabPanel(material_workspace_text("path_sources"),
        value = "light_path", history_ui),
      selected = "examples"),
    transmissionSourceImportUI(ns("history_reset"), workspace = TRUE))
}
