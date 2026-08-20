# Transmission presentation helpers --------------------------------------

#' Current locale for Transmission tables
#'
#' @return A locale identifier understood by `gt`.
#' @noRd
transmission_gt_locale <- function() {
  if (identical(transmission_language_setting(), "Deutsch")) "de" else "en"
}

#' Add Spectran's export watermark without depending on a reactive language
#'
#' @param font_size Base plot font size.
#' @param language_direct Optional explicit language.
#'
#' @return A patchwork plot annotation.
#' @noRd
transmission_plot_footnote <- function(
  font_size = 15,
  language_direct = NULL
) {
  patchwork::plot_annotation(
    caption = paste0(
      transmission_text(
        "watermark_created_with",
        language_direct = language_direct
      ),
      "**LiTG Spectran**"
    ),
    theme = ggplot2::theme(
      plot.caption = ggtext::element_markdown(
        size = font_size / 3 * 2,
        colour = "#4b4b4b"
      ),
      plot.margin = ggplot2::margin()
    )
  )
}

#' Render a `gt` table without registering a second Shiny input binding
#'
#' `gt::gt_output()` intentionally registers both input and output bindings
#' under the same identifier. Shiny 1.14 reports that shared identifier as a
#' diagnostic warning. Transmission tables are display-only, so raw `gt` HTML
#' inside a regular Shiny UI output is the appropriate contract.
#'
#' @param table A `gt_tbl`.
#'
#' @return HTML suitable for `shiny::renderUI()`.
#' @noRd
transmission_gt_html <- function(table) {
  stopifnot(inherits(table, "gt_tbl"))
  htmltools::HTML(gt::as_raw_html(table, inline_css = TRUE))
}

#' Display-only Shiny output container for a Transmission `gt` table
#'
#' @param output_id Namespaced Shiny output identifier.
#'
#' @return A Shiny UI output tag.
#' @noRd
transmission_gt_output <- function(output_id) {
  shiny::uiOutput(output_id)
}

#' Render a plain scientific symbol with a typographic subscript
#'
#' Transmission metric contracts deliberately retain plain-text symbols for
#' CSV and audit exports. This helper is used only by visible `gt` tables so
#' their notation matches Spectran's Analysis tables.
#'
#' @param symbol Plain-text scientific symbol.
#'
#' @return HTML strings with every qualifier after the leading symbol rendered
#'   as a subscript.
#' @noRd
transmission_symbol_html <- function(symbol) {
  symbol <- as.character(symbol)
  empty <- is.na(symbol) | !nzchar(symbol)
  escaped <- htmltools::htmlEscape(symbol)
  rendered <- paste0(
    substr(escaped, 1L, 1L),
    "<sub>",
    substring(escaped, 2L),
    "</sub>"
  )
  rendered[empty] <- ""
  rendered
}

#' Format visible scientific symbols in a `gt` table
#'
#' @param table A `gt_tbl` containing a plain-text `symbol` column.
#'
#' @return The table with display-only HTML subscripts.
#' @noRd
transmission_gt_format_symbols <- function(table) {
  gt::text_transform(
    table,
    fn = function(values) {
      lapply(transmission_symbol_html(values), gt::html)
    },
    locations = gt::cells_body(columns = tidyselect::all_of("symbol"))
  )
}

#' Transmission tabset with deterministic, namespaced panel identifiers
#'
#' Shiny and bslib normally generate a short random integer for each Bootstrap
#' tabset. Two tabsets on the same page can therefore receive identical panel
#' IDs. Bootstrap then activates the first matching panel in the document,
#' which can leave a selected Transmission tab showing content from another
#' tabset. The Shiny input ID is already globally namespaced, so it provides a
#' stable collision-free basis for the navigation and panel identifiers.
#'
#' @param ... Unnamed `shiny::tabPanel()` objects.
#' @param id Required, globally namespaced Shiny input identifier.
#' @param selected Initially selected tab value.
#' @param type Tabset presentation type.
#'
#' @return A Shiny tabset whose navigation targets and panels use deterministic
#'   identifiers.
#' @noRd
transmission_tabset_panel <- function(
  ...,
  id,
  selected = NULL,
  type = c("tabs", "pills", "hidden")
) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("`id` must be one non-empty namespaced identifier.", call. = FALSE)
  }
  type <- match.arg(type)
  tabset <- shiny::tabsetPanel(
    ...,
    id = id,
    selected = selected,
    type = type
  )

  navigation_index <- which(vapply(
    tabset$children,
    function(child) {
      inherits(child, "shiny.tag") &&
        identical(child$name, "ul") &&
        grepl("(^| )nav( |$)", child$attribs$class %||% "")
    },
    logical(1)
  ))
  content_index <- which(vapply(
    tabset$children,
    function(child) {
      inherits(child, "shiny.tag") &&
        identical(child$name, "div") &&
        grepl("(^| )tab-content( |$)", child$attribs$class %||% "")
    },
    logical(1)
  ))
  if (length(navigation_index) != 1L || length(content_index) != 1L) {
    stop("Unexpected Shiny tabset structure.", call. = FALSE)
  }

  navigation <- tabset$children[[navigation_index]]
  content <- tabset$children[[content_index]]
  if (length(navigation$children) != length(content$children)) {
    stop("Transmission tab navigation and panel counts differ.", call. = FALSE)
  }

  identifier <- paste0(
    "spectran-",
    gsub("[^A-Za-z0-9_-]+", "-", id)
  )
  navigation$attribs[["data-tabsetid"]] <- identifier
  content$attribs[["data-tabsetid"]] <- identifier

  for (index in seq_along(navigation$children)) {
    target <- paste0("tab-", identifier, "-", index)
    navigation_item <- navigation$children[[index]]
    panel <- content$children[[index]]
    if (
      !inherits(navigation_item, "shiny.tag") ||
        length(navigation_item$children) < 1L ||
        !inherits(navigation_item$children[[1]], "shiny.tag") ||
        !identical(navigation_item$children[[1]]$name, "a") ||
        !inherits(panel, "shiny.tag")
    ) {
      stop("Unexpected Transmission tab item structure.", call. = FALSE)
    }
    navigation_item$children[[1]]$attribs$href <- paste0("#", target)
    panel$attribs$id <- target
    navigation$children[[index]] <- navigation_item
    content$children[[index]] <- panel
  }

  tabset$children[[navigation_index]] <- navigation
  tabset$children[[content_index]] <- content
  tabset
}

#' Build the script that keeps a programmatically selected tab visible
#'
#' @param tabset_id Complete, namespaced tabset identifier.
#' @param value Tab value selected by Shiny.
#'
#' @return A validated JavaScript statement for `shinyjs::runjs()`.
#' @noRd
transmission_tab_scroll_script <- function(tabset_id, value) {
  tabset_id <- transmission_scalar_text(tabset_id, "tabset_id")
  value <- transmission_scalar_text(value, "value")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.:-]*$", tabset_id)) {
    stop("`tabset_id` contains unsupported characters.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_-]+$", value)) {
    stop("`value` contains unsupported characters.", call. = FALSE)
  }
  paste0(
    "(function () { var tabset = document.getElementById('",
    tabset_id,
    "'); if (!tabset) { return; } var tab = tabset.querySelector(",
    "'a[data-value=\"",
    value,
    "\"]'); if (!tab) { return; } var padding = 12; ",
    "var adjustItem = function (item) { if (!item) { return; } ",
    "var viewRect = tabset.getBoundingClientRect(); ",
    "var itemRect = item.getBoundingClientRect(); ",
    "var target = tabset.scrollLeft; ",
    "if (itemRect.left < viewRect.left + padding) { ",
    "target -= viewRect.left + padding - itemRect.left; } ",
    "else if (itemRect.right > viewRect.right - padding) { ",
    "target += itemRect.right - (viewRect.right - padding); } ",
    "var maximum = Math.max(0, tabset.scrollWidth - tabset.clientWidth); ",
    "tabset.scrollLeft = Math.max(0, Math.min(maximum, target)); }; ",
    "var item = tab.closest('li') || tab; ",
    "requestAnimationFrame(function () { ",
    "requestAnimationFrame(function () { adjustItem(item); }); }); ",
    "setTimeout(function () { adjustItem(item); }, 120); ",
    "setTimeout(function () { adjustItem(item); }, 320); ",
    "setTimeout(function () { adjustItem(item); }, 800); ",
    "setTimeout(function () { adjustItem(item); }, 1600); }());"
  )
}

#' Keep each newly active Transmission tab fully visible
#'
#' @param tabset_id Complete, namespaced tabset identifier.
#'
#' @return A validated JavaScript statement for an inline script tag.
#' @noRd
transmission_tab_observer_script <- function(tabset_id) {
  tabset_id <- transmission_scalar_text(tabset_id, "tabset_id")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.:-]*$", tabset_id)) {
    stop("`tabset_id` contains unsupported characters.", call. = FALSE)
  }
  paste0(
    "(function () { var install = function () { var tabset = ",
    "document.getElementById('",
    tabset_id,
    "'); if (!tabset) { return false; } ",
    "if (tabset.getAttribute('data-spectran-tab-scroll') === 'true') { ",
    "return true; } var padding = 12; var adjust = function () { ",
    "var item = tabset.querySelector('li.active'); if (!item) { return; } ",
    "var viewRect = tabset.getBoundingClientRect(); ",
    "var itemRect = item.getBoundingClientRect(); ",
    "var target = tabset.scrollLeft; ",
    "if (itemRect.left < viewRect.left + padding) { ",
    "target -= viewRect.left + padding - itemRect.left; } ",
    "else if (itemRect.right > viewRect.right - padding) { ",
    "target += itemRect.right - (viewRect.right - padding); } ",
    "var maximum = Math.max(0, tabset.scrollWidth - tabset.clientWidth); ",
    "tabset.scrollLeft = Math.max(0, Math.min(maximum, target)); }; ",
    "var schedule = function () { requestAnimationFrame(function () { ",
    "requestAnimationFrame(adjust); }); setTimeout(adjust, 120); ",
    "setTimeout(adjust, 320); setTimeout(adjust, 800); ",
    "setTimeout(adjust, 1600); }; ",
    "var observer = new MutationObserver(schedule); ",
    "observer.observe(tabset, { subtree: true, attributes: true, ",
    "attributeFilter: ['class', 'aria-selected'] }); ",
    "tabset.__spectranTabScrollObserver = observer; ",
    "if (window.jQuery) { window.jQuery(tabset).off(",
    "'shown.bs.tab.spectranTabScroll').on(",
    "'shown.bs.tab.spectranTabScroll', schedule); } ",
    "tabset.setAttribute('data-spectran-tab-scroll', 'true'); ",
    "schedule(); return true; }; if (!install()) { ",
    "requestAnimationFrame(install); setTimeout(install, 120); } }());"
  )
}

#' Keep a programmatically selected Transmission tab in view
#'
#' @param tabset_id Complete, namespaced tabset identifier.
#' @param value Selected tab value.
#'
#' @return `NULL`, invisibly.
#' @noRd
transmission_scroll_tab_into_view <- function(tabset_id, value) {
  shinyjs::runjs(transmission_tab_scroll_script(tabset_id, value))
  invisible(NULL)
}

#' Build the delegated keyboard handler for guided Transmission actions
#'
#' Dynamic Shiny buttons are replaced when their labels or availability
#' change. A single delegated listener therefore remains valid without
#' accumulating element-specific handlers.
#'
#' @return A JavaScript statement installed once per document.
#' @noRd
transmission_guided_keyboard_script <- function() {
  paste0(
    "(function () { ",
    "if (window.__spectranTransmissionGuidedKeyboard) { return; } ",
    "window.__spectranTransmissionGuidedKeyboard = true; ",
    "document.addEventListener('keydown', function (event) { ",
    "var target = event.target.closest('.transmission-guided-action'); ",
    "if (!target || target.disabled || event.repeat) { return; } ",
    "var activation = event.key === 'Enter' || event.key === ' ' || ",
    "event.key === 'Spacebar' || event.code === 'Space'; ",
    "if (!activation) { return; } ",
    "event.preventDefault(); event.stopPropagation(); target.click(); ",
    "}); }());"
  )
}

#' Restore native Space activation for checkbox inputs
#'
#' Some combinations of the dashboard shell and browser keyboard dispatch do
#' not emit the checkbox's native click after Space. The delegated handler
#' performs that activation once, while preventing a second native toggle.
#'
#' @return A JavaScript statement installed once per document.
#' @noRd
spectran_checkbox_keyboard_script <- function() {
  paste0(
    "(function () { ",
    "if (window.__spectranCheckboxKeyboard) { return; } ",
    "window.__spectranCheckboxKeyboard = true; ",
    "document.addEventListener('keydown', function (event) { ",
    "var target = event.target; ",
    "if (!(target instanceof HTMLInputElement) || ",
    "target.type !== 'checkbox' || target.disabled || event.repeat) { ",
    "return; } ",
    "var activation = event.key === ' ' || event.key === 'Spacebar' || ",
    "event.code === 'Space'; ",
    "if (!activation) { return; } ",
    "event.preventDefault(); event.stopPropagation(); ",
    "var original = target; var targetId = target.id; ",
    "var restoreFocus = function (attempt) { ",
    "window.setTimeout(function () { ",
    "var current = targetId ? document.getElementById(targetId) : null; ",
    "var active = document.activeElement; ",
    "if (current && current !== original && ",
    "(active === original || active === document.body || active === null)) { ",
    "try { current.focus({preventScroll: true}); } ",
    "catch (error) { current.focus(); } return; } ",
    "if (active && active !== original && active !== document.body) { return; } ",
    "if (attempt < 20) { restoreFocus(attempt + 1); } ",
    "}, 50); }; ",
    "target.click(); restoreFocus(0); ",
    "}); }());"
  )
}

#' Spectran's configured wavelength palette with a safe fallback
#'
#' @return A character vector of colours.
#' @noRd
transmission_spectral_palette <- function() {
  palette_name <- if (
    exists("palette", envir = the, inherits = FALSE) &&
      is.character(the$palette) &&
      length(the$palette) == 1L &&
      the$palette %in% names(ColorP)
  ) {
    the$palette
  } else {
    "Lang"
  }
  ColorP[[palette_name]]
}

#' Apply Spectran's table hierarchy to a `gt` table
#'
#' @param table A `gt_tbl`.
#' @param compact Whether to use the compact preview-table spacing.
#' @param variant `"utility"` for preview/history tables or `"analysis"`
#'   for the continuous Analysis-style result package.
#'
#' @return A styled `gt_tbl`.
#' @noRd
transmission_gt_theme <- function(
  table,
  compact = FALSE,
  variant = c("utility", "analysis")
) {
  stopifnot(inherits(table, "gt_tbl"))
  variant <- match.arg(variant)

  if (identical(variant, "analysis")) {
    return(
      table |>
        gt::opt_align_table_header(align = "left") |>
        gt::tab_options(
          table.width = gt::pct(100),
          table.layout = "auto",
          table.align = "left",
          table.background.color = "transparent",
          table.font.names = c(
            "system-ui",
            "-apple-system",
            "BlinkMacSystemFont",
            "Segoe UI",
            "sans-serif"
          ),
          table.font.size = gt::px(14),
          table.border.top.style = "solid",
          table.border.top.width = gt::px(2),
          table.border.top.color = "#9a9a9a",
          table.border.bottom.style = "solid",
          table.border.bottom.width = gt::px(2),
          table.border.bottom.color = "#9a9a9a",
          heading.background.color = "transparent",
          heading.title.font.size = gt::px(16),
          heading.title.font.weight = "bold",
          heading.subtitle.font.size = gt::px(13),
          heading.padding = gt::px(5),
          heading.border.bottom.style = "solid",
          heading.border.bottom.width = gt::px(1),
          heading.border.bottom.color = "#9a9a9a",
          column_labels.background.color = "transparent",
          column_labels.font.weight = "normal",
          column_labels.padding = gt::px(5),
          column_labels.border.top.style = "none",
          column_labels.border.bottom.style = "solid",
          column_labels.border.bottom.width = gt::px(1),
          column_labels.border.bottom.color = "#b3b3b3",
          table_body.hlines.style = "solid",
          table_body.hlines.width = gt::px(1),
          table_body.hlines.color = "#d0d0d0",
          data_row.padding = gt::px(5),
          source_notes.background.color = "transparent",
          source_notes.font.size = gt::px(12),
          source_notes.padding = gt::px(5),
          source_notes.border.lr.style = "none",
          source_notes.border.bottom.style = "none",
          container.width = gt::pct(100),
          container.padding.x = gt::px(0),
          container.padding.y = gt::px(0),
          container.overflow.x = TRUE
        )
    )
  }

  table |>
    gt::opt_align_table_header(align = "left") |>
    gt::opt_row_striping() |>
    gt::tab_options(
      table.width = gt::pct(100),
      table.layout = "auto",
      table.align = "left",
      table.background.color = "white",
      table.font.names = c("Arial", "Helvetica", "sans-serif"),
      table.font.size = gt::px(if (isTRUE(compact)) 12 else 14),
      table.border.top.style = "solid",
      table.border.top.width = gt::px(1),
      table.border.top.color = "#9a9a9a",
      table.border.bottom.style = "solid",
      table.border.bottom.width = gt::px(1),
      table.border.bottom.color = "#9a9a9a",
      heading.background.color = "white",
      heading.title.font.size = gt::px(if (isTRUE(compact)) 14 else 16),
      heading.subtitle.font.size = gt::px(12),
      heading.padding = gt::px(if (isTRUE(compact)) 5 else 8),
      column_labels.background.color = "#f3f3f3",
      column_labels.font.weight = "bold",
      column_labels.padding = gt::px(if (isTRUE(compact)) 5 else 7),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#9a9a9a",
      table_body.hlines.style = "solid",
      table_body.hlines.width = gt::px(1),
      table_body.hlines.color = "#d5d5d5",
      data_row.padding = gt::px(if (isTRUE(compact)) 4 else 6),
      row.striping.background_color = "#f7f7f7",
      row.striping.include_stub = TRUE,
      source_notes.background.color = "white",
      source_notes.font.size = gt::px(11),
      source_notes.padding = gt::px(6),
      container.width = gt::pct(100),
      container.padding.x = gt::px(0),
      container.padding.y = gt::px(0),
      container.overflow.x = TRUE
    )
}

#' Format numeric columns in a Transmission `gt` table
#'
#' @param table A `gt_tbl`.
#' @param columns Character vector of numeric column names.
#' @param trim_trailing_zeros Whether to suppress trailing zeroes. Result
#'   metrics use a fixed three-decimal display; source-data previews retain
#'   their compact audit-oriented formatting.
#'
#' @return A formatted `gt_tbl`.
#' @noRd
transmission_gt_format_numbers <- function(
  table,
  columns,
  trim_trailing_zeros = FALSE
) {
  gt::fmt_number(
    table,
    columns = tidyselect::all_of(columns),
    decimals = 3,
    drop_trailing_zeros = trim_trailing_zeros,
    drop_trailing_dec_mark = trim_trailing_zeros,
    use_seps = TRUE,
    locale = transmission_gt_locale()
  )
}

#' Build a compact `gt` preview of parsed transmission rows
#'
#' @param preparation A transmission preparation.
#'
#' @return A `gt_tbl`.
#' @noRd
transmission_input_preview_gt <- function(preparation) {
  stopifnot(inherits(preparation, "transmission_preparation"))
  stopifnot(!is.null(preparation$normalized))

  preview <- utils::head(preparation$normalized, 10L)
  original_match <- match(preview$source_row, preparation$original$source_row)
  table_data <- data.frame(
    source_row = preview$source_row,
    wavelength_nm = preview$wavelength_nm,
    input_value = preparation$original$transmission_input[original_match],
    scaled_transmittance = preview$transmittance,
    inside = ifelse(
      preview$within_calculation_range,
      transmission_text("yes"),
      transmission_text("no")
    ),
    check.names = FALSE
  )

  table_data |>
    gt::gt() |>
    gt::cols_label(
      source_row = transmission_text("preview_source_row"),
      wavelength_nm = transmission_text("preview_wavelength"),
      input_value = transmission_text("preview_input"),
      scaled_transmittance = transmission_text("preview_scaled"),
      inside = transmission_text("preview_inside")
    ) |>
    transmission_gt_format_numbers(
      columns = c("wavelength_nm", "input_value", "scaled_transmittance"),
      trim_trailing_zeros = TRUE
    ) |>
    gt::cols_align(
      align = "right",
      columns = tidyselect::all_of(c(
        "source_row",
        "wavelength_nm",
        "input_value",
        "scaled_transmittance"
      ))
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::all_of("inside")
    ) |>
    transmission_gt_theme(compact = TRUE)
}

#' Build a compact `gt` summary of the completed calculation grid
#'
#' @param preparation A transmission preparation.
#'
#' @return A `gt_tbl`.
#' @noRd
transmission_status_summary_gt <- function(preparation) {
  stopifnot(inherits(preparation, "transmission_preparation"))
  summary <- transmission_status_summary(preparation)
  table_data <- data.frame(
    status = transmission_status_label(summary$status),
    samples = summary$samples,
    check.names = FALSE
  )

  table_data |>
    gt::gt(rowname_col = "status") |>
    gt::cols_label(samples = transmission_text("preview_samples")) |>
    gt::fmt_integer(
      columns = tidyselect::all_of("samples"),
      locale = transmission_gt_locale()
    ) |>
    gt::cols_align(
      align = "right",
      columns = tidyselect::all_of("samples")
    ) |>
    transmission_gt_theme(compact = TRUE)
}

#' Build the session history as a `gt` table
#'
#' @param history Valid session-local transmission history.
#'
#' @return A `gt_tbl` with the active node highlighted.
#' @noRd
transmission_history_gt <- function(history) {
  validate_transmission_history(history)
  records <- transmission_history_table(history)
  table_data <- data.frame(
    sequence = records$sequence_id,
    node = records$node_id,
    parent = ifelse(
      is.na(records$parent_id),
      transmission_text("history_root"),
      records$parent_id
    ),
    name = records$name,
    created_by = ifelse(
      records$change_type == "import",
      transmission_text("history_import"),
      transmission_text("history_promotion")
    ),
    active = ifelse(
      records$active,
      transmission_text("yes"),
      transmission_text("no")
    ),
    active_flag = records$active,
    check.names = FALSE
  )

  table_data |>
    gt::gt() |>
    gt::tab_header(title = transmission_text("history_tree")) |>
    gt::cols_label(
      sequence = transmission_text("history_sequence"),
      node = transmission_text("history_node"),
      parent = transmission_text("history_parent"),
      name = transmission_text("history_name"),
      created_by = transmission_text("history_created_by"),
      active = transmission_text("history_active")
    ) |>
    gt::fmt_integer(
      columns = tidyselect::all_of("sequence"),
      locale = transmission_gt_locale()
    ) |>
    gt::cols_hide(columns = tidyselect::all_of("active_flag")) |>
    gt::cols_align(
      align = "right",
      columns = tidyselect::all_of("sequence")
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::all_of(c("node", "parent", "active"))
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#fff8be"),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(rows = which(table_data$active_flag))
    ) |>
    transmission_gt_theme()
}

#' Build the D65 properties `gt` table
#'
#' @param snapshot Immutable applied transmission snapshot.
#'
#' @return A `gt_tbl`.
#' @noRd
transmission_d65_gt <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  properties <- snapshot$d65_properties
  table_data <- data.frame(
    property = transmission_metric_labels(
      properties$metric_id,
      properties$metric_label
    ),
    symbol = properties$symbol,
    fraction = ifelse(
      properties$defined,
      properties$transmitted_value,
      NA_real_
    ),
    percent = ifelse(
      properties$defined,
      properties$transmitted_value,
      NA_real_
    ),
    check.names = FALSE
  )

  table_data |>
    gt::gt(rowname_col = "property") |>
    gt::tab_header(
      title = transmission_text("d65_heading"),
      subtitle = transmission_text("gt_d65_subtitle")
    ) |>
    gt::cols_label(
      symbol = transmission_text("table_symbol"),
      fraction = transmission_text("table_fraction"),
      percent = transmission_text("table_percent")
    ) |>
    transmission_gt_format_symbols() |>
    gt::tab_spanner(
      label = transmission_text("gt_transmittance_spanner"),
      columns = tidyselect::all_of(c("fraction", "percent"))
    ) |>
    transmission_gt_format_numbers(columns = "fraction") |>
    gt::fmt_percent(
      columns = tidyselect::all_of("percent"),
      decimals = 1,
      drop_trailing_zeros = FALSE,
      locale = transmission_gt_locale()
    ) |>
    gt::sub_missing(
      columns = tidyselect::all_of(c("fraction", "percent")),
      missing_text = transmission_text("undefined")
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::all_of("symbol")
    ) |>
    gt::cols_align(
      align = "right",
      columns = tidyselect::all_of(c("fraction", "percent"))
    ) |>
    gt::tab_source_note(source_note = transmission_text("gt_d65_note")) |>
    transmission_gt_theme(variant = "analysis")
}

#' Build the incident and transmitted absolute-metrics `gt` table
#'
#' @param snapshot Immutable applied transmission snapshot.
#'
#' @return A `gt_tbl`.
#' @noRd
transmission_absolute_gt <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  metrics <- snapshot$active_metrics[
    snapshot$active_metrics$comparison_type == "retained",
    ,
    drop = FALSE
  ]
  table_data <- data.frame(
    metric = transmission_metric_labels(
      metrics$metric_id,
      metrics$metric_label
    ),
    symbol = metrics$symbol,
    incident = ifelse(metrics$defined, metrics$incident_value, NA_real_),
    transmitted = ifelse(metrics$defined, metrics$transmitted_value, NA_real_),
    unit = metrics$unit,
    retained = ifelse(
      metrics$comparison_defined,
      metrics$comparison_value,
      NA_real_
    ),
    check.names = FALSE
  )

  table_data |>
    gt::gt(rowname_col = "metric") |>
    gt::tab_header(title = transmission_text("absolute_heading")) |>
    gt::cols_label(
      symbol = transmission_text("table_symbol"),
      incident = transmission_text("table_incident"),
      transmitted = transmission_text("table_transmitted"),
      unit = transmission_text("table_unit"),
      retained = transmission_text("table_retained")
    ) |>
    transmission_gt_format_symbols() |>
    gt::tab_spanner(
      label = transmission_text("gt_light_values_spanner"),
      columns = tidyselect::all_of(c("incident", "transmitted"))
    ) |>
    transmission_gt_format_numbers(columns = c("incident", "transmitted")) |>
    gt::fmt_percent(
      columns = tidyselect::all_of("retained"),
      decimals = 1,
      drop_trailing_zeros = FALSE,
      locale = transmission_gt_locale()
    ) |>
    gt::sub_missing(
      columns = tidyselect::all_of(c(
        "incident",
        "transmitted",
        "retained"
      )),
      missing_text = transmission_text("undefined")
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::all_of(c("symbol", "unit"))
    ) |>
    gt::cols_align(
      align = "right",
      columns = tidyselect::all_of(c(
        "incident",
        "transmitted",
        "retained"
      ))
    ) |>
    gt::tab_source_note(source_note = transmission_text("gt_retained_note")) |>
    transmission_gt_theme(variant = "analysis")
}

#' Build the action-factor and DER `gt` table
#'
#' @param snapshot Immutable applied transmission snapshot.
#'
#' @return A `gt_tbl`.
#' @noRd
transmission_balance_gt <- function(snapshot) {
  validate_transmission_applied_snapshot(snapshot)
  metrics <- snapshot$active_metrics[
    snapshot$active_metrics$comparison_type == "change",
    ,
    drop = FALSE
  ]
  table_data <- data.frame(
    metric = transmission_metric_labels(
      metrics$metric_id,
      metrics$metric_label
    ),
    symbol = metrics$symbol,
    incident = ifelse(
      is.finite(metrics$incident_value),
      metrics$incident_value,
      NA_real_
    ),
    transmitted = ifelse(
      is.finite(metrics$transmitted_value),
      metrics$transmitted_value,
      NA_real_
    ),
    absolute_change = ifelse(
      metrics$comparison_defined,
      metrics$absolute_change,
      NA_real_
    ),
    relative_change = ifelse(
      metrics$relative_change_defined,
      metrics$relative_change,
      NA_real_
    ),
    check.names = FALSE
  )

  table_data |>
    gt::gt(rowname_col = "metric") |>
    gt::tab_header(title = transmission_text("balance_heading")) |>
    gt::cols_label(
      symbol = transmission_text("table_symbol"),
      incident = transmission_text("table_incident"),
      transmitted = transmission_text("table_transmitted"),
      absolute_change = transmission_text("table_absolute_change"),
      relative_change = transmission_text("table_relative_change")
    ) |>
    transmission_gt_format_symbols() |>
    gt::tab_spanner(
      label = transmission_text("gt_balance_values_spanner"),
      columns = tidyselect::all_of(c("incident", "transmitted"))
    ) |>
    gt::tab_spanner(
      label = transmission_text("gt_change_spanner"),
      columns = tidyselect::all_of(c(
        "absolute_change",
        "relative_change"
      ))
    ) |>
    transmission_gt_format_numbers(
      columns = c("incident", "transmitted", "absolute_change")
    ) |>
    gt::fmt_percent(
      columns = tidyselect::all_of("relative_change"),
      decimals = 1,
      drop_trailing_zeros = FALSE,
      force_sign = TRUE,
      locale = transmission_gt_locale()
    ) |>
    gt::sub_missing(
      columns = tidyselect::all_of(c(
        "incident",
        "transmitted",
        "absolute_change",
        "relative_change"
      )),
      missing_text = transmission_text("undefined")
    ) |>
    gt::cols_align(
      align = "center",
      columns = tidyselect::all_of("symbol")
    ) |>
    gt::cols_align(
      align = "right",
      columns = tidyselect::all_of(c(
        "incident",
        "transmitted",
        "absolute_change",
        "relative_change"
      ))
    ) |>
    gt::tab_source_note(source_note = transmission_text("balance_intro")) |>
    transmission_gt_theme(variant = "analysis")
}

#' Clean transmittance panel for applied-result plots
#'
#' @param snapshot Immutable applied transmission snapshot.
#' @param show_title Whether to show the panel title.
#' @param panel_tag Optional panel tag.
#' @param compact_x Retained for compatibility. Panel B uses Spectran's
#'   Analysis wavelength breaks in all layouts.
#' @param font_size Base figure font size.
#'
#' @return A ggplot object.
#' @noRd
transmission_filter_panel_plot <- function(
  snapshot,
  show_title = TRUE,
  panel_tag = NULL,
  compact_x = FALSE,
  font_size = 13
) {
  validate_transmission_applied_snapshot(snapshot)
  filter_data <- snapshot$filter

  ggplot2::ggplot(
    filter_data,
    ggplot2::aes(
      x = .data$wavelength_nm,
      y = .data$transmittance
    )
  ) +
    ggridges::geom_ridgeline_gradient(
      ggplot2::aes(
        y = 0,
        height = .data$transmittance,
        fill = .data$wavelength_nm
      ),
      scale = 1,
      colour = NA,
      alpha = 0.72
    ) +
    ggplot2::geom_line(colour = "#1b1b1b", linewidth = 0.9) +
    ggplot2::scale_fill_gradientn(
      colours = transmission_spectral_palette(),
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(380, 780),
      breaks = c(400, 500, 600, 700, 780),
      expand = ggplot2::expansion(add = c(4, 10))
    ) +
    transmission_fraction_y_scale() +
    ggplot2::labs(
      title = if (isTRUE(show_title))
        transmission_text("plot_filter_panel") else NULL,
      tag = panel_tag,
      x = transmission_text("plot_wavelength"),
      y = transmission_text("plot_transmittance")
    ) +
    transmission_plot_theme(font_size = font_size) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0),
      plot.tag = ggplot2::element_text(
        face = "bold",
        size = font_size * 0.87
      ),
      axis.text.x = ggplot2::element_text(size = font_size * 0.75),
      plot.margin = ggplot2::margin(5, 8, 5, 5)
    )
}

#' Save a Transmission result plot as a PNG
#'
#' @param snapshot Immutable applied transmission snapshot.
#' @param file Destination path supplied by Shiny.
#' @param show_transmittance_panel Whether to include panel B.
#' @param incident_fill Whether to show the lighter incident fill.
#' @param response_curves Individually selected action-spectrum overlays.
#' @param width Output width in inches.
#' @param height Output height in inches.
#' @param font_size Base figure font size.
#' @param max_irradiance Optional upper scale limit in mW/m2/nm.
#'
#' @return `file`, invisibly.
#' @noRd
write_transmission_result_plot <- function(
  snapshot,
  file,
  show_transmittance_panel = FALSE,
  incident_fill = TRUE,
  response_curves = character(),
  width = if (isTRUE(show_transmittance_panel)) 12 else 9,
  height = 5.8,
  font_size = 15,
  max_irradiance = NULL
) {
  validate_transmission_applied_snapshot(snapshot)
  plot <- transmission_spectral_comparison_plot(
    snapshot,
    show_transmittance_panel = show_transmittance_panel,
    incident_fill = incident_fill,
    response_curves = response_curves,
    panel_layout = "side",
    font_size = font_size,
    max_irradiance = max_irradiance
  )
  plot <- plot + transmission_plot_footnote(font_size = font_size)
  ggplot2::ggsave(
    filename = file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  invisible(file)
}

#' Save the completed transmittance curve as a PNG
#'
#' @param snapshot Immutable applied transmission snapshot.
#' @param file Destination path supplied by Shiny.
#'
#' @return `file`, invisibly.
#' @noRd
write_transmission_filter_plot <- function(
  snapshot,
  file,
  width = 9,
  height = 5.8,
  font_size = 15
) {
  validate_transmission_applied_snapshot(snapshot)
  plot <- transmission_filter_panel_plot(
    snapshot,
    show_title = TRUE,
    font_size = font_size
  ) +
    transmission_plot_footnote(font_size = font_size)
  ggplot2::ggsave(
    filename = file,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  invisible(file)
}

#' Save a styled Transmission `gt` table as a PNG
#'
#' @param table A styled `gt_tbl`.
#' @param file Destination path supplied by Shiny.
#'
#' @return `file`, invisibly.
#' @noRd
write_transmission_gt_png <- function(table, file) {
  stopifnot(inherits(table, "gt_tbl"))
  gt::gtsave(
    data = table,
    filename = file,
    expand = 8,
    zoom = 2
  )
  invisible(file)
}

#' Save an Analysis-style result figure and metric table as one PNG
#'
#' @param snapshot Applied transmission snapshot.
#' @param file Destination path.
#' @param metric_group One of `"d65"`, `"light"`, or `"balance"`.
#' @param show_transmittance_panel Include the narrower panel B.
#' @param incident_fill Show the lighter incident fill.
#' @param response_curves Individually selected action-spectrum overlays.
#' @param width Output width in inches.
#' @param height Plot-area height in inches. The table extends the final image.
#' @param font_size Base figure font size.
#' @param max_irradiance Optional upper scale limit in mW/m2/nm.
#'
#' @return `file`, invisibly.
#' @noRd
write_transmission_plot_table_png <- function(
  snapshot,
  file,
  metric_group = c("d65", "light", "balance"),
  show_transmittance_panel = FALSE,
  incident_fill = TRUE,
  response_curves = character(),
  width = if (isTRUE(show_transmittance_panel)) 12 else 9,
  height = 5.8,
  font_size = 15,
  max_irradiance = NULL
) {
  validate_transmission_applied_snapshot(snapshot)
  metric_group <- match.arg(metric_group)
  table_file <- tempfile(fileext = ".png")
  on.exit(unlink(table_file), add = TRUE)
  write_transmission_gt_png(
    transmission_metric_gt(snapshot, metric_group),
    table_file
  )
  table_image <- png::readPNG(table_file, native = TRUE)
  plot <- transmission_spectral_comparison_plot(
    snapshot = snapshot,
    show_transmittance_panel = show_transmittance_panel,
    incident_fill = incident_fill,
    response_curves = response_curves,
    panel_layout = "side",
    font_size = font_size,
    max_irradiance = max_irradiance
  )
  table_as_plot <- cowplot::ggdraw() +
    cowplot::draw_image(table_image, x = 0, y = 0, width = 1, height = 1)
  table_ratio <- max(0.55, dim(table_image)[[1L]] / dim(table_image)[[2L]])
  combined <- cowplot::plot_grid(
    plot,
    table_as_plot,
    ncol = 1,
    rel_heights = c(height, width * table_ratio)
  ) +
    transmission_plot_footnote(font_size = font_size)
  ggplot2::ggsave(
    filename = file,
    plot = combined,
    width = width,
    height = height + width * table_ratio,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  invisible(file)
}
