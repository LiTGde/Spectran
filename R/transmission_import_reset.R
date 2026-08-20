# Confirmed source-import history reset ---------------------------------

#' UI for confirmed source-import history resets
#'
#' @param id Shiny module identifier.
#'
#' @return Shiny UI tags.
#' @noRd
transmissionSourceImportUI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("status")),
    shinyjs::hidden(htmltools::tags$section(
      id = ns("confirmation"),
      class = paste(
        "alert alert-warning transmission-import-confirmation"
      ),
      role = "alertdialog",
      `aria-labelledby` = ns("confirmation_heading"),
      `aria-describedby` = ns("confirmation_message"),
      `aria-live` = "assertive",
      `aria-atomic` = "true",
      htmltools::tags$h4(
        id = ns("confirmation_heading"),
        transmission_text("import_confirm_heading")
      ),
      shiny::uiOutput(ns("confirmation_message")),
      htmltools::tags$div(
        class = "transmission-import-confirmation-actions",
        shiny::actionButton(
          ns("cancel_import"),
          label = transmission_text("import_cancel"),
          icon = shiny::icon("times")
        ),
        shiny::actionButton(
          ns("confirm_import"),
          label = transmission_text("import_confirm_action"),
          icon = shiny::icon("upload"),
          class = "btn-danger"
        )
      )
    ))
  )
}

#' Focus an HTML element by its validated ID
#'
#' @param element_id Complete, already-namespaced HTML identifier.
#'
#' @return `NULL`, invisibly.
#' @noRd
transmission_focus_element <- function(element_id) {
  element_id <- transmission_scalar_text(element_id, "element_id")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.:-]*$", element_id)) {
    stop("`element_id` contains unsupported characters.", call. = FALSE)
  }
  shinyjs::runjs(paste0(
    "var element = document.getElementById('",
    element_id,
    "'); if (element) { element.focus(); }"
  ))
  invisible(NULL)
}

#' Validate and freeze one requested source import
#'
#' @param request List with `source_id` and `source_name`.
#'
#' @return A normalized request list.
#' @noRd
as_transmission_import_request <- function(request) {
  if (!is.list(request)) {
    stop("A source import request must be a list.", call. = FALSE)
  }
  request$source_id <- transmission_scalar_text(
    request$source_id,
    "source_id"
  )
  request$source_name <- transmission_scalar_text(
    request$source_name,
    "source_name"
  )
  request
}

#' Server for confirmed source-import history resets
#'
#' @param id Shiny module identifier.
#' @param history Reactive session history.
#' @param perform_import Function that receives a frozen import request and
#'   performs the top-level active-spectrum mutation.
#' @param return_focus_id Complete HTML identifier of the initiating control.
#'
#' @return Request, confirm, and cancel functions plus state reactives.
#' @noRd
transmissionSourceImportServer <- function(
  id,
  history,
  perform_import,
  return_focus_id
) {
  if (!shiny::is.reactive(history)) {
    stop("`history` must be reactive.", call. = FALSE)
  }
  if (!is.function(perform_import)) {
    stop("`perform_import` must be a function.", call. = FALSE)
  }
  return_focus_id <- transmission_scalar_text(
    return_focus_id,
    "return_focus_id"
  )

  shiny::moduleServer(id, function(input, output, session) {
    pending <- shiny::reactiveVal(NULL)
    feedback <- shiny::reactiveVal(NULL)
    last_request_click <- shiny::reactiveVal(-Inf)
    last_request_keyboard <- shiny::reactiveVal(-Inf)
    last_confirm_click <- shiny::reactiveVal(-Inf)
    last_confirm_keyboard <- shiny::reactiveVal(-Inf)
    last_cancel_click <- shiny::reactiveVal(-Inf)
    last_cancel_keyboard <- shiny::reactiveVal(-Inf)

    activation_allowed <- function(trigger, last_click, last_keyboard) {
      trigger <- match.arg(trigger, c("click", "keyboard"))
      now <- unname(proc.time()[["elapsed"]])
      cross_mode_window_seconds <- 0.5
      if (
        trigger == "click" &&
          now - last_keyboard() < cross_mode_window_seconds
      ) {
        return(FALSE)
      }
      if (
        trigger == "keyboard" &&
          now - last_click() < cross_mode_window_seconds
      ) {
        return(FALSE)
      }
      if (trigger == "click") {
        last_click(now)
      } else {
        last_keyboard(now)
      }
      TRUE
    }

    focus_initiator <- function() {
      transmission_focus_element(return_focus_id)
    }

    close_confirmation <- function() {
      shinyjs::hide(session$ns("confirmation"), asis = TRUE)
      focus_initiator()
      invisible(NULL)
    }

    request_import <- function(
      request,
      trigger = c("click", "keyboard")
    ) {
      trigger <- match.arg(trigger)
      if (
        !activation_allowed(
          trigger,
          last_request_click,
          last_request_keyboard
        )
      ) {
        return(invisible(NULL))
      }
      request <- as_transmission_import_request(request)
      current_history <- history()
      if (!transmission_import_requires_confirmation(current_history)) {
        perform_import(request)
        feedback(list(
          state = "current",
          message = transmission_text("import_direct", request$source_name)
        ))
        return(invisible(NULL))
      }

      pending(list(
        request = request,
        promoted_count = transmission_import_promoted_count(current_history)
      ))
      feedback(list(
        state = "warning",
        message = transmission_text(
          "import_confirmation_required",
          request$source_name
        )
      ))
      shinyjs::show(session$ns("confirmation"), asis = TRUE)
      shinyjs::delay(
        0,
        transmission_focus_element(session$ns("cancel_import"))
      )
      invisible(NULL)
    }

    cancel_import <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (
        !activation_allowed(
          trigger,
          last_cancel_click,
          last_cancel_keyboard
        ) ||
          is.null(pending())
      ) {
        return(invisible(NULL))
      }
      source_name <- pending()$request$source_name
      pending(NULL)
      close_confirmation()
      feedback(list(
        state = "current",
        message = transmission_text("import_cancelled", source_name)
      ))
      invisible(NULL)
    }

    confirm_import <- function(trigger = c("click", "keyboard")) {
      trigger <- match.arg(trigger)
      if (
        !activation_allowed(
          trigger,
          last_confirm_click,
          last_confirm_keyboard
        ) ||
          is.null(pending())
      ) {
        return(invisible(NULL))
      }
      current <- pending()
      pending(NULL)
      close_confirmation()
      perform_import(current$request)
      completion_message <- if (current$promoted_count == 1L) {
        transmission_text(
          "import_completed_one",
          current$request$source_name
        )
      } else {
        transmission_text(
          "import_completed_many",
          current$request$source_name,
          current$promoted_count
        )
      }
      feedback(list(
        state = "current",
        message = completion_message
      ))
      invisible(NULL)
    }

    shiny::observeEvent(input$cancel_import, {
      cancel_import("click")
    })
    shiny::observeEvent(input$confirm_import, {
      confirm_import("click")
    })

    shinyjs::onevent(
      event = "keydown",
      id = "cancel_import",
      expr = function(event) {
        if (is_transmission_activation_key(event)) {
          cancel_import("keyboard")
        }
      },
      properties = c("key", "code", "repeat", "which")
    )
    shinyjs::onevent(
      event = "keydown",
      id = "confirm_import",
      expr = function(event) {
        if (is_transmission_activation_key(event)) {
          confirm_import("keyboard")
        }
      },
      properties = c("key", "code", "repeat", "which")
    )
    shinyjs::onevent(
      event = "keydown",
      id = "confirmation",
      expr = function(event) {
        if (is_transmission_escape_key(event)) {
          cancel_import("keyboard")
        }
      },
      properties = c("key", "code", "repeat", "which")
    )

    output$confirmation_message <- shiny::renderUI({
      current <- pending()
      shiny::req(current)
      confirmation_message <- if (current$promoted_count == 1L) {
        transmission_text(
          "import_confirm_message_one",
          current$request$source_name
        )
      } else {
        transmission_text(
          "import_confirm_message_many",
          current$request$source_name,
          current$promoted_count
        )
      }
      htmltools::tagList(
        htmltools::tags$p(confirmation_message),
        htmltools::tags$p(
          transmission_text("import_confirm_irreversible")
        )
      )
    })

    output$status <- shiny::renderUI({
      current <- feedback()
      if (is.null(current)) {
        return(NULL)
      }
      htmltools::tags$div(
        class = paste0(
          "transmission-source-import-feedback import-",
          current$state
        ),
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        current$message
      )
    })

    list(
      request = request_import,
      confirm = confirm_import,
      cancel = cancel_import,
      pending = shiny::reactive(pending()),
      feedback = shiny::reactive(feedback())
    )
  })
}
