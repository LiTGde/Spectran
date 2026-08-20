# Shared active-spectrum state -------------------------------------------

#' Initialize fields used by Spectran's shared active-spectrum state
#'
#' @param Spectrum A `reactiveValues` object shared by import and analysis.
#'
#' @return `Spectrum`, invisibly.
#' @noRd
initialize_spectran_spectrum_state <- function(Spectrum) {
  defaults <- list(
    Spectrum = NULL,
    Spectrum_raw = NULL,
    Name = NULL,
    Origin = NULL,
    Destination = NULL,
    Illu = NULL,
    Analysis = 0L,
    revision = 0L,
    change_type = NULL,
    node_id = NULL,
    provenance = list(),
    committed_state = NULL,
    import_guard = NULL,
    import_attempt = 0L
  )
  for (name in names(defaults)) {
    current <- shiny::isolate(Spectrum[[name]])
    if (is.null(current)) {
      Spectrum[[name]] <- defaults[[name]]
    }
  }
  invisible(Spectrum)
}

#' Signal one explicit source-import attempt
#'
#' The attempt counter is separate from the raw spectrum so choosing the same
#' source again still crosses the import confirmation boundary.
#'
#' @param Spectrum Shared `reactiveValues` object.
#'
#' @return The new attempt number, invisibly.
#' @noRd
signal_spectran_import_attempt <- function(Spectrum) {
  initialize_spectran_spectrum_state(Spectrum)
  current <- shiny::isolate(Spectrum$import_attempt)
  if (
    is.null(current) ||
      length(current) != 1L ||
      !is.finite(current) ||
      current < 0L
  ) {
    current <- 0L
  }
  next_attempt <- as.integer(current) + 1L
  Spectrum$import_attempt <- next_attempt
  invisible(next_attempt)
}

#' Activate a spectrum through the single shared-state boundary
#'
#' @param Spectrum Shared `reactiveValues` object.
#' @param spectrum Canonical 401-row Spectran spectrum.
#' @param name User-facing spectrum name.
#' @param origin Source origin label.
#' @param change_type One of `import`, `promotion`, or `restore`.
#' @param node_id Session history node identifier.
#' @param provenance Activation provenance.
#' @param destination Existing Spectran destination label.
#' @param revision Optional explicit revision number.
#'
#' @return The new revision number, invisibly.
#' @noRd
activate_spectran_spectrum <- function(
  Spectrum,
  spectrum,
  name,
  origin,
  change_type = c("import", "promotion", "restore"),
  node_id,
  provenance = list(),
  destination = lang$ui(69),
  revision = NULL
) {
  change_type <- match.arg(change_type)
  spectrum <- as_visible_spectrum(spectrum, "spectrum")
  name <- transmission_scalar_text(name, "name")
  origin <- transmission_scalar_text(origin, "origin")
  node_id <- transmission_scalar_text(node_id, "node_id")
  if (!is.list(provenance)) {
    stop("`provenance` must be a list.", call. = FALSE)
  }
  initialize_spectran_spectrum_state(Spectrum)

  current_revision <- shiny::isolate(Spectrum$revision)
  if (
    is.null(current_revision) ||
      length(current_revision) != 1L ||
      !is.finite(current_revision)
  ) {
    current_revision <- 0L
  }
  if (is.null(revision)) {
    revision <- as.integer(current_revision) + 1L
  } else {
    revision <- as.integer(revision)
  }
  if (length(revision) != 1L || is.na(revision) || revision < 1L) {
    stop("`revision` must be a positive integer.", call. = FALSE)
  }

  current_analysis <- shiny::isolate(Spectrum$Analysis)
  if (
    is.null(current_analysis) ||
      length(current_analysis) != 1L ||
      !is.finite(current_analysis)
  ) {
    current_analysis <- 0L
  }

  Spectrum$Name <- name
  Spectrum$Origin <- origin
  Spectrum$Destination <- destination
  Spectrum$Illu <- Calc_lux(
    spectrum$Bestrahlungsstaerke,
    Specs$AS_wide,
    Specs$Efficacy
  )
  Spectrum$revision <- revision
  Spectrum$change_type <- change_type
  Spectrum$node_id <- node_id
  Spectrum$provenance <- provenance
  Spectrum$Analysis <- as.integer(current_analysis) + 1L
  Spectrum$committed_state <- list(
    Spectrum = spectrum,
    Name = name,
    Origin = origin,
    Destination = destination,
    Illu = Spectrum$Illu,
    Analysis = Spectrum$Analysis,
    revision = revision,
    change_type = change_type,
    node_id = node_id,
    provenance = provenance
  )
  Spectrum$Spectrum <- spectrum

  invisible(revision)
}

#' Restore the last centrally activated state while an import awaits consent
#'
#' @param Spectrum Shared `reactiveValues` object.
#'
#' @return Whether a committed state was restored, invisibly.
#' @noRd
restore_spectran_committed_state <- function(Spectrum) {
  committed <- shiny::isolate(Spectrum$committed_state)
  if (!is.list(committed) || is.null(committed$Spectrum)) {
    return(invisible(FALSE))
  }
  fields <- setdiff(names(committed), "Spectrum")
  for (field in fields) {
    Spectrum[[field]] <- committed[[field]]
  }
  if (!identical(shiny::isolate(Spectrum$Spectrum), committed$Spectrum)) {
    Spectrum$Spectrum <- committed$Spectrum
  }
  invisible(TRUE)
}

#' Adapt Spectran's shared state to the transmission module contract
#'
#' @param Spectrum Shared `reactiveValues` object.
#'
#' @return A `transmission_active_spectrum`, or `NULL` before import.
#' @noRd
spectran_transmission_active_state <- function(Spectrum) {
  if (is.null(Spectrum$Spectrum)) {
    return(NULL)
  }
  revision <- Spectrum$revision
  if (is.null(revision) || !is.finite(revision) || revision < 1L) {
    revision <- 1L
  }
  change_type <- Spectrum$change_type
  if (
    is.null(change_type) ||
      !change_type %in%
        c(
          "import",
          "promotion",
          "restore"
        )
  ) {
    change_type <- "import"
  }
  node_id <- Spectrum$node_id
  if (is.null(node_id) || !nzchar(node_id)) {
    node_id <- "node-1"
  }
  new_transmission_active_spectrum(
    spectrum = Spectrum$Spectrum,
    name = Spectrum$Name,
    origin = Spectrum$Origin,
    revision = revision,
    change_type = change_type,
    node_id = node_id
  )
}

#' Apply a transmission activation event to Spectran's shared state
#'
#' @param Spectrum Shared `reactiveValues` object.
#' @param event Valid promotion or restore event.
#'
#' @return The new revision number, invisibly.
#' @noRd
activate_spectran_transmission_event <- function(Spectrum, event) {
  if (!inherits(event, "transmission_activation_event")) {
    stop("`event` must be a transmission activation event.", call. = FALSE)
  }
  event <- new_transmission_activation_event(
    action_sequence = event$action_sequence,
    change_type = event$change_type,
    node_id = event$node_id,
    parent_id = event$parent_id,
    spectrum = event$spectrum,
    name = event$name,
    provenance = event$provenance
  )
  activate_spectran_spectrum(
    Spectrum = Spectrum,
    spectrum = event$spectrum,
    name = event$name,
    origin = "Transmission",
    change_type = event$change_type,
    node_id = event$node_id,
    provenance = event$provenance,
    destination = lang$ui(69)
  )
}
