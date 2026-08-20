# Session-local transmission history -------------------------------------

#' Validate non-empty scalar text
#'
#' @param value Value to validate.
#' @param arg Argument name for errors.
#'
#' @return Trimmed text.
#' @noRd
transmission_scalar_text <- function(value, arg) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    stop(paste0("`", arg, "` must be non-empty text."), call. = FALSE)
  }
  trimws(value)
}

#' Validate a session-local positive integer
#'
#' @param value Value to validate.
#' @param arg Argument name for errors.
#' @param allow_zero Whether zero is permitted.
#'
#' @return An integer.
#' @noRd
transmission_session_integer <- function(value, arg, allow_zero = FALSE) {
  minimum <- if (isTRUE(allow_zero)) 0L else 1L
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value < minimum ||
      value != as.integer(value)
  ) {
    qualifier <- if (isTRUE(allow_zero)) "non-negative" else "positive"
    stop(
      paste0("`", arg, "` must be one ", qualifier, " integer."),
      call. = FALSE
    )
  }
  as.integer(value)
}

#' Create Spectran's active-spectrum adapter
#'
#' @param spectrum A 401-row Spectran visible spectrum.
#' @param name User-facing spectrum name.
#' @param origin Source origin.
#' @param revision Session-local activation revision.
#' @param change_type One of `import`, `promotion`, or `restore`.
#' @param node_id Active history-node identifier.
#'
#' @return A `transmission_active_spectrum` list.
#' @noRd
new_transmission_active_spectrum <- function(
  spectrum,
  name,
  origin,
  revision,
  change_type = c("import", "promotion", "restore"),
  node_id
) {
  change_type <- match.arg(change_type)
  structure(
    list(
      spectrum = as_visible_spectrum(spectrum, arg = "spectrum"),
      name = transmission_scalar_text(name, "name"),
      origin = transmission_scalar_text(origin, "origin"),
      revision = transmission_session_integer(
        revision,
        "revision",
        allow_zero = TRUE
      ),
      change_type = change_type,
      node_id = transmission_scalar_text(node_id, "node_id")
    ),
    class = c("transmission_active_spectrum", "list")
  )
}

#' Validate an active-spectrum adapter
#'
#' @param active_state Candidate adapter.
#'
#' @return A validated `transmission_active_spectrum`.
#' @noRd
as_transmission_active_spectrum <- function(active_state) {
  required <- c(
    "spectrum",
    "name",
    "origin",
    "revision",
    "change_type",
    "node_id"
  )
  if (!is.list(active_state) || !all(required %in% names(active_state))) {
    stop(
      paste0(
        "`active_state` must contain: ",
        paste(required, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  new_transmission_active_spectrum(
    spectrum = active_state$spectrum,
    name = active_state$name,
    origin = active_state$origin,
    revision = active_state$revision,
    change_type = active_state$change_type,
    node_id = active_state$node_id
  )
}

#' Create an explicit active-spectrum event payload
#'
#' @param action_sequence Session-local event sequence.
#' @param change_type One of `promotion` or `restore`.
#' @param node_id Target history-node identifier.
#' @param parent_id Parent node, or `NA_character_` for a root.
#' @param spectrum Target active spectrum.
#' @param name Target active-spectrum name.
#' @param provenance Provenance metadata.
#'
#' @return A `transmission_activation_event` list.
#' @noRd
new_transmission_activation_event <- function(
  action_sequence,
  change_type = c("promotion", "restore"),
  node_id,
  parent_id,
  spectrum,
  name,
  provenance = list()
) {
  change_type <- match.arg(change_type)
  if (!is.list(provenance)) {
    stop("`provenance` must be a list.", call. = FALSE)
  }
  if (
    !is.character(parent_id) ||
      length(parent_id) != 1L ||
      (!is.na(parent_id) && !nzchar(trimws(parent_id)))
  ) {
    stop("`parent_id` must be one node identifier or `NA`.", call. = FALSE)
  }
  structure(
    list(
      action_sequence = transmission_session_integer(
        action_sequence,
        "action_sequence"
      ),
      change_type = change_type,
      node_id = transmission_scalar_text(node_id, "node_id"),
      parent_id = if (is.na(parent_id)) NA_character_ else trimws(parent_id),
      spectrum = as_visible_spectrum(spectrum, arg = "spectrum"),
      name = transmission_scalar_text(name, "name"),
      provenance = provenance
    ),
    class = c("transmission_activation_event", "list")
  )
}

#' Convert an activation event into an active-spectrum adapter
#'
#' @param event A transmission activation event.
#' @param revision New active-spectrum revision.
#'
#' @return A `transmission_active_spectrum`.
#' @noRd
transmission_active_spectrum_from_event <- function(event, revision) {
  if (!inherits(event, "transmission_activation_event")) {
    stop(
      "`event` must be a transmission activation event.",
      call. = FALSE
    )
  }
  origin <- event$provenance$origin
  if (is.null(origin) || !is.character(origin) || length(origin) != 1L) {
    origin <- if (identical(event$change_type, "promotion")) {
      "Transmission"
    } else {
      "Restored transmission history"
    }
  }
  new_transmission_active_spectrum(
    spectrum = event$spectrum,
    name = event$name,
    origin = origin,
    revision = revision,
    change_type = event$change_type,
    node_id = event$node_id
  )
}

#' Construct one history node
#'
#' @param sequence_id Session-local node sequence.
#' @param node_id Node identifier.
#' @param parent_id Parent node or `NA_character_`.
#' @param spectrum Spectrum archived at the node.
#' @param applied_snapshot Applied snapshot for promoted nodes.
#' @param provenance Node provenance.
#' @param name User-facing spectrum name.
#' @param change_type Node creation type.
#' @param active Whether this is the active node.
#'
#' @return A history-node list.
#' @noRd
new_transmission_history_node <- function(
  sequence_id,
  node_id,
  parent_id,
  spectrum,
  applied_snapshot,
  provenance,
  name,
  change_type = c("import", "promotion"),
  active = FALSE
) {
  change_type <- match.arg(change_type)
  if (!is.list(provenance)) {
    stop("`provenance` must be a list.", call. = FALSE)
  }
  if (
    !is.character(parent_id) ||
      length(parent_id) != 1L ||
      (!is.na(parent_id) && !nzchar(trimws(parent_id)))
  ) {
    stop("`parent_id` must be one node identifier or `NA`.", call. = FALSE)
  }
  list(
    sequence_id = transmission_session_integer(sequence_id, "sequence_id"),
    node_id = transmission_scalar_text(node_id, "node_id"),
    parent_id = if (is.na(parent_id)) NA_character_ else trimws(parent_id),
    spectrum = as_visible_spectrum(spectrum, arg = "spectrum"),
    applied_snapshot = applied_snapshot,
    provenance = provenance,
    name = transmission_scalar_text(name, "name"),
    change_type = change_type,
    active = isTRUE(active)
  )
}

#' Start a new history tree from an imported source
#'
#' @param active_state Active-spectrum adapter for the imported source.
#' @param provenance Root provenance.
#'
#' @return A `transmission_history` list.
#' @noRd
new_transmission_history <- function(active_state, provenance = list()) {
  active_state <- as_transmission_active_spectrum(active_state)
  if (!identical(active_state$change_type, "import")) {
    stop(
      "A new transmission history must start from an imported source.",
      call. = FALSE
    )
  }
  root_provenance <- c(
    list(
      origin = active_state$origin,
      source_revision = active_state$revision
    ),
    provenance
  )
  root <- new_transmission_history_node(
    sequence_id = 1L,
    node_id = active_state$node_id,
    parent_id = NA_character_,
    spectrum = active_state$spectrum,
    applied_snapshot = NULL,
    provenance = root_provenance,
    name = active_state$name,
    change_type = "import",
    active = TRUE
  )
  nodes <- list(root)
  names(nodes) <- root$node_id
  structure(
    list(
      nodes = nodes,
      active_node_id = root$node_id,
      next_sequence = 2L
    ),
    class = c("transmission_history", "list")
  )
}

#' Validate a transmission history tree
#'
#' @param history Candidate history.
#'
#' @return The history, invisibly.
#' @noRd
validate_transmission_history <- function(history) {
  if (
    !inherits(history, "transmission_history") ||
      !is.list(history$nodes) ||
      length(history$nodes) == 0L ||
      is.null(history$active_node_id) ||
      !history$active_node_id %in% names(history$nodes)
  ) {
    stop("`history` must be a valid transmission history.", call. = FALSE)
  }
  active <- vapply(
    history$nodes,
    function(node) isTRUE(node$active),
    logical(1)
  )
  if (sum(active) != 1L || !isTRUE(active[[history$active_node_id]])) {
    stop(
      "A transmission history must contain exactly one active node.",
      call. = FALSE
    )
  }
  invisible(history)
}

#' Determine whether a source import needs destructive-reset confirmation
#'
#' A root-only history contains no promoted work and may be replaced directly.
#' Any promoted node, including one derived from the same source that is about
#' to be imported, requires an explicit confirmation.
#'
#' @param history A transmission history or `NULL` before history starts.
#'
#' @return A single logical value.
#' @noRd
transmission_import_requires_confirmation <- function(history) {
  if (is.null(history)) {
    return(FALSE)
  }
  validate_transmission_history(history)
  length(history$nodes) > 1L
}

#' Count promoted nodes that a source import would remove
#'
#' @param history A transmission history or `NULL`.
#'
#' @return A non-negative integer.
#' @noRd
transmission_import_promoted_count <- function(history) {
  if (is.null(history)) {
    return(0L)
  }
  validate_transmission_history(history)
  as.integer(max(0L, length(history$nodes) - 1L))
}

#' Get one history node
#'
#' @param history Transmission history.
#' @param node_id Node identifier.
#'
#' @return A history-node list.
#' @noRd
transmission_history_node <- function(history, node_id) {
  validate_transmission_history(history)
  node_id <- transmission_scalar_text(node_id, "node_id")
  node <- history$nodes[[node_id]]
  if (is.null(node)) {
    stop(paste0("History node `", node_id, "` does not exist."), call. = FALSE)
  }
  node
}

#' Add a promoted spectrum as a child of the active node
#'
#' @param history Transmission history.
#' @param snapshot Current immutable applied snapshot.
#' @param name User-edited promoted spectrum name.
#' @param provenance Additional promotion provenance.
#'
#' @return A list containing updated `history` and the new `node`.
#' @noRd
transmission_history_promote <- function(
  history,
  snapshot,
  name,
  provenance = list()
) {
  validate_transmission_history(history)
  if (!inherits(snapshot, "transmission_applied_snapshot")) {
    stop(
      "`snapshot` must be a transmission applied snapshot.",
      call. = FALSE
    )
  }
  if (!is.list(provenance)) {
    stop("`provenance` must be a list.", call. = FALSE)
  }
  sequence_id <- transmission_session_integer(
    history$next_sequence,
    "history$next_sequence"
  )
  node_id <- paste0("node-", sequence_id)
  while (node_id %in% names(history$nodes)) {
    sequence_id <- sequence_id + 1L
    node_id <- paste0("node-", sequence_id)
  }
  parent_id <- history$active_node_id
  parent <- transmission_history_node(history, parent_id)
  node_provenance <- c(
    list(
      origin = "Transmission",
      parent_name = parent$name,
      filter_name = snapshot$metadata$filter_name,
      apply_sequence = snapshot$apply_sequence
    ),
    provenance
  )
  node <- new_transmission_history_node(
    sequence_id = sequence_id,
    node_id = node_id,
    parent_id = parent_id,
    spectrum = snapshot$transmitted_spectrum,
    applied_snapshot = snapshot,
    provenance = node_provenance,
    name = name,
    change_type = "promotion",
    active = TRUE
  )
  history$nodes <- lapply(history$nodes, function(existing) {
    existing$active <- FALSE
    existing
  })
  history$nodes[[node_id]] <- node
  history$active_node_id <- node_id
  history$next_sequence <- sequence_id + 1L
  validate_transmission_history(history)
  list(history = history, node = node)
}

#' Restore an existing node without deleting branches
#'
#' @param history Transmission history.
#' @param node_id Target node identifier.
#'
#' @return A list containing updated `history` and the restored `node`.
#' @noRd
transmission_history_restore <- function(history, node_id) {
  target <- transmission_history_node(history, node_id)
  history$nodes <- lapply(history$nodes, function(node) {
    node$active <- identical(node$node_id, target$node_id)
    node
  })
  history$active_node_id <- target$node_id
  validate_transmission_history(history)
  list(history = history, node = history$nodes[[target$node_id]])
}

#' Flatten history nodes for display and export
#'
#' @param history Transmission history.
#'
#' @return A tibble with one row per node.
#' @noRd
transmission_history_table <- function(history) {
  validate_transmission_history(history)
  rows <- lapply(history$nodes, function(node) {
    snapshot <- node$applied_snapshot
    tibble::tibble(
      sequence_id = node$sequence_id,
      node_id = node$node_id,
      parent_id = node$parent_id,
      name = node$name,
      change_type = node$change_type,
      active = node$active,
      filter_name = if (is.null(snapshot)) {
        NA_character_
      } else {
        snapshot$metadata$filter_name
      },
      incident_name = if (is.null(snapshot)) {
        NA_character_
      } else {
        snapshot$incident_name
      },
      apply_sequence = if (is.null(snapshot)) {
        NA_integer_
      } else {
        snapshot$apply_sequence
      }
    )
  })
  result <- do.call(rbind, rows)
  result[order(result$sequence_id), , drop = FALSE]
}

#' Stack every archived node spectrum for audit export
#'
#' @param history Transmission history.
#'
#' @return A long-form tibble.
#' @noRd
transmission_history_spectra <- function(history) {
  validate_transmission_history(history)
  rows <- lapply(history$nodes, function(node) {
    tibble::tibble(
      sequence_id = node$sequence_id,
      node_id = node$node_id,
      parent_id = node$parent_id,
      name = node$name,
      active = node$active,
      wavelength_nm = node$spectrum$Wellenlaenge,
      spectral_irradiance_w_m2_nm = node$spectrum$Bestrahlungsstaerke
    )
  })
  do.call(rbind, rows)
}

#' Create labelled history choices
#'
#' @param history Transmission history.
#'
#' @return A named character vector of node identifiers.
#' @noRd
transmission_history_choices <- function(history) {
  table <- transmission_history_table(history)
  labels <- paste0(
    table$sequence_id,
    ". ",
    table$name,
    ifelse(
      table$active,
      paste0(" ", transmission_text("history_active_suffix")),
      ""
    )
  )
  stats::setNames(table$node_id, labels)
}
