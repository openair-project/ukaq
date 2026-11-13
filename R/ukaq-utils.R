#' Check if tibble is installed. If so, format table as a tibble.
#' @param table Input table
#' @param .class Inherited from parent function
#' @noRd
tbl_out <- function(table, .class) {
  input <- .class %||% ifelse(rlang::is_installed("tibble"), "tbl", "df")
  input <- rlang::arg_match(input, c("tbl", "df"))

  if (input == "tbl") {
    table <- tibble::tibble(table)
  }

  return(table)
}

#' Convenient way to load an RData file into R
#' @param path URL to RData
#' @param object The specific measurement `data_type` of interest. If `NULL`,
#'   the file is expected to contain a single object, which is returned.
#' @noRd
loadRData <- function(path, object = NULL) {
  # nocov start
  # connect to the URL
  connection <- url(path)

  # disconnect from the connection
  on.exit(close.connection(connection))

  # load the .RData file into its own environment, so loaded objects can't
  # clash with local variables
  env <- new.env(parent = emptyenv())
  x <- load(connection, envir = env)

  # if no specific object requested, return the only object
  if (is.null(object)) {
    return(env[[x[1]]])
  }

  # Objects are named "{CODE}_{YEAR}" (hourly) and "{CODE}_{YEAR}_{suffix}"
  # (everything else)
  prefix <- x[which.min(nchar(x))]
  id <- substring(x, nchar(prefix) + 2L)
  object_suffixes <- c(
    "hourly" = "",
    "daily" = "daily_mean",
    "15_min" = "15min",
    "8_hour" = "8hour_mean",
    "24_hour" = "24hour_mean",
    "daily_max_8" = "daily_max_8hour"
  )

  obj <- x[id == object_suffixes[[object]]]
  if (length(obj) != 1L) {
    stop("No '", object, "' data found in ", path, ".")
  }
  table <- env[[obj]]

  # the "daily" object contains additional daily statistics (e.g.,
  # gravimetric PM) which should be merged with the daily means
  if (object == "daily" && "daily" %in% id) {
    daily2 <- env[[x[id == "daily"]]]
    table <- merge(
      table,
      daily2,
      by = intersect(c("date", "site", "code"), names(daily2)),
      all = TRUE
    )
  }

  table <-
    table[c(
      "date",
      "code",
      "site",
      names(table)[!names(table) %in% c("date", "code", "site")]
    )]

  # return
  return(table)
} # nocov end

#' @noRd
loadRDS <- function(x) {
  # nocov start
  con <- url(x)
  on.exit(close.connection(con))
  readRDS(con)
} # nocov end


#' Base R coalesce
#' @noRd
coalesce <- function(...) {
  Reduce(
    function(x, y) {
      i <- which(is.na(x))
      x[i] <- y[i]
      x
    },
    list(...)
  )
}

#' Replaces "ukaq" with valid source names
#' @noRd
match_source <- function(source, network_names) {
  if (any(source == "ukaq")) {
    source <- network_names
  } else {
    source <-
      rlang::arg_match(source, network_names, multiple = TRUE)
  }
  return(source)
}

#' Pair site codes with networks
#'
#' `source` is either length 1 (applied to every code) or the same length as
#' `code` (matched element-wise). Each element is either a network or "ukaq",
#' meaning the network should be worked out automatically.
#'
#' @return A `data.frame` with columns `code` and `source`.
#' @noRd
pair_code_source <- function(code, source, network_names) {
  source <- tolower(source)

  bad_source <- setdiff(source, c("ukaq", network_names))
  if (length(bad_source) > 0L) {
    stop(
      "`source` must only contain ",
      paste0("'", c("ukaq", network_names), "'", collapse = ", "),
      ", not ",
      paste0("'", bad_source, "'", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (length(source) != 1L && length(source) != length(code)) {
    stop(
      "`source` must be length 1 or the same length as `code` (",
      length(code),
      "), not ",
      length(source),
      ".\n",
      "Either provide one network (or 'ukaq') for every code, or provide ",
      "a network (or 'ukaq') for each individual code.",
      call. = FALSE
    )
  }

  pairs <- data.frame(
    code = code,
    source = rep_len(source, length(code)),
    stringsAsFactors = FALSE
  )

  pairs[!duplicated(tolower(paste(pairs$code, pairs$source, sep = "\r"))), ]
}

#' The networks needed to resolve a set of code/source pairs
#' @noRd
pair_networks <- function(pairs, network_names) {
  if (any(pairs$source == "ukaq")) {
    return(network_names)
  }
  network_names[network_names %in% pairs$source]
}

#' Resolve code/source pairs against the code/source combinations which are
#' actually available.
#'
#' Codes paired with "ukaq" are assigned the first network (in the order of
#' `network_names`) in which they are available. Codes which can't be found are
#' dropped with a warning (or an error, if none can be found).
#'
#' @param pairs Output of `pair_code_source()`.
#' @param available A `data.frame` with at least `code` and `source` columns.
#' @return A `data.frame` with columns `code` and `source`, using the codes as
#'   spelled in `available`.
#' @noRd
resolve_code_source <- function(pairs, available, network_names) {
  available <- unique(as.data.frame(available)[c("code", "source")])
  available_key <- tolower(paste(available$code, available$source, sep = "\r"))

  resolved <- data.frame(
    code = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
  missing <- character()
  ambiguous <- character()

  for (i in seq_len(nrow(pairs))) {
    code_i <- pairs$code[i]
    source_i <- pairs$source[i]

    if (source_i == "ukaq") {
      found_in <-
        network_names[
          tolower(paste(code_i, network_names, sep = "\r")) %in%
            available_key
        ]
      if (length(found_in) == 0L) {
        missing <- c(missing, code_i)
        next
      }
      if (length(found_in) > 1L) {
        ambiguous <- c(
          ambiguous,
          paste0(code_i, " (", paste(found_in, collapse = ", "), ")")
        )
      }
      source_i <- found_in[1]
    }

    id <- match(
      tolower(paste(code_i, source_i, sep = "\r")),
      available_key
    )

    if (is.na(id)) {
      missing <- c(missing, paste0(code_i, " (", source_i, ")"))
      next
    }

    resolved <- rbind(resolved, available[id, c("code", "source")])
  }

  if (nrow(resolved) == 0L) {
    stop(
      "None of the requested codes were found: ",
      paste(missing, collapse = ", "),
      ".\nSee import_ukaq_meta() for valid site codes and their networks.",
      call. = FALSE
    )
  }

  if (length(missing) > 0L) {
    warning(
      "The following codes were not found and have been skipped: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (length(ambiguous) > 0L) {
    warning(
      "The following codes were found in multiple networks: ",
      paste(ambiguous, collapse = "; "),
      ".\nThe first network listed has been used for each. To choose a ",
      "different network, pair each code with a network using `source`.",
      call. = FALSE
    )
  }

  rownames(resolved) <- NULL
  resolved
}

#' Filter data to resolved code/source pairs
#' @noRd
filter_code_source <- function(data, code, source, network_names) {
  pairs <- pair_code_source(code, source, network_names)
  resolved <- resolve_code_source(pairs, data, network_names)
  data_key <- tolower(paste(data$code, data$source, sep = "\r"))
  resolved_key <- tolower(paste(resolved$code, resolved$source, sep = "\r"))
  data[data_key %in% resolved_key, ]
}

#' Check for longer/wider and sub in for long/wide
#' @noRd
match_pivot <- function(pivot) {
  if (tolower(pivot) %in% c("wide", "wider")) {
    return("wide")
  } else if (tolower(pivot) %in% c("long", "longer")) {
    return("long")
  } else {
    stop("'pivot' should be one of 'wide' or 'long', not '", pivot, "'.")
  }
}

#' Reformats all factors columns to character ones
#' @noRd
factor_to_char <- function(data) {
  classes <- sapply(data, class)
  factor_classes <- classes[classes == "factor"]
  for (i in names(factor_classes)) {
    data[[i]] <- as.character(data[[i]])
  }
  return(data)
}

#' An alternative form of rbind which resolves different names
#' @param data A list of dataframes
#' @noRd
rbindNames <- function(data) {
  all_names <- lapply(data, names)

  data <- lapply(data, function(x) {
    newnames <- setdiff(unlist(all_names), names(x))
    for (i in newnames) {
      x[[i]] <- NA
    }
    x
  })

  data <- do.call(rbind, data)

  return(data)
}

#' Add Metadata cols
#' @noRd
append_metadata_cols <- function(data, source, metadata_columns) {
  meta <- import_ukaq_meta(source = source, by_pollutant = FALSE)
  meta <- meta[c(metadata_columns, "code", "source")]
  data_names <- names(data)
  data <- merge(data, meta, by = c("code", "source"))
  # merge() moves the 'by' columns to the front; restore the original order
  data <- data[c(data_names, metadata_columns)]
  return(data)
}

#' Functions to initialise progress bars
#' @noRd
pb_init <- function(name, x) {
  if (rlang::is_installed("cli")) {
    pb <- cli::cli_progress_bar(
      name = name,
      total = x,
      .envir = parent.frame(2)
    )
  } else {
    pb <- utils::txtProgressBar(title = name, label = name, min = 0, max = x)
  }
  return(pb)
}

#' @noRd
pb_close <- function(pb) {
  if (rlang::is_installed("cli")) {
    cli::cli_progress_done(id = pb)
  } else {
    close(pb)
  }
}

#' @noRd
pb_increment <- function(pb, x) {
  if (rlang::is_installed("cli")) {
    cli::cli_progress_update(id = pb, set = x)
  } else {
    utils::setTxtProgressBar(pb, x)
  }
}
