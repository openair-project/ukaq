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
#' @param object The specific object of interest, if multiple exist
#' @noRd
loadRData <- function(path, object) {
  # nocov start
  # connect to the URL
  connection <- url(path)

  # disconnect from the connection
  on.exit(close.connection(connection))

  # load the .RData file
  x <- load(connection)

  # if only only object, just return it
  if (length(x) == 1L) {
    table <- get(x)
    return(table)
  }

  # if multiple objects, get the one they want
  id <- x
  id[1] <- paste0(x[1], "_hourly")
  id <- sub("^[^_]+_[^_]+_(\\.*)", "\\1", id)
  if (object != "daily") {
    obj <- x[which(id == object)]
    table <- get(obj)
  } else {
    table <- get(x[which(id == "daily_mean")])
    if (exists("daily")) {
      daily2 <- get(x[which(id == "daily")])
      table <- merge(table, daily2, by = c("date", "site", "code"))
    }
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
  data <- merge(data, meta, by = c("code", "source"))
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
