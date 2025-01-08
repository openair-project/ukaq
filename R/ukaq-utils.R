#' Check if tibble is installed. If so, format table as a tibble.
#' @param table Input table
#' @param .return Inherited from parent function
#' @noRd
tbl <- function(table, .return) {
  input <- .return %||% ifelse(rlang::is_installed("tibble"), "tbl", "df")
  input <- rlang::arg_match(input, c("tbl", "df"))

  if (input == "tbl") {
    table <- tibble::tibble(table)
  }

  return(table)
}

#' Convenient way to load an RData file into R
#' @param path URL to RData
#' @noRd
loadRData <- function(path) {
  # connect to the URL
  connection <- url(path)

  # disconnect from the connection
  on.exit(close.connection(connection))

  # load the .RData file
  x <- load(connection)

  table <- get(x)

  return(table)
}

#' @noRd
loadRDS <- function(x) {
  con <- url(x)
  on.exit(close.connection(con))
  readRDS(con)
}


#' Base R coalesce
#' @noRd
coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x
  },
  list(...))
}
