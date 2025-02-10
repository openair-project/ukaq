
#' Import Monthly and Annual Summaries of UK Air Quality Data
#'
#' Imports either annual or monthly summaries matching UK AIR. Unlike the data
#' available in [import_ukaq_measurements()], this data is appended with a
#' monthly/annual data capture and additional relevant columns (e.g., the month
#' as a character for monthly data).
#'
#' @param code *Specific site codes to import.*
#'
#'    *default:* `NULL`
#'
#'   `code` expects a vector of character values. It defines the specific sites
#'   to import will filter the result for specific sites based on their site
#'   codes, available through [import_ukaq_meta()]. Note that a mismatch between
#'   `code` and `source` may result in no data being imported. If `NULL`, the
#'   default, all available data for the `year` and `source` will be returned.
#'
#' @param data_type *What type of summary should be returned?*
#'
#'   *default:* `"annual"`
#'
#'   [import_ukaq_summaries()] can return either `"annual"` (the default) or
#'   `"monthly"` summary data.
#'
#' @inheritParams import_ukaq_measurements
#'
#' @return a `data.frame`
#'
#' @author Jack Davison, David Carslaw
#'
#' @references With thanks to Trevor Davies and [Ricardo
#'   Plc](https://www.ricardo.com/en) for preparing and hosting the data
#'
#' @export
import_ukaq_summaries <-
  function(code = NULL,
           year,
           source = "aurn",
           data_type = "annual",
           pollutant = NULL,
           append_metadata = FALSE,
           metadata_columns = c("site_type", "latitude", "longitude"),
           pivot = "wide",
           progress = NA,
           ...,
           .class = NULL) {
    pivot <- match_pivot(pivot)
    metadata_columns <-
      rlang::arg_match(metadata_columns, metadata_column_names, multiple = TRUE)
    source <-
      match_source(source = source, network_names = ukaq_network_names)
    data_type <-
      rlang::arg_match(data_type, c("annual", "monthly"))

    # get grid of years & sources
    grid <-
      expand.grid(year = year,
                  source = source,
                  stringsAsFactors = FALSE)

    # import data
    data <- importSummaries(grid = grid,
                            data_type = data_type,
                            progress = progress)

    # format data
    data <- formatSummary(data = data,
                          code = code,
                          source = source,
                          data_type = data_type,
                          pollutant = pollutant,
                          append_metadata = append_metadata,
                          metadata_columns = metadata_columns,
                          pivot = pivot)

    # return
    return(tbl(data, .class))
  }

#' Format AQ Summary Data
#' @noRd
formatSummary <- function(data,
                          code,
                          source,
                          data_type,
                          pollutant,
                          append_metadata,
                          metadata_columns,
                          pivot){
  # drop UKA code
  names(data) <- tolower(names(data))
  names(data) <- sub("noxasno2", "nox", names(data))
  data$uka_code <- NULL

  # format factor cols as characters
  data <- factor_to_char(data)

  # add extra columns per summary type
  if (data_type == "annual") {
    data$date <- as.Date(paste0(data$year, "-01-01"), tz = "GMT")
    data <-
      data[, c("source", "date", setdiff(names(data), c("date", "source")))]
  }
  if (data_type == "monthly") {
    data$date <- as.Date(data$date, tz = "GMT")
    data$year <- as.integer(format(data$date, "%Y"))
    data$month <- as.integer(format(data$date, "%m"))
    data$month_label <- factor(format(data$date, "%b"), month.abb)
    data <-
      data[, c("source",
               "date",
               "year",
               "month",
               "month_label",
               setdiff(
                 names(data),
                 c("source", "date", "year", "month", "month_label")
               ))]
  }

  # filter for site
  if (!is.null(code)) {
    data <- data[tolower(data$code) %in% tolower(code),]
  }

  # pivot & deal with pollutants
  # (poll filtering method varies with format)
  if (pivot == "long") {
    data <- pivot_summary_longer(data)
    if (!is.null(pollutant)) {
      data <- data[tolower(data$pollutant) %in% tolower(pollutant),]
    }
  }

  if (pivot == "wide") {
    if (!is.null(pollutant)) {
      if (data_type == "monthly") {
        non_pollutant_cols <-
          c("source",
            "date",
            "year",
            "month",
            "month_label",
            "code",
            "site")
      } else {
        non_pollutant_cols <- c("source", "date", "year", "code", "site")
      }

      pollutant_cols <-
        names(data)[grepl(paste(pollutant, collapse = "|"), names(data))]

      data <- data[, c(non_pollutant_cols, pollutant_cols)]
    }

    names(data) <- gsub(".mean", "", names(data))
  }

  # append metadata, if requested
  if (append_metadata) {
    data <-
      append_metadata_cols(data, source = source, metadata_columns = metadata_columns)
  }

  return(data)
}

#' Format a summary URL
#' @noRd
summary_url <-
  function(source, stat = c("annual", "monthly"), year) {
    stat <- match.arg(stat)

    url <- switch(
      source,
      "aurn" = "https://uk-air.defra.gov.uk/openair/R_data/summary_STAT_AURN_YEAR.rds",
      "aqe" = "https://airqualityengland.co.uk/assets/openair/R_data/summary_STAT_AQE_YEAR.rds",
      "saqn" = "https://www.scottishairquality.scot/openair/R_data/summary_STAT_SCOT_YEAR.rds",
      "waqn" = "https://airquality.gov.wales/sites/default/files/openair/R_data/summary_STAT_WAQ_YEAR.rds",
      "niaqn" = "https://www.airqualityni.co.uk/openair/R_data/summary_STAT_NI_YEAR.rds",
      "lmam" = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/summary_STAT_LMAM_YEAR.rds"
    )

    url <- gsub("YEAR", year, url)

    url <- gsub("STAT", stat, url)

    return(url)
  }

#' Import annual/monhtly summaries
#' @noRd
importSummaries <- function(grid, data_type, progress) {
  # deal w/ progress opt if NA
  if (is.na(progress)) {
    progress <- FALSE
    if (nrow(grid) > 1L && rlang::is_interactive()) {
      progress <- TRUE
    }
  }

  if (progress) {
    pb <- utils::txtProgressBar(
      min = 0,
      max = nrow(grid),
      initial = 0,
      style = 3L
    )
    on.exit(close(pb))
  }

  data <-
    lapply(1:nrow(grid), function(x) {
      df <- tryCatch(
        suppressWarnings(loadRDS(
          summary_url(
            source = grid$source[x],
            year = grid$year[x],
            stat = data_type
          )
        )),
        error = function(e)
          NULL
      )
      if (!is.null(df)) {
        df$source <- grid$source[x]
        df <- df[, c("source", names(df)[names(df) != "source"])]
      }
      if (progress) {
        utils::setTxtProgressBar(pb, value = x)
      }
      df
    })

  all_names <- lapply(data, names)

  data <- lapply(data, function(x) {
    newnames <- setdiff(unlist(all_names), names(x))
    for (i in newnames) {
      x[[i]] <- NA
    }
    x
  })

  data <- do.call(rbind, data)

  data
}


#' Pivot monthly/annual summaries into a long format
#' @noRd
pivot_summary_longer <- function(data) {
  data <- as.data.frame(data)

  names(data) <-
    sub("2.5",
        "25",
        names(data))

  names(data) <-
    sub("o3.mean.daily.max.8hour",
        "o3meandailymax8hour.mean",
        names(data))

  names(data) <-
    sub("o3.aot40v",
        "o3aot40v.mean",
        names(data))

  names(data) <-
    sub("o3.aot40f",
        "o3aot40f.mean",
        names(data))

  names(data) <-
    sub("o3.summer",
        "o3summer",
        names(data))

  names(data)[names(data) == "somo35"] <- "somo35.mean"

  pivotvars <-
    c(names(data)[endsWith(names(data), ".mean")],
      names(data)[endsWith(names(data), ".capture")])

  idvars <-
    setdiff(names(data), pivotvars)

  long <-
    stats::reshape(
      data,
      varying = pivotvars,
      v.names = "value",
      timevar = "variable",
      idvar = c("source", "date", "code", "site"),
      times = pivotvars,
      direction = "long"
    )

  rownames(long) <- NULL

  long$pollutant <-
    sapply(strsplit(long$variable, split = "\\."), function(x)
      x[1])

  long$stat <-
    sapply(strsplit(long$variable, split = "\\."), function(x)
      x[2])

  long$variable <- NULL

  wide <- stats::reshape(
    long,
    timevar = "stat",
    idvar = c(idvars, "pollutant"),
    direction = "wide"
  )

  names(wide) <- sub("value\\.", "", names(wide))

  wide$pollutant <- sub("25", "2.5", wide$pollutant)

  return(wide)
}
