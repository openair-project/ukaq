#' Import Continuous UK Air Quality Monitoring Data
#'
#' Imports continuous monitoring data from UK air quality networks. This
#' function defaults to importing hourly data, but has options to instead return
#' other common time periods.
#'
#' @param code *Specific site codes to import.*
#'
#'    **required**
#'
#'   `code` expects a vector of character values. It defines the specific sites
#'   to import will filter the result for specific sites based on their site
#'   codes, available through [import_ukaq_meta()]. Note that a mismatch between
#'   `code` and `source` may result in no data being imported.
#'
#' @param year *A year, or range of years, from which to import data.*
#'
#'    **required**
#'
#'   `year` expects a vector of integer values. It can be a single year
#'   (`2020`), a selection of specific years (c(`2020, 2021, 2022`)), or a range
#'   of years (`2020:2025`).
#'
#' @param source *One or more UK Monitoring networks from which to import data.*
#'
#'   *default:* `"ukaq"`
#'
#'   The default, `"ukaq"`, will look for sites from any of the available
#'   networks. Any combination of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`,
#'   `"niaqn"` or `"lmam"` will *only* import data from those specific
#'   monitoring networks. Note that a mismatch between `code` and `source` may
#'   result in no data being imported.
#'
#' @param pollutant *One or more pollutants for which to import data.*
#'
#'  *default:* `NULL`
#'
#'   By default, all available pollutants are imported. `pollutant` allows any
#'   specific combination of the pollutants to be returned instead.
#'
#' @param append_meteorology *Append modelled meteorology to the dataframe?*
#'
#'  *default:* `TRUE`
#'
#'   Certain networks such as the AURN are accompanied by modelled
#'   meteorological data obtained from the WRF model (See
#'   <https://uk-air.defra.gov.uk/research/air-quality-modelling?view=modelling>).
#'   Setting `append_meteorology = FALSE` will remove this, which may be useful
#'   if joining measured meteorological data sourced from elsewhere.
#'
#' @param append_quality_flag *Append ratification indicators to the dataframe?*
#'
#'  *default:* `FALSE`
#'
#'   If `TRUE`, additional `_qc` columns will be appended to the data which are
#'   `TRUE` if the data has been ratified and `FALSE` if it has not been.
#'   Specific ratification dates can be obtained from [import_ukaq_meta()].
#'
#' @param append_metadata *Append site metadata to the dataframe?*
#'
#'  *default:* `FALSE`
#'
#'   When `TRUE`, the resulting `data.frame` will have site metadata appended.
#'   The specific columns are selected using `metadata_columns`.
#'
#' @param metadata_columns *Specific metadata columns to append to the
#'   data.frame.*
#'
#'  *default:* `c("site_type", "latitude", "longitude")`
#'
#'   When `append_metadata = TRUE`, the columns selected here will be appended
#'   to the data. Columns names should match those in [import_ukaq_meta()],
#'   excluding pollutant-specific columns (e.g., ratification date).
#'
#' @param pivot *Should the dataframe be 'wide' or 'long'?*
#'
#'  *default:* `"wide"`
#'
#'   There are two main ways to store air quality data; "long" (with 'pollutant'
#'   and 'value' columns) or "wide" (with each pollutant value being stored in
#'   its own column). `pivot` allows users to define which format they would
#'   prefer their data in.
#'
#' @param ... *Not used.*
#'
#' @param .class *Signifier for the dataframe class.*
#'
#'    *default:* `NULL`
#'
#'   [ukaq][ukaq-package] functions, by default, will return `tbl_df`s if the
#'   [tibble][tibble::tibble-package] package is installed, but will otherwise
#'   return `data.frame`s. `.class` can override this behaviour, and takes
#'   either `"tbl"` or `"df"`, which sets the return class to be `tbl_df` or
#'   `data.frame` respectively.
#'
#' @param data_type *What type of summary should be returned?*
#'
#'   *default:* `"hourly"`
#'
#'   [import_ukaq_measurements()] can return many different continuous
#'   measurement types. These are as follows:
#'
#'   - `"hourly"`: Hourly data (the default).
#'   - `"daily"`: Daily average data.
#'   - `"15_min"`: 15-minute average SO2 concentrations.
#'   - `"8_hour"`: 8-hour rolling mean concentrations for O3 and CO.
#'   - `"24_hour"`: 24-hour rolling mean concentrations for particulates.
#'   - `"daily_max_8"`: Maximum daily rolling 8-hour maximum for O3 and CO.
#'
#' @param progress *Show a progress bar?*
#'
#'   *default:* `NA`
#'
#'   When `TRUE`, this function will print a progress bar to track individual
#'   files being imported. If `FALSE` this is supressed. If `NA`, the default,
#'   the function will work out if a progress bar would be useful (i.e., if the
#'   function is being run in an interactive session and if more than one remote
#'   file is being accessed).
#'
#' @return a `data.frame`
#'
#' @author Jack Davison, David Carslaw
#'
#' @references With thanks to Trevor Davies and [Ricardo
#'   Plc](https://www.ricardo.com/en) for preparing and hosting the data
#'
#' @export
import_ukaq_measurements <-
  function(code,
           year,
           source = "ukaq",
           data_type = "hourly",
           pollutant = NULL,
           append_meteorology = TRUE,
           append_quality_flag = FALSE,
           append_metadata = FALSE,
           metadata_columns = c("site_type", "latitude", "longitude"),
           pivot = "wide",
           progress = NA,
           ...,
           .class = NULL) {
    # ensure 'source' is correct
    source <- match_source(source, ukaq_network_names)

    # ensure data_type matches
    data_type <-
      rlang::arg_match(data_type,
                       c(
                         "hourly",
                         "daily",
                         "15_min",
                         "8_hour",
                         "24_hour",
                         "daily_max_8"
                       ))

    # ensure pivot arg matches
    pivot <- match_pivot(pivot)

    # get metadata
    meta <- import_ukaq_meta(source = source)

    # order metadata by user-defined order
    meta$source <- factor(meta$source, source)
    meta <- meta[order(meta$source),]

    # filter metadata for codes
    meta_with_code <- meta[tolower(meta$code) %in% tolower(code), ]

    # ensure one line per code
    meta_final <-
      do.call(rbind, lapply(unique(meta_with_code$code), function(x) {
        utils::head(meta_with_code[meta_with_code$code == x,], n = 1L)
      }))

    # flag if there were any ambiguous codes
    if (nrow(meta_final) < nrow(meta_with_code)) {
      id <-
        sapply(split(meta_with_code, ~ code), function(x) {
          nrow(x)
        })
      id <- names(id[id > 1])

      warning(
        paste0(
          "Ambiguous Codes Detected: ",
          id,
          ".\nImporting sites using following order of preference: ",
          paste(source, collapse = ", ")
        )
      )
    }

    # get a grid of metadata for each year
    grid <-
      do.call(rbind, lapply(year, function(x) {
        df <- meta_final
        df$year <- x
        df$source <- as.character(df$source)
        df <- df[c("source", "code", "lmam_code", "year")]
        return(df)
      }))

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

    # import data
    data <-
      lapply(1:nrow(grid), function(x) {
        m <- grid[x,]
        url <-
          measURL(
            source = m$source,
            code = m$code,
            year = m$year,
            dir = m$lmam_code
          )
        if (progress) {
          utils::setTxtProgressBar(pb, value = x)
        }
        tryCatch(
          suppressWarnings(loadRData(url, object = data_type) |> transform(source = m$source)),
          error = function(e) {
            NULL
          }
        )
      })

    # bind data
    data <- rbindNames(data)

    if (is.null(data)) {
      stop("No data has been returned.")
    }

    # move 'source' to front
    data <- data[c("source", setdiff(names(data), "source"))]

    # deal with names
    names(data) <- tolower(names(data))
    names(data)[names(data) == "noxasno2"] <- "nox"

    # filter pollutants
    if (!is.null(pollutant)) {
      id_cols <- c("source", "date", "code", "site", tolower(pollutant))
      for (i in c("ws", "wd", "temp")) {
        if (i %in% names(data)) {
          id_cols <- append(id_cols, i)
        }
      }

      data <- data[id_cols]
    }

    # remove meteorology if not requested
    if (!append_meteorology) {
      data$ws <- data$wd <- data$temp <- NULL
    }

    if (pivot == "long") {
      data <-
        stats::reshape(
        data,
        varying = setdiff(names(data), c("source", "date", "code", "site", "ws", "wd", "temp")),
        v.names = "value",
        timevar = "pollutant",
        times = setdiff(names(data), c("source", "date", "code", "site", "ws", "wd", "temp")),
        direction = "long"
      )

      data$id <- NULL

      rownames(data) <- NULL

      if (append_quality_flag) {
        meta_by_pollutant <- import_ukaq_meta(source = source, by_pollutant = TRUE)
        meta_by_pollutant <- meta_by_pollutant[c("source", "code", "pollutant", "ratified_to")]
        meta_by_pollutant <- meta_by_pollutant[!is.na(meta_by_pollutant$ratified_to), ]
        meta_by_pollutant$ratified_to <- as.POSIXct(meta_by_pollutant$ratified_to, tz = "GMT")

        data <- merge(data, meta_by_pollutant, by = c("source", "code", "pollutant"))

        data$qc <- data$date <= data$ratified_to

        data$ratified_to <- NULL
      }
    }

    if (pivot == "wide" && append_quality_flag) {
      meta_by_pollutant <- import_ukaq_meta(source = source, by_pollutant = TRUE)
      meta_by_pollutant <- meta_by_pollutant[c("source", "code", "pollutant", "ratified_to")]
      meta_by_pollutant <- meta_by_pollutant[!is.na(meta_by_pollutant$ratified_to), ]
      meta_by_pollutant <- meta_by_pollutant[meta_by_pollutant$pollutant %in% names(data),]

      meta_by_pollutant$source_code <- paste(meta_by_pollutant$source, meta_by_pollutant$code)
      data$source_code <- paste(data$source, data$code)
      meta_by_pollutant <- meta_by_pollutant[meta_by_pollutant$source_code %in% data$source_code, ]

      newdat <- list()
      for (i in unique(data$source_code)) {
        meta_i <-
          meta_by_pollutant[meta_by_pollutant$source_code == i, ]
        meta_i$ratified_to <- as.POSIXct(meta_i$ratified_to, tz = "GMT")
        data_i <- data[data$source_code == i,]

        for (j in unique(meta_i$pollutant)) {
          meta_j <- meta_i[meta_i$pollutant == j, ]
          data_i[[paste0(j, "_qc")]] <- data_i$date <= meta_j$ratified_to
        }

        newdat <- append(newdat, list(data_i))
      }

      data <- rbindNames(newdat)

      data$source_code <- NULL
    }

    # append metadata, if requested
    if (append_metadata) {
      data <-
        append_metadata_cols(data, source = source, metadata_columns = metadata_columns)
    }

    return(tbl(data, .class = .class))
  }

#' Construct measurement URL
#' @noRd
measURL <- function(source, code, year, dir = "") {
  url <-
    switch(
      source,
      aurn = "https://uk-air.defra.gov.uk/openair/R_data/CODE_YEAR.RData",
      saqn = "https://www.scottishairquality.scot/openair/R_data/CODE_YEAR.RData",
      niaqn = "https://www.airqualityni.co.uk/openair/R_data/CODE_YEAR.RData",
      waqn = "https://airquality.gov.wales/sites/default/files/openair/R_data/CODE_YEAR.RData",
      aqe = "https://airqualityengland.co.uk/assets/openair/R_data/CODE_YEAR.RData",
      lmam = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/DIR/CODE_YEAR.RData"
    )

  url <- sub("CODE", toupper(code), url)
  url <- sub("YEAR", as.character(year), url)
  url <- sub("DIR", dir, url)

  return(url)
}
