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
#'   codes, available through [import_ukaq_meta()]. Each code can be paired
#'   with a specific network using `source`.
#'
#' @param year *A year, or range of years, from which to import data.*
#'
#'    **required**
#'
#'   `year` expects a vector of integer values. It can be a single year
#'   (`2020`), a selection of specific years (c(`2020, 2021, 2022`)), or a range
#'   of years (`2020:2025`).
#'
#' @param source *The UK monitoring network(s) from which to import data.*
#'
#'   *default:* `"ukaq"`
#'
#'   Each value should be one of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`,
#'   `"niaqn"`, `"lmam"`, or `"ukaq"`. `source` can be provided in two ways:
#'
#'   - **A single value**, which applies to every `code`. The default,
#'   `"ukaq"`, will look for each site in all of the available networks; any
#'   other value will *only* import data from that network.
#'
#'   - **A vector the same length as `code`**, pairing each site code with a
#'   network. For example, `code = c("MY1", "KC1", "ABD")` and `source =
#'   c("aurn", "aurn", "saqn")`. `"ukaq"` can be used for any code whose
#'   network should be worked out automatically.
#'
#'   When a site is found in multiple networks under `"ukaq"`, the first of
#'   `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, `"niaqn"` and `"lmam"` is used and a
#'   warning is given.
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
#'   meteorological data obtained from the WRF model. Setting
#'   `append_meteorology = FALSE` will remove this, which may be useful if
#'   joining measured meteorological data sourced from elsewhere.
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
#'   *default:* `rlang::is_interactive()`
#'
#'   When `TRUE`, this function will print a progress bar to track individual
#'   files being imported. If `FALSE` this is suppressed. By default, a progress
#'   bar is shown in interactive sessions only.
#'
#' @return a `data.frame`
#'
#' @author Jack Davison, David Carslaw
#'
#' @references With thanks to Trevor Davies and [WSP
#'   UK&I](https://www.wsp.com/en-gb) for preparing and hosting the data
#'
#' @export
import_ukaq_measurements <-
  function(
    code,
    year,
    source = "ukaq",
    data_type = "hourly",
    pollutant = NULL,
    append_meteorology = TRUE,
    append_quality_flag = FALSE,
    append_metadata = FALSE,
    metadata_columns = c("site_type", "latitude", "longitude"),
    pivot = "wide",
    progress = rlang::is_interactive(),
    ...,
    .class = NULL
  ) {
    rlang::check_dots_empty()

    # pair each code with a network (or "ukaq", to be worked out)
    pairs <- pair_code_source(code, source, ukaq_network_names)

    # ensure data_type matches
    data_type <-
      rlang::arg_match(
        data_type,
        c(
          "hourly",
          "daily",
          "15_min",
          "8_hour",
          "24_hour",
          "daily_max_8"
        )
      )

    # ensure pivot arg matches
    pivot <- match_pivot(pivot)

    # get metadata for the networks needed to resolve the codes
    meta <- import_ukaq_meta(
      source = pair_networks(pairs, ukaq_network_names),
      .class = "df"
    )

    # resolve each code to a single network
    resolved <- resolve_code_source(pairs, meta, ukaq_network_names)
    meta_final <- meta[
      match(
        tolower(paste(resolved$code, resolved$source, sep = "\r")),
        tolower(paste(meta$code, meta$source, sep = "\r"))
      ),
    ]

    # only these networks are needed for later metadata (e.g., QC flags)
    source <- unique(meta_final$source)

    # get a grid of metadata for each year
    grid <-
      do.call(
        rbind,
        lapply(year, function(x) {
          df <- meta_final
          df$year <- x
          df$source <- as.character(df$source)
          df <- df[c("source", "code", "lmam_code", "year")]
          return(df)
        })
      )

    if (progress) {
      pb <- pb_init(name = "Importing Measurements", x = nrow(grid))
      on.exit(pb_close(pb))
    }

    # import data
    data <-
      lapply(1:nrow(grid), function(x) {
        m <- grid[x, ]
        url <-
          measURL(
            source = m$source,
            code = m$code,
            year = m$year,
            dir = m$lmam_code
          )
        if (progress) {
          pb_increment(pb, x = x)
        }
        tryCatch(
          suppressWarnings(
            transform(loadRData(url, object = data_type), source = m$source)
          ),
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
      pollutant <- tolower(pollutant)
      missing_polls <- setdiff(pollutant, names(data))
      if (length(missing_polls) == length(pollutant)) {
        stop(
          "None of the requested pollutants were found in the imported data: ",
          paste(pollutant, collapse = ", "),
          ". Available pollutants are: ",
          paste(
            setdiff(
              names(data),
              c("source", "date", "code", "site", "ws", "wd", "temp")
            ),
            collapse = ", "
          ),
          "."
        )
      }
      if (length(missing_polls) > 0L) {
        warning(
          "The following pollutants were not found in the imported data and ",
          "have been skipped: ",
          paste(missing_polls, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
      id_cols <- c(
        "source",
        "date",
        "code",
        "site",
        intersect(pollutant, names(data))
      )
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
          varying = setdiff(
            names(data),
            c("source", "date", "code", "site", "ws", "wd", "temp")
          ),
          v.names = "value",
          timevar = "pollutant",
          times = setdiff(
            names(data),
            c("source", "date", "code", "site", "ws", "wd", "temp")
          ),
          direction = "long"
        )

      data$id <- NULL

      rownames(data) <- NULL
    }

    # append ratification flags, if requested
    if (append_quality_flag) {
      cutoffs <- ratification_cutoffs(source)
      cutoff_keys <- tolower(paste(cutoffs$source, cutoffs$code, sep = "\r"))
      data_keys <- tolower(paste(data$source, data$code, sep = "\r"))

      if (pivot == "long") {
        # left join, so pollutants without a ratification date are kept (with
        # a missing flag) rather than dropped
        cutoff <- cutoffs$cutoff[match(
          paste(data_keys, data$pollutant, sep = "\r"),
          paste(cutoff_keys, cutoffs$pollutant, sep = "\r")
        )]
        data$qc <- data$date < cutoff
      }

      if (pivot == "wide") {
        for (j in intersect(unique(cutoffs$pollutant), names(data))) {
          id <- cutoffs$pollutant == j
          cutoff <- cutoffs$cutoff[id][match(data_keys, cutoff_keys[id])]
          if (all(is.na(cutoff))) {
            next
          }
          data[[paste0(j, "_qc")]] <- data$date < cutoff
        }
      }
    }

    # append metadata, if requested
    if (append_metadata) {
      data <-
        append_metadata_cols(
          data,
          source = source,
          metadata_columns = metadata_columns
        )
    }

    return(tbl_out(data, .class = .class))
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

#' Get the date-time before which each site-pollutant's data is ratified
#' @noRd
ratification_cutoffs <- function(source) {
  meta <- import_ukaq_meta(source = source, by_pollutant = TRUE, .class = "df")
  meta <- meta[
    !is.na(meta$ratified_to),
    c("source", "code", "pollutant", "ratified_to")
  ]

  # if a site lists a pollutant more than once, conservatively use the
  # earliest ratification date
  meta <- meta[order(meta$ratified_to), ]
  meta <- meta[!duplicated(meta[c("source", "code", "pollutant")]), ]

  # data are ratified up to the *end* of the ratified_to date
  meta$cutoff <- as.POSIXct(format(meta$ratified_to + 1L), tz = "GMT")
  meta$ratified_to <- NULL

  meta
}
