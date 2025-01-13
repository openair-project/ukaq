#' Import UK Daily Air Quality Index Data
#'
#' Imports [UK Daily Air Quality Index
#' (DAQI)](https://uk-air.defra.gov.uk/air-pollution/daqi) data for selected
#' pollutants, years, and sites. Returned information includes the relevant
#' daily statistic (e.g., daily average for PM10), the index (1:10) and the band
#' (Low, Moderate, High, or Very High).
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
#'   *default:* `"aurn"`
#'
#'   The default, `"aurn"`, will import data from the AURN. Any combination of
#'   `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, and `"niaqn"` will
#'   *only* import data from those specific monitoring networks.
#'   Alternatively, `"ukaq"` will import data from all five. Note that a
#'   mismatch between `code` and `source` may result in no data being imported.
#'
#' @param pollutant *One or more DAQI pollutants for which to import data.*
#'
#'  *default:* `r daqi_pollutant_names`
#'
#'   By default, all `r length(daqi_pollutant_names)` DAQI pollutants are
#'   imported by [import_ukaq_daqi()]. `pollutant` allows any specific
#'   combination of the DAQI pollutants to be returned instead.
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
#'  *default:* `"long"`
#'
#'   There are two main ways to store air quality data; "long" (with 'pollutant'
#'   and 'value' columns) or "wide" (with each pollutant value being stored in
#'   its own column). `pivot` allows users to define which format they would
#'   prefer their data in.
#'
#' @param ... *Not used.*
#'
#' @param .return *Signifier for the dataframe class.*
#'
#'    *default:* `NULL`
#'
#'   [ukaq][ukaq-package] functions, by default, will return `tbl_df`s if the
#'   [tibble][tibble::tibble-package] package is installed, but will otherwise
#'   return `data.frame`s. `.return` can override this behaviour, and takes
#'   either `"tbl"` or `"df"`, which sets the return class to be `tbl_df` or
#'   `data.frame` respectively.
#'
#' @return a `data.frame`
#'
#' @author Jack Davison, David Carslaw
#'
#' @references With thanks to Trevor Davies and [Ricardo
#'   Plc](https://www.ricardo.com/en) for preparing and hosting the data
#'
#' @seealso <https://uk-air.defra.gov.uk/air-pollution/daqi> for more
#'   information about the DAQI.
#'
#' @export
import_ukaq_daqi <-
  function(code,
           year,
           source = "aurn",
           pollutant = c("no2", "o3", "pm10", "pm2.5", "so2"),
           append_metadata = FALSE,
           metadata_columns = c("site_type", "latitude", "longitude"),
           pivot = "long",
           ...,
           .return = NULL) {
    pollutant <-
      rlang::arg_match(pollutant, daqi_pollutant_names, multiple = TRUE)
    pivot <-
      rlang::arg_match(pivot, c("wide", "long"))
    metadata_columns <-
      rlang::arg_match(metadata_columns, metadata_column_names, multiple = TRUE)
    source <-
      match_source(source = source, network_names = ukaq_network_names_nolocal)

    # import data
    daqi <- importDAQI(year = year, source = source)

    # format
    daqi <-
      formatDAQI(
        daqi = daqi,
        pollutant = pollutant,
        code = code,
        pivot = pivot,
        append_metadata = append_metadata,
        metadata_columns = metadata_columns
      )

    return(tbl(daqi, .return))
  }

#' Import DAQI data
#' @noRd
importDAQI <- function(year, source){
  grid <-
    expand.grid(year = year,
                source = source,
                stringsAsFactors = FALSE)

  # load daqi data
  daqi <-
    lapply(1:nrow(grid), function(x) {
      df <- tryCatch(
        suppressWarnings(loadRDS(daqi_url(
          grid$source[x], grid$year[x]
        ))),
        error = function(e)
          NULL
      )
      if (!is.null(df)) {
        df$source <- grid$source[x]
        df <- df[, c("source", names(df)[names(df) != "source"])]
      }
      df
    })

  daqi <- do.call(rbind, daqi)

  # abort if there's no data
  if (is.null(daqi)) {
    stop("No data has been returned.")
  }

  return(daqi)
}

#' Format DAQI data
#' @noRd
formatDAQI <- function(daqi, pollutant, code, pivot, append_metadata, metadata_columns) {
  # deal with data types & names
  names(daqi) <- tolower(names(daqi))
  daqi$date <- as.Date(as.character(daqi$date), tz = "GMT")

  # remove factors
  daqi <- factor_to_char(daqi)

  # create poll band column
  daqi <- append_daqi_bands(daqi)

  # filter for site/pollutant
  daqi <- daqi[tolower(daqi$pollutant) %in% tolower(pollutant),]
  daqi <- daqi[tolower(daqi$code) %in% tolower(code),]

  # pivot, if required
  if (pivot == "wide") {
    daqi <- pivotDAQI(daqi)
  }

  # append metadata, if requested
  if (append_metadata) {
    daqi <-
      append_metadata_cols(daqi, source = source, metadata_columns = metadata_columns)
  }

  # order by site/date
  daqi <- daqi[order(daqi$site, daqi$date),]

  # return
  return(daqi)
}

#' Reshape DAQI
#' @noRd
pivotDAQI <- function(daqi) {
  names(daqi)[names(daqi) == "concentration"] <- "value"
  names(daqi)[names(daqi) == "poll_index"] <- "index"
  names(daqi)[names(daqi) == "poll_band"] <- "band"

  daqi <-
    stats::reshape(
      as.data.frame(daqi)[c("date",
                            "code",
                            "site",
                            "pollutant",
                            "value",
                            "index",
                            "band")],
      timevar = "pollutant",
      idvar = c("date", "code", "site"),
      direction = "wide",
      v.names = c("value", "index", "band"),
      sep = "_"
    )

  names(daqi) <- gsub("value_", "", names(daqi))

  grid <-
    expand.grid(a = c("index", "band"), b = daqi_pollutant_names)
  old <- paste(grid$a, grid$b, sep = "_")
  new <- paste(grid$b, grid$a, sep = "_")

  for (i in seq_along(old)) {
    names(daqi)[names(daqi) == old[i]] <- new[i]
  }

  return(daqi)
}

#' Format DAQI URL using source/year
#' @noRd
daqi_url <- function(source, year) {
  url <- switch(
    source,
    "aurn" = "https://uk-air.defra.gov.uk/openair/R_data/annual_DAQI_AURN_YEAR.rds",
    "aqe" = "https://airqualityengland.co.uk/assets/openair/R_data/annual_DAQI_AQE_YEAR.rds",
    "saqn" = "https://www.scottishairquality.scot/openair/R_data/annual_DAQI_SCOT_YEAR.rds",
    "waqn" = "https://airquality.gov.wales/sites/default/files/openair/R_data/annual_DAQI_WAQ_YEAR.rds",
    "niaqn" = "https://www.airqualityni.co.uk/openair/R_data/annual_DAQI_NI_YEAR.rds"
  )

  url <- gsub("YEAR", year, url)

  return(url)
}

#' Add DAQI bands column before "measurement_period" col
#' @noRd
append_daqi_bands <- function(data) {
  data$poll_band <- ifelse(
    data$poll_index %in% 1:3,
    "Low",
    ifelse(
      data$poll_index %in% 4:6,
      "Moderate",
      ifelse(data$poll_index %in% 7:9, "High", "Very High")
    )
  )
  data$poll_band <-
    factor(data$poll_band, c("Low", "Moderate", "High", "Very High"))

  data <-
    data[, c(names(data)[names(data) != "measurement_period"], "measurement_period")]

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
